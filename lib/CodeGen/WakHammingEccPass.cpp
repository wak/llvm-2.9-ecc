// ハミング符号をIRで実装しようとした名残り
// 今は，汎用実装のWakInsertEccに移行済み．遅いけれど．

#define DEBUG_TYPE "wak-duplicate-insn-test"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Attributes.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Intrinsics.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/BasicBlock.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ConstantFolder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
  class WakHammingEcc : public FunctionPass {
  public:
    static char ID;
    Function *FUNC;

    WakHammingEcc() : FunctionPass(ID) {
      initializeWakHammingEccPass(*PassRegistry::getPassRegistry());
    }

  private:
    Value *getEncodedValuePtr(Value *value,
                              IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > &B) {
      Value *HAMMING_CODE = FUNC->getParent()->getGlobalVariable("HAMMING_CODE");
      if (HAMMING_CODE == NULL)
        report_fatal_error("The symbol HAMMING_CODE not found (ecc_check_failed)");
      Value *idxprom  = B.CreateZExt(value, B.getInt64Ty(), "idxprom");
      Value *indexes[] = {B.getInt32(0), idxprom};
      Value *arrayidx = B.CreateInBoundsGEP(HAMMING_CODE, indexes, indexes + 2, "arrayidx");

      return arrayidx;
    }

    void insertHammingToStoreSub(BasicBlock::iterator &insn_iter, unsigned long copy_base_addr) {
      StoreInst *insn = dyn_cast<StoreInst>(insn_iter);

      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(insn);

      Value *arrayidx = getEncodedValuePtr(insn->getValueOperand(), B);
      Value *encoded = B.CreateLoad(arrayidx);
      Value *syndrome_32 = B.CreateAnd(encoded, 0xf);  // &  0xff
      Value *syndrome_8  = B.CreateTrunc(syndrome_32, B.getInt8Ty());

      Value *cast_inst = B.CreatePtrToInt(insn->getPointerOperand(), B.getInt64Ty(), "dup-start");
      Value *r1 = B.CreateAnd(cast_inst, B.getInt64(0x1fffff));
      Value *r2 = B.CreateOr(r1, B.getInt64(copy_base_addr));
      Value *r3 = B.CreateIntToPtr(r2, insn->getPointerOperand()->getType());
      B.CreateStore(syndrome_8, r3, "A Duplication end");
    }

    // 読み込み時用
    void insertHammingToLoadSub(BasicBlock::iterator &insn_iter, unsigned long copy_base_addr) {
      errs() << "pos 1\n";
      LoadInst *load_insn = dyn_cast<LoadInst>(insn_iter);
      load_insn->isecc_inserted = true;

      assert(load_insn);

      errs() << "pos 2\n";
      // ContBB: チェック後に実行する通常のコードをBBから分離する
      //insn_iter++;
      BasicBlock *BB = load_insn->getParent();
      //BB->dump();

      // Loadより前と，Loadとそれ以降に分割
      BasicBlock *RetryBB = BB->splitBasicBlock(insn_iter, "retry");
      // BB->dump();
      // RetryBB->dump();

      // Loadと, それより後の二つに分割
      // @todo Load後が無いと問題
      BasicBlock::iterator iter = RetryBB->begin();
      iter++;
      BasicBlock *ContBB = RetryBB->splitBasicBlock(iter, "ContBB");
      // BB->dump();

      errs() << "pos 3\n";
      // 変数定義
      Value *value = load_insn;

      // BBとContBBに分離できたので，LOAD命令直後（チェックするためには値をとって
      // こないといけないため）から，チェック用の命令を追加していく．

      // IRBuilderのインスタンスBを用いて追加していける．なぜ，ここで
      // getTerminator()でよいかと言うと，LOAD命令のあとに，ContBBへのジャンプが
      // 生成されているため．もちろん，このジャンプは，後で削除する必要がある．
      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(RetryBB->getTerminator());

      errs() << "pos 4\n";
      // ECC領域に保存したシンドロームを取得する => copied_syndrome
      // %0 = ptrtoint i32* %value to i64
      // %and = and i64 %0, 2097151
      // %or = or i64 68719476736, %and
      // %1 = inttoptr i64 %or to i32*
      // %tmp = load i32* %1
      Value *cast_inst = B.CreatePtrToInt(load_insn->getPointerOperand(), B.getInt64Ty(), "get_copied_syndrome_start");
      Value *sy_r1 = B.CreateAnd(cast_inst, B.getInt64(0x1fffff));
      Value *sy_r2 = B.CreateOr(sy_r1, B.getInt64(copy_base_addr));
      Value *sy_r3 = B.CreateIntToPtr(sy_r2, load_insn->getPointerOperand()->getType());
      Value *copied_syndrome = B.CreateLoad(sy_r3, "copied_syndrome");

      errs() << "pos 5\n";
      // 誤り訂正関数を呼んで，正しい値をvalueにstoreするブロック
      BasicBlock *CorrectBB = BasicBlock::Create(FUNC->getContext(), "HammingCorrectBB", FUNC);
      {
        IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B2(CorrectBB);
        Function *func = FUNC->getParent()->getFunction("_hamming_ecc_correct");
        if (func == NULL)
          report_fatal_error("Correct function not found (_hamming_ecc_correct)");
        Value *corrected = B2.CreateCall2(func, value, copied_syndrome, "corrected");
        B2.CreateStore(corrected, load_insn->getPointerOperand());
        B2.CreateBr(RetryBB);
      }

      errs() << "pos 6\n";
      // チェックする値とそのシンドロームをくっつけたものをつくる => encoded
      // %tmp1 = load i32* %value, align 4
      // %shl  = shl i32 %tmp1, 4
      // %tmp2 = load i32* %syndrome, align 4
      // %or3  = or i32 %shl, %tmp2
      Value *en_32_value   = B.CreateZExt(value, B.getInt32Ty(), "en_32_value");
      Value *en_32_encoded = B.CreateZExt(copied_syndrome, B.getInt32Ty(), "en_32_copied_encoded");
      Value *en_shl  = B.CreateShl(en_32_value, 4);   // = (value << 4)
      Value *encoded = B.CreateOr(en_shl, en_32_encoded); // = (en_shl | copied_syndrome)

      errs() << "pos 7\n";
      // チェックする
      // %tmp4     = load i32* %value, align 4
      // %and5     = and i32 %tmp4, 255
      // %idxprom  = zext i32 %and5 to i64
      // %arrayidx = getelementptr inbounds [256 x i32]* @HAMMING_CODE, i32 0, i64 %idxprom
      // %tmp6 = load i32* %arrayidx
      // %tmp7 = load i32* %encoded, align 4
      // %cmp = icmp eq i32 %tmp6, %tmp7
      // br i1 %cmp, label %if.then, label %if.else
      Value *HAMMING_CODE = FUNC->getParent()->getGlobalVariable("HAMMING_CODE");
      if (HAMMING_CODE == NULL)
        report_fatal_error("The symbol HAMMING_CODE not found (ecc_check_failed)");
      Value *ch_and5     = B.CreateAnd(value, 0xff, "ch_and5");
      Value *ch_idxprom  = B.CreateZExt(ch_and5, B.getInt64Ty(), "ch_idxprom");
      // Value *ch_array    = B.CreateLoad(HAMMING_CODE);
      Value *indexes[] = {B.getInt32(0), ch_idxprom};
      Value *ch_arrayidx = B.CreateInBoundsGEP(HAMMING_CODE, indexes, indexes + 2, "ch_arrayidx");
      Value *right_encoded = B.CreateLoad(ch_arrayidx, "right_encoded");
      Value *ch_cmp = B.CreateICmpEQ(right_encoded, encoded, "ch_cmp");
      B.CreateCondBr(ch_cmp, ContBB, CorrectBB);

      errs() << "pos 8\n";
      // splitBasicBlockすると，自動で分割したBBへのジャンプがあるため，削除
      if (RetryBB->getTerminator() != NULL)
        RetryBB->getTerminator()->eraseFromParent();

      errs() << "pos 9\n";
      // BBの位置を調整する
      // BB-> RetryBB -> CorrectBB -> ContBB
      RetryBB->moveAfter(BB);
      CorrectBB->moveAfter(RetryBB); // CorrectBBをBBの後ろに
      ContBB->moveAfter(CorrectBB); // ContBBをCorrectBBの後ろに

      errs() << "==============================\n";
      errs() << "[BB]\n";
      BB->dump();
      errs() << "[RetryBB]\n";
      RetryBB->dump();
      errs() << "[CorrectBB]\n";
      CorrectBB->dump();
      errs() << "[ContBB]\n";
      ContBB->dump();
      errs() << "==============================\n";
      return;
    }


    Value *checkPointerHasEccValue(StoreInst *StoreInsn) {
      LoadInst *load_i = dyn_cast<LoadInst>(StoreInsn->getPointerOperand());
      // define void @assign_to_pointer_var() nounwind {
      // entry:
      //   %ecc_ptr = [ECC-0x4b59288]alloca i32*, align 8
      //   store i32* @ecc_g_var, i32** [ECC-0x4b59288]%ecc_ptr, align 8
      //   %tmp = load i32** [ECC-0x4b59288]%ecc_ptr, align 8
      //   store i32 300, i32* %tmp
      //   ret void
      // }

      // int **p _ecc_; みたいなのは，pは保護しないが，*pは保護する
      // 従って，一段Load命令が挟まっているはず．
      if (!load_i)
        return NULL;
      if (load_i->getPointerOperand()->isecc)
        return load_i->getPointerOperand();

      return NULL;
    }

    bool insertHammingToStore(Function &F) {
      bool changed = false;

      errs() << "- [Hamming to store instruction] -\n";

      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // for each block
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // for each instruction
          const Value *ecc = NULL;

          errs() << "inst: " << I->getOpcodeName() << "\n";

          if (StoreInst *store_insn = dyn_cast<StoreInst>(I)) {
            Value *pointerOperand = store_insn->getPointerOperand();

            // ポインタ
            if ((ecc = checkPointerHasEccValue(store_insn)) != NULL) {
              errs() << "[pointer] ";
              goto found;
            }

            // 変数アクセスでも，（ここのコード位置で見つかる）ポインタは違う．
            if (pointerOperand->isecc &&
                pointerOperand->getType()->getNumContainedTypes() > 0 &&
                pointerOperand->getType()->getContainedType(0)->isPointerTy()) {
              // do nothing: int *ptr _ecc_ = &var;
              goto skip;
            }

            // 変数アクセス
            if (pointerOperand->isecc) {
              errs() << "[var] ";
              ecc = pointerOperand;
              goto found;
            }

            // 構造体メンバその1
            if (GetElementPtrInst *get_insn =
                dyn_cast<GetElementPtrInst>(pointerOperand)) {
              errs() << "[struct member] ";
              if (get_insn->getPointerOperand()->isecc) {
                  ecc = get_insn->getPointerOperand();
                  goto found;
              }
            }

            // 構造体メンバその2 （アドレスが固定の場合: 例.グローバル変数）
            if (ConstantExpr *expr = dyn_cast<ConstantExpr>(pointerOperand)) {
              errs() << "[struct member] ";
              if (expr->getOpcode() == Instruction::GetElementPtr &&
                  expr->getNumOperands() > 0 &&
                  expr->getOperand(0)->isecc) {
                ecc = expr->getOperand(0);
                goto found;
              } else {
                errs() << "OP: " << expr->getOpcode() << "  INST: "
                       << Instruction::GetElementPtr << "\n";
              }
            }
          }

          found:
          skip:
          if (ecc) {
            errs() << "  Store instruction with ecc lvalue found [" << ecc << "].\n";
            insertHammingToStoreSub(I, 0x1000000000);
            changed = true;
          }
        }
      }

      return changed;
    }

    bool insertHammingToLoad(Function &F) {
      bool changed = false;

      errs() << "- [Hamming to load instruction] -\n";
      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ) {
        // for each block
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // for each instruction
          // I->dump();

          if (LoadInst *insn = dyn_cast<LoadInst>(I)) {
            if (insn->isecc_inserted)
              continue;
            if (insn->getPointerOperand()->isecc) {
              errs() << "  Load instruction with ecc found.\n";
              insertHammingToLoadSub(I, 0x1000000000);
              changed = true;
              goto retry;
            }
          }
        }
        next:
        ++BB;
        continue;
        retry:
        continue;
      }
      return changed;
    }

    virtual bool runOnFunction(Function &F) {
      bool changed = false;
      FUNC = &F;

      errs() << "--- [Wak Hamming Code] ---\n";
      errs() << "Function: ";
      errs().write_escaped(F.getName());
      errs() << "\n\n";
      insertHammingToStore(F);
      insertHammingToLoad(F);
      errs() << "-------------------- [wak] --------------------\n\n";
      return changed;
    }

    virtual const char *getPassName() const {
      return "WakHammingEccPass";
    }
  };
}

char WakHammingEcc::ID = 0;
INITIALIZE_PASS(WakHammingEcc, "wak-hamming-ecc",
                "Wak insert hamming-code ecc", false, false)

FunctionPass *llvm::createWakHammingEccPass(const TargetLowering *tli) {
  return new WakHammingEcc();
}
