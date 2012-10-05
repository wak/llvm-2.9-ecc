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

  // not work
  template <bool preserveNames = true>
  class AfterInserter {
  protected:
    void InsertHelper(Instruction *I, const Twine &Name,
                      BasicBlock *BB, BasicBlock::iterator InsertPt) const {
      if (BB) BB->getInstList().insertAfter(InsertPt, I);
      if (preserveNames)
        I->setName(Name);
    }
  };

  class WakDuplicateInsnTest : public FunctionPass {
  public:
    static char ID;
    Function *FUNC;

    WakDuplicateInsnTest() : FunctionPass(ID) {
      initializeWakDuplicateInsnTestPass(*PassRegistry::getPassRegistry());
    }

  private:

    unsigned int getPointerSizeInBits() const {
      switch (FUNC->getParent()->getPointerSize()) {
      case Module::Pointer32:
        return 32;
      case Module::Pointer64:
        return 64;
      case Module::AnyPointerSize:
        report_fatal_error("getPointerSizeInBits(): unknown pointer size\n");
      default:
        assert(0 && "unknown pointer type");
      }
      assert(0 && "No here");
    }

    void insertStoreTest(BasicBlock::iterator &insn_iter) {
      StoreInst *insn = dyn_cast<StoreInst>(insn_iter);

      // @todo: ここでiteratorを++しておくので，呼び出し元もちゃんといけるか？
      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(insn->getParent(), ++insn_iter);

      // 更新関数を取得
      Function *update_bits_function = FUNC->getParent()->getFunction("ecc_update_ecc_data_bits");
      if (update_bits_function == NULL)
        report_fatal_error("ECC update function not found (ecc_update_ecc_data_int)");

      // 更新される（された）サイズを取得
      unsigned size = 0;
      const Type *destTy = insn->getPointerOperand()->getType();
      if (const PointerType *ptrTy = dyn_cast<PointerType>(destTy)) {
        const Type *containedTy = ptrTy->getContainedType(0);
        if (containedTy->isPointerTy())
          // (1) int *p;  => sizeof(*p) = sizeof(int)
          size = getPointerSizeInBits();
        else
          // (2) int **p; => sizeof(*p) = sizeof(int *)
          size = containedTy->getPrimitiveSizeInBits();
      } else {
        // (3) int v; => sizeof(v) = sizeof(int)
        size = destTy->getPrimitiveSizeInBits();
      }

      // 関数呼出し命令挿入
      // ecc_update_ecc_data_bits((void *) pointerOperand, target_size_in_bits);
      B.CreateCall2(update_bits_function,
                    B.CreatePointerCast(insn->getPointerOperand(), B.getInt8PtrTy()),
                    B.getInt64(size));
    }


    void insertDupStoreInsn(BasicBlock::iterator &insn_iter,
                            unsigned long copy_base_addr) {
      /*
       * %1 = and      i64 ptrtoint (i32* @val to i64), 0x1fffff
       * %2 = or       i64 %1, 0x1000000000
       * %3 = inttoptr i64 %2 to i32*
       * store i32 100, i32* %3, align 4
       */

      StoreInst *insn = dyn_cast<StoreInst>(insn_iter);

      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(insn);

      Value *cast_inst =
        B.CreatePtrToInt(insn->getPointerOperand(),
                         B.getInt64Ty(), "dup-start");
      Value *r1 = B.CreateAnd(cast_inst, B.getInt64(0x1fffff));
      Value *r2 = B.CreateOr(r1, B.getInt64(copy_base_addr));
      Value *r3 = B.CreateIntToPtr(r2, insn->getPointerOperand()->getType());
      B.CreateStore(insn->getValueOperand(), r3, "A Duplication end");
    }

    BasicBlock *createErrorHandlingBB(void) {
      BasicBlock *FailBB =
        BasicBlock::Create(FUNC->getContext(), "EccCheckFailBlk", FUNC);

      Function *errorHandler = FUNC->getParent()->getFunction("ecc_check_failed");
      if (errorHandler == NULL)
        report_fatal_error("Error handler not found (ecc_check_failed)");

      CallInst::Create(errorHandler, "", FailBB);
      new UnreachableInst(FUNC->getContext(), FailBB);
      if (FailBB->getTerminator() == NULL)
        report_fatal_error("wak: unreachableinst is not terminator!");
      return FailBB;
    }

    void insertCheckInsn(BasicBlock::iterator insn_iter,
                         unsigned long copy_base_addr) {
      LoadInst *loadInsn = dyn_cast<LoadInst>(insn_iter);

      // ContBB: LOAD命令の次の命令から始まるブロックを作成
      insn_iter++;
      BasicBlock *BB = loadInsn->getParent();
      Instruction *continuous_instruction = insn_iter;
      BasicBlock *ContBB = BB->splitBasicBlock(continuous_instruction, "ECC_success");

      // まずは，LOAD命令の後にチェック用の命令を追加していく
      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(BB->getTerminator());

      // ECC領域その1と比較
      Value *cast_inst = B.CreatePtrToInt(loadInsn->getPointerOperand(), B.getInt64Ty());
      Value *r2 = B.CreateAnd(cast_inst, B.getInt64(0x1fffff));
      Value *r3 = B.CreateOr(r2, B.getInt64(copy_base_addr));
      Value *r4 = B.CreateIntToPtr(r3, loadInsn->getPointerOperand()->getType());
      Value *r5 = B.CreateLoad(r4);
      Value *r7 = B.CreateICmpEQ(r5, loadInsn);

      B.CreateCondBr(r7, ContBB, createErrorHandlingBB());
      ContBB->moveAfter(BB);

      // splitBasicBlockすると，自動で分割したBBへのジャンプがあるため，削除
      BB->getTerminator()->eraseFromParent();

      if (ContBB->getTerminator() == NULL)
        report_fatal_error("wak: ContDB does not have terminator!");
      if (BB->getTerminator() == NULL)
        report_fatal_error("wak: BB does not have terminator!");
    }

    bool insertInstructionsForRead(Function &F) {
      bool changed = false;

      errs() << "- [DIT: insert check instructions ]\n";
      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // for each block
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // for each instruction
          if (LoadInst *insn = dyn_cast<LoadInst>(I)) {
            if (insn->getPointerOperand()->isecc) {
              errs() << "  Load instruction with ecc found.\n";
              insertCheckInsn(I, 0x1000000000);
              changed = true;
            }
          }
        }
      }

      return changed;
    }

    // 単純に，ポインタがいくつ挟まっているかを数える．
    int countPointerWraps(const Type *type) {
      int count;

      for (count = 0; type->isPointerTy(); count++)
        type = type->getContainedType(0);
      return count;
    }

    Value *checkPointerHasEccValue(Value *pointerOperand) {
      assert(isEccPointerVariable(pointerOperand));

      // ポインタルール
      //   ポインタ変数に_ecc_(level = 0)を指定した場合，
      //   実体からlevel個分のポインタをチェックする
      //
      //   - 例1: int **p __attribute__((ecc(0)));
      //     - p   = ... なし
      //     - *p  = ... なし
      //     - **p = ... 保護
      //   - 例1: int **p __attribute__((ecc(1)));
      //     - p   = ... なし
      //     - *p  = ... 保護
      //     - **p = ... 保護
      //
      // アルゴリズム（代入時）
      //   1. 祖先を辿り，ECC付きかチェックする
      //   2. levelを取得する
      //   3. 代入する型をチェックし，ポインタの階層がlevel内かチェックする
      //
      // アルゴリズム（参照時）
      //   - 代入時アルゴリズムでOK?


      // isEccPointerVariable()でポインタであることはチェックしてある
      size_t nr_dereferences = 0;
      Value *currentValue = pointerOperand;
      while (Instruction *currentInst = dyn_cast<Instruction>(currentValue)) {
        if (currentValue->isecc)
          break;

        if (LoadInst *loadInst = dyn_cast<LoadInst>(currentInst)) {
          // ポインタを剥がす場合は，load命令が入る
          currentValue = loadInst->getPointerOperand();
        } else if (GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(currentInst)) {
          // 配列を参照する場合は，GEP命令が入る
          currentValue = gepInst->getPointerOperand();
        } else {
          break;
        }
        nr_dereferences ++;
      }

      if (currentValue && !currentValue->isecc)
        return NULL;

      int protectRefLevel = currentValue->ecc_reference_level;
      int toWraps         = countPointerWraps(pointerOperand->getType());
      int rootWraps       = countPointerWraps(currentValue->getType());
      assert(toWraps <= rootWraps);

      errs() << "RefLevel = " << protectRefLevel << ", root = " << rootWraps << ", to = " << toWraps;

      if (protectRefLevel < 0)
        return currentValue;

      // StoreのpointerOperandは，変更したいアドレスへのポインタなので，
      // 一つポインタが多く被さっている．そのため，+1が必要．
      if (toWraps <= protectRefLevel + 1)
        return currentValue;

      // if (const PointerType *ty = dyn_cast<PointerType>(pointerOperand->getType())) {
      //   // 今は，level == 0のみなので…
      //   if (!ty->getContainedType(0)->isPointerTy())
      //     return currentValue;
      // }

      return NULL;

      // if (!load_i)
      //   return NULL;
      // if (load_i->getPointerOperand()->isecc)
      //   return load_i->getPointerOperand();

      return NULL;
      // dont use
      /*
      int times = 0;
      for (User::op_iterator iter = Insn->op_begin(), e = Insn->op_end();
           iter != e; ++iter, ++times) {
        if (Instruction *i = dyn_cast<Instruction>(*iter)) {
          errs() << "wak: inst: " << i->getOpcodeName() << "\n";
        } else {
          errs() << "wak: skip\n";
        }
      }
      errs() << "\n";
      return false;
      */
    }

    bool isEccPointerVariable(Value *pointerOperand) {
      const Type *destTy = pointerOperand->getType();

      // storeのオペランドはポインタのはず
      assert(destTy->isPointerTy() && "store's operand must pointer");

      if (dyn_cast<LoadInst>(pointerOperand))
        // 間にload命令が挟まっている場合は，ポインタを剥がして代入するケース
        return true;

      if (const PointerType *ptrTy = dyn_cast<PointerType>(destTy)) {
        if (!ptrTy->getContainedType(0)->isPointerTy()) {
          // loadが挟まっておらず，ポインタを一つ剥がした結果，それがポインタでな
          // ければ，実体．int i; i = value;
          return false;
        }
      }

      return true;
    }

    Value *getEccProtectedOperand(Value *pointerOperand) {
      // ポインタ
      // ポインタECC変数か？
      if (isEccPointerVariable(pointerOperand)) {
          errs() << "[pointer] ";
        if (Value *operand = checkPointerHasEccValue(pointerOperand)) {
          return operand;
        }
        return NULL;
      }

      // @todo: これは何だ？
      // 変数アクセスでも，（ここのコード位置で見つかる）ポインタは違う．
      if (pointerOperand->isecc &&
          pointerOperand->getType()->getNumContainedTypes() > 0 &&
          pointerOperand->getType()->getContainedType(0)->isPointerTy()) {
        // do nothing: int *ptr _ecc_ = &var;

        // これが無いと死ぬ．たぶん，テスト用プログラムのプロトタイプ宣言と型が一
        // 致していないから．ポインタとポインタのポインタ？
        return NULL;
      }

      // 変数アクセス
      if (pointerOperand->isecc) {
        errs() << "[var] ";
        return pointerOperand;
      }

      // 構造体メンバその1
      if (GetElementPtrInst *gep_inst = dyn_cast<GetElementPtrInst>(pointerOperand)) {
        errs() << "[struct member] ";
        if (gep_inst->getPointerOperand()->isecc) {
          return gep_inst->getPointerOperand();
        }
      }

      // 構造体メンバその2 （アドレスが固定の場合: 例.グローバル変数）
      if (ConstantExpr *expr = dyn_cast<ConstantExpr>(pointerOperand)) {
        errs() << "[struct member] ";
        if (expr->getOpcode() == Instruction::GetElementPtr &&
            expr->getNumOperands() > 0 &&
            expr->getOperand(0)->isecc) {
          return expr->getOperand(0);
        } else {
          errs() << "OP: " << expr->getOpcode() << "  INST: " << Instruction::GetElementPtr << "\n";
        }
      }

      return NULL;
    }

    bool insertInstructionsForWrite(Function &F) {
      bool changed = false;

      errs() << "- [DIT: insert duplication instructions ]\n";

      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // 各ブロックを処理する

        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // 各命令を処理する
          errs() << "\n";
          errs() << "inst: " << I->getOpcodeName() << " ";

          StoreInst *store_insn = dyn_cast<StoreInst>(I);
          if (store_insn == NULL)
            continue;

          Value *ecc = getEccProtectedOperand(store_insn->getPointerOperand());
          if (ecc) {
            errs() << "\n  Store instruction with ecc lvalue found [" << ecc << "].\n";
            insertStoreTest(I);
            // insertDupStoreInsn(I, 0x1000000000);
            // insertDupStoreInsn(I, 0x2000000000);
            changed = true;
          }
        }
      }
      errs() << "\n";

      return changed;
    }


    virtual bool runOnFunction(Function &F) {
      bool changed = false;
      FUNC = &F;

      errs() << "--- [Wak insert duplicate instuction test] ---\n";
      errs() << "Function: ";
      errs().write_escaped(F.getName());
      errs() << "\n\n";

      if (insertInstructionsForWrite(F)) // 代入時に複製を作る
        changed = true;
      //if (insertInstructionsForRead(F)) // 参照時に複製を作る
      //changed = true;

      errs() << "-------------------- [wak] --------------------\n\n";
      return changed;
    }

    virtual const char *getPassName() const {
      return "WakDuplicateInsnTestPass";
    }
  };
}

char WakDuplicateInsnTest::ID = 0;
INITIALIZE_PASS(WakDuplicateInsnTest, "wak-duplicate-insn-test",
                "Wak insert duplicate instruction test", false, false)

FunctionPass *llvm::createWakDuplicateInsnTestPass(const TargetLowering *tli) {
  return new WakDuplicateInsnTest();
}
