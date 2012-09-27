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

    bool insertCheckInstructions(Function &F) {
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

    bool insertDuplicationInstructions(Function &F) {
      bool changed = false;

      errs() << "- [DIT: insert duplication instructions ]\n";

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
            insertDupStoreInsn(I, 0x1000000000);
            // insertDupStoreInsn(I, 0x2000000000);
            changed = true;
          }
        }
      }

      return changed;
    }


    virtual bool runOnFunction(Function &F) {
      bool changed = false;
      FUNC = &F;

      errs() << "--- [Wak insert duplicate instuction test] ---\n";
      errs() << "Function: ";
      errs().write_escaped(F.getName());
      errs() << "\n\n";

      if (insertDuplicationInstructions(F)) // 代入時に複製を作る
        changed = true;
      if (insertCheckInstructions(F)) // 参照時に複製を作る
        changed = true;

      errs() << "-------------------- [wak] --------------------\n\n";
      return true;
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
