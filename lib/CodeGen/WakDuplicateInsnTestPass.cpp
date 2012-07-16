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
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/IRBuilder.h"

using namespace llvm;

namespace {
  class WakDuplicateInsnTest : public FunctionPass {
  public:
    static char ID;

    WakDuplicateInsnTest() : FunctionPass(ID) {
      initializeWakDuplicateInsnTestPass(*PassRegistry::getPassRegistry());
    }

  private:
    void insertDupStoreInsn(StoreInst *insn) {
      /*
       * %1 = and      i64 ptrtoint (i32* @val to i64), 0x1fffff
       * %2 = or       i64 %1, 0x1000000000
       * %3 = inttoptr i64 %2 to i32*
       * store i32 100, i32* %3, align 4
       */
      IRBuilder<> B(insn);

      Value *r1 = B.CreateAnd(B.CreatePtrToInt(insn->getPointerOperand(),
                                               B.getInt64Ty()),
                              B.getInt64(0x1fffff));
      Value *r2 = B.CreateOr(r1, B.getInt64(0x1000000000));
      Value *r3 = B.CreateIntToPtr(r2, insn->getPointerOperand()->getType());
      B.CreateStore(insn->getValueOperand(), r3);
    }

    virtual bool runOnFunction(Function &F) {
      bool changed = false;

      errs() << "vvv [Wak insert duplicate instuction test] vvv\n";
      errs() << "Function: ";
      errs().write_escaped(F.getName());
      errs() << "\n\n";

      Function::iterator i = F.begin();
      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // for each block
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // for each instruction

          if (StoreInst *insn = dyn_cast<StoreInst>(I)) {
            if (insn->getPointerOperand()->isecc) {
              errs() << "  Store instruction with ecc lvalue found.\n";
              insertDupStoreInsn(insn);
              changed = true;
            }
          }
        }
      }
      errs() << "^^^ [Wak insert duplicate instruction test] ^^^\n";

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
