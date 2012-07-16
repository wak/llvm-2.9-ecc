#define DEBUG_TYPE "wak-ecc-check"
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
using namespace llvm;

namespace {
  class WakEccCheck : public FunctionPass {
  public:
    static char ID;
    WakEccCheck() : FunctionPass(ID) {
      initializeStackProtectorPass(*PassRegistry::getPassRegistry());
    }

  private:
    virtual bool runOnFunction(Function &F) {
      errs() << "Wak Pass: runOnFunction(";
      errs().write_escaped(F.getName()) << ")\n";

      Function::iterator i = F.begin();
      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // for each block
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // for each instruction
          errs() << "Wak Pass: opcode = " << I->getOpcode() << "\n";
          for (Instruction::op_iterator O = I->op_begin(), E = I->op_end(); O != E; O++) {
            // O: Use *
            Value *v = O->get();
            errs () << "  Operand: " << v << " (ECC: " << v->isecc << ")\n";
          }
        }
      }
      return false;
    }
    virtual const char *getPassName() const {
      return "WakPass";
    }
  };
}

char WakEccCheck::ID = 0;
INITIALIZE_PASS(WakEccCheck, "wak-ecc-check",
                "Wak Ecc Checker Function Module", false, false)

FunctionPass *llvm::createWakEccCheckPass(const TargetLowering *tli) {
  return new WakEccCheck();
}
