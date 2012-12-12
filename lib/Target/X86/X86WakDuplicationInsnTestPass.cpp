// 二重化をMachineFunctionPassでやろうとした名残り．
// MFPでやろうと思ったけれど，大変なのでWakHammingEccに移行しています．

#include "llvm/Pass.h"
#include "llvm/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetInstrItineraries.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "/home/wak/work/llvm/light/llvm-2.9/lib/Target/X86/X86.h"

using namespace llvm;

// メモ
//   TID: TargetInstrDescriptor
namespace {
class X86WakDuplicateInsnTest : public MachineFunctionPass {
public:
  static char ID;

  const TargetLowering *TLI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  const InstrItineraryData *InstrItins;
  const MachineLoopInfo *MLI;
  int FnNum;

  X86WakDuplicateInsnTest();
  virtual bool runOnMachineFunction(MachineFunction &MF);

  // 必要
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
  virtual const char *getPassName() const {
    return "X86 Wak Duplication Test Pass";
  }
};
}

char X86WakDuplicateInsnTest::ID = 0;
INITIALIZE_PASS_BEGIN(X86WakDuplicateInsnTest, "wak-x86-duplicate-insn-test", "X86WakDuplicateInsnTest", false, false)
INITIALIZE_PASS_END(X86WakDuplicateInsnTest, "wak-x86-duplicate-insn-test", "X86WakDuplicateInsnTest", false, false)

FunctionPass *llvm::createX86WakDuplicateInsnTestPass(X86TargetMachine &TM, llvm::CodeGenOpt::Level OptLevel) {
  return new X86WakDuplicateInsnTest();
}

X86WakDuplicateInsnTest::X86WakDuplicateInsnTest() : MachineFunctionPass(ID), FnNum(-1) {
  initializeX86WakDuplicateInsnTestPass(*PassRegistry::getPassRegistry());
}

bool X86WakDuplicateInsnTest::runOnMachineFunction(MachineFunction &MF) {
  errs() << "wak: Processing function (" << ++FnNum <<  ") \'"
         << MF.getFunction()->getName() << "\'\n";

  TLI = MF.getTarget().getTargetLowering();
  TII = MF.getTarget().getInstrInfo();
  TRI = MF.getTarget().getRegisterInfo();
  InstrItins = MF.getTarget().getInstrItineraryData();
  if (!TII)
    return false;

  for (MachineFunction::iterator mbbi = MF.begin(), E = MF.end(); mbbi != E; ++mbbi) {
    // mbbi: MachineBasicBlock Iterator
    errs() << "wak: BB\n";

    for (MachineBasicBlock::iterator mii = mbbi->begin(); mii != mbbi->end(); ++mii) {
      // mii: MachineInstr Iterator
      errs() << "wak:  inst: " << mii->getDesc().getName() << "\n";

      for (MachineInstr::mmo_iterator i = mii->memoperands_begin(); i != mii->memoperands_end(); ++i) {
        bool point = false;
        MachineMemOperand *operand = *i;
        const Value *value = operand->getValue();

        errs() << "wak:   mem: " << (void *)(operand->getValue()) << "\n";
        if (value->isecc)
          point = true;
        if (const GetElementPtrInst *insn = dyn_cast<GetElementPtrInst>(value)) {
          if (insn->getPointerOperand()->isecc)
            point = true;
        }
        if (point) {
          errs() << "wak:   found: " << mii->getDesc().getName()
                 << "(" <<  mii->getOpcode() << ")" << "\n";
          break;
        }
      }
    }
  }
  return true;
}
