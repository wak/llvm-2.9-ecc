
#include "llvm/Pass.h"
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
	class WakTest : public MachineFunctionPass {
	public:
		static char ID;

		const TargetLowering *TLI;
		const TargetInstrInfo *TII;
		const TargetRegisterInfo *TRI;
		const InstrItineraryData *InstrItins;
		const MachineLoopInfo *MLI;
		int FnNum;

		WakTest();
		virtual bool runOnMachineFunction(MachineFunction &MF);

		// 必要
		virtual void getAnalysisUsage(AnalysisUsage &AU) const {
			AU.setPreservesCFG();
			MachineFunctionPass::getAnalysisUsage(AU);
		}
		virtual const char *getPassName() const {
			return "WakTestMachinePass";
		}
	};
}
char WakTest::ID = 0;

INITIALIZE_PASS_BEGIN(WakTest, "wak-machine-pass", "Wak MachinePass", false, false)
INITIALIZE_PASS_END(WakTest, "wak-machine-pass", "Wak MachinePass", false, false)

FunctionPass *llvm::createX86WakTest(X86TargetMachine &TM, llvm::CodeGenOpt::Level OptLevel) {
	return new WakTest();
}

WakTest::WakTest() : MachineFunctionPass(ID), FnNum(-1) {
	initializeWakTestPass(*PassRegistry::getPassRegistry());
}

bool WakTest::runOnMachineFunction(MachineFunction &MF) {
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

		for (MachineBasicBlock::iterator mii = mbbi->begin(); mii != mbbi->end(); ++mii) {
			// mii: MachineInstr Iterator
      // *miiは，MachineInstr
			// アーキテクチャ依存の命令（SUB64ri8レベル）は，TargetInstrDescに格納されている
			// mii::getDescでTargetInstrDescが取得できる

			errs() << "  opcode: " << mii->getDesc().getName()
			       << "(" <<  mii->getOpcode() << ")" << "\n";

			if (mii->getOpcode() != 1424) // MOV64mr 1424
				continue;
			errs() << "  Insert >>>>>>>\n";
			MachineInstr *instr = MF.CreateMachineInstr(TII->get(X86::SYSEXIT), mii->getDebugLoc(), true);
			mbbi->insert(mii, instr);
			errs() << "  <<<<<<<<<<<<<<\n";
		}
	}
	return true;
}

//static RegisterPass<WakTest> X("wak-machine", "Wak machine function pass", false, false);
