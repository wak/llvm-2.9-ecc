//===-- RegAllocFast.cpp - A fast register allocator for debug code -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This register allocator allocates registers to a basic block at a time,
// attempting to keep values in registers and reusing registers as appropriate.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "regalloc"
#include "llvm/BasicBlock.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "../Target/X86/X86.h"  // wak: bad way
#include "llvm/WakOptions.h"    // wak
#include "llvm/Support/Wak.h"    // wak
#include <algorithm>
using namespace llvm;

STATISTIC(NumStores, "Number of stores added");
STATISTIC(NumLoads , "Number of loads added");
STATISTIC(NumCopies, "Number of copies coalesced");

static RegisterRegAlloc
  fastRegAlloc("fast", "fast register allocator", createFastRegisterAllocator);

namespace {
  class RAFast : public MachineFunctionPass {
  public:
    static char ID;
    RAFast() : MachineFunctionPass(ID), StackSlotForVirtReg(-1),
               isBulkSpilling(false) {
      initializePHIEliminationPass(*PassRegistry::getPassRegistry());
      initializeTwoAddressInstructionPassPass(*PassRegistry::getPassRegistry());
    }
  private:
    const TargetMachine *TM;
    MachineFunction *MF;
    MachineRegisterInfo *MRI;
    const TargetRegisterInfo *TRI;
    const TargetInstrInfo *TII;

    // Basic block currently being allocated.
    MachineBasicBlock *MBB;

    // StackSlotForVirtReg - Maps virtual regs to the frame index where these
    // values are spilled.
    IndexedMap<int, VirtReg2IndexFunctor> StackSlotForVirtReg;

    // Everything we know about a live virtual register.
    struct LiveReg {
      MachineInstr *LastUse;    // Last instr to use reg.
      unsigned PhysReg;         // Currently held here.
      unsigned short LastOpNum; // OpNum on LastUse.
      bool Dirty;               // Register needs spill.

      LiveReg(unsigned p=0) : LastUse(0), PhysReg(p), LastOpNum(0),
                              Dirty(false) {}
    };

    typedef DenseMap<unsigned, LiveReg> LiveRegMap;
    typedef LiveRegMap::value_type LiveRegEntry;

    // LiveVirtRegs - This map contains entries for each virtual register
    // that is currently available in a physical register.
    // wak: 仮想レジスタ => 現在の利用可能な物理レジスタへのマッピング
    LiveRegMap LiveVirtRegs;

    DenseMap<unsigned, MachineInstr *> LiveDbgValueMap;

    // RegState - Track the state of a physical register.
    enum RegState {
      // A disabled register is not available for allocation, but an alias may
      // be in use. A register can only be moved out of the disabled state if
      // all aliases are disabled.
      regDisabled,

      // A free register is not currently in use and can be allocated
      // immediately without checking aliases.
      // wak: このレジスタは，現在使われていない．エイリアスをチェックせずに，
      //      使うことができる．
      regFree,

      // A reserved register has been assigned expolicitly (e.g., setting up a
      // call parameter), and it remains reserved until it is used.
      // wak: 予約済みレジスタは，既に明示的（例えば，関数呼び出しのパラメータ）
      //      に割り付けられており，使われるまで予約したままである．
      regReserved

      // A register state may also be a virtual register number, indication that
      // the physical register is currently allocated to a virtual register. In
      // that case, LiveVirtRegs contains the inverse mapping.

      // wak: レジスタステートは，仮想レジスタ番号かもしれない．その場合は，物理
      //      レジスタは，現在仮想レジスタに割付けられていることを意味する．この
      //      ケースでは，LiveVirtRegsが逆のマッピング（仮想->物理）を保持してい
      //      る．
    };

    // PhysRegState - One of the RegState enums, or a virtreg.
    // wak: 物理レジスタの使用状況．
    // wak: 物理レジスタ => RegState or 仮想レジスタ番号
    std::vector<unsigned> PhysRegState;

    // UsedInInstr - BitVector of physregs that are used in the current
    // instruction, and so cannot be allocated.
    // wak: 現在の命令における，物理レジスタの使用状況．つまり，割付け出来ない．
    BitVector UsedInInstr;

    // Allocatable - vector of allocatable physical registers.
    // wak: 割付け可能な物理レジスタ（使用状況は除外？）
    BitVector Allocatable;

    // SkippedInstrs - Descriptors of instructions whose clobber list was
    // ignored because all registers were spilled. It is still necessary to
    // mark all the clobbered registers as used by the function.

    // wak: clobber listが無効である命令（すべてのレジスタがspillされた）の詳細．
    //      これは，関数で，すべてのclobberedなレジスタをマークするために必要である？
    SmallPtrSet<const TargetInstrDesc*, 4> SkippedInstrs;

    // isBulkSpilling - This flag is set when LiveRegMap will be cleared
    // completely after spilling all live registers. LiveRegMap entries should
    // not be erased.
    // wak: LiveRegMapすべてのliveレジスタが完全にクリアされてるときにセットされる．
    //      LiveRegMapのエントリは，削除すべきではない（しなくてもよい）．
    //      コンパイル速度向上のため．気にしなくてもよい．
    bool isBulkSpilling;

    enum {
      spillClean = 1,
      spillDirty = 100,
      spillImpossible = ~0u
    };
  public:
    virtual const char *getPassName() const {
      return "Fast Register Allocator";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequiredID(PHIEliminationID);
      AU.addRequiredID(TwoAddressInstructionPassID);
      MachineFunctionPass::getAnalysisUsage(AU);
    }

  private:
    bool runOnMachineFunction(MachineFunction &Fn);
    void AllocateBasicBlock();
    void handleThroughOperands(MachineInstr *MI,
                               SmallVectorImpl<unsigned> &VirtDead);
    int getStackSpaceFor(unsigned VirtReg, const TargetRegisterClass *RC);
    bool isLastUseOfLocalReg(MachineOperand&);

    void addKillFlag(const LiveReg&);
    void killVirtReg(LiveRegMap::iterator);
    void killVirtReg(unsigned VirtReg);
    void spillVirtReg(MachineBasicBlock::iterator MI, LiveRegMap::iterator);
    void spillVirtReg(MachineBasicBlock::iterator MI, unsigned VirtReg);

    void usePhysReg(MachineOperand&);
    void definePhysReg(MachineInstr *MI, unsigned PhysReg, RegState NewState);
    unsigned calcSpillCost(unsigned PhysReg) const;
    void assignVirtToPhysReg(LiveRegEntry &LRE, unsigned PhysReg);
    void allocVirtReg(MachineInstr *MI, LiveRegEntry &LRE, unsigned Hint);
    LiveRegMap::iterator defineVirtReg(MachineInstr *MI, unsigned OpNum,
                                       unsigned VirtReg, unsigned Hint);
    LiveRegMap::iterator reloadVirtReg(MachineInstr *MI, unsigned OpNum,
                                       unsigned VirtReg, unsigned Hint);
    void spillAll(MachineInstr *MI);
    bool setPhysReg(MachineInstr *MI, unsigned OpNum, unsigned PhysReg);


    unsigned mustUseWakPhysReg; // wak: レジスタ割付け場面で，実際に割り付けたいレジスタ
    bool wakMyRegisterAllocated;
    void wakAssertMyRegisterNotUsed(unsigned reg = 0, const char *pos = "?") {
      return;                   // @todo fix me
      if (!wakMyRegisterAllocated)
        return;
      if (reg == X86::R15)
        errs() << "wak: error: X86::R15 may used (pos: " << pos << ").\n";
      if (PhysRegState[X86::R15] != regDisabled)
        errs() << "wak: error: X86::R15 is used (pos: " << pos << ").\n";
    }

    DenseMap<unsigned, unsigned> wakForceVRegToPRegMap;
    bool wakRegisterLotateFlag;
    void wakResetRegisterLotate(void) {
      wakRegisterLotateFlag = true;
    }
    void wakRegisterLotate(void) {
      wakRegisterLotateFlag = !wakRegisterLotateFlag;
    }
    // wak: バイト数にマッチするものを返す
    //      AliasSetを使えば，もっと美しくできるはず
    unsigned wakRegisterForBytes(unsigned bytes = 8) {
      if (wakRegisterLotateFlag) {
        switch (bytes) {
        case 8: return X86::R15;
        case 4: return X86::R15D;
        case 2: return X86::R15W;
        case 1: return X86::R15B;
        }
      } else {
        switch (bytes) {
        case 8: return X86::R14;
        case 4: return X86::R14D;
        case 2: return X86::R14W;
        case 1: return X86::R14B;
        }
      }
      errs() << "wak: error cannot select my register for size = " << bytes << "\n";
      llvm_unreachable("unknown register size");
    }

    // wak: 物理レジスタは，Myレジスタか？
    bool isWakRegister(unsigned PhysReg) {
      return (X86::R15 <= PhysReg && PhysReg <= X86::R15W) ||
             (X86::R14 <= PhysReg && PhysReg <= X86::R14W);
    }
  };
  char RAFast::ID = 0;
}

/// getStackSpaceFor - This allocates space for the specified virtual register
/// to be held on the stack.
int RAFast::getStackSpaceFor(unsigned VirtReg, const TargetRegisterClass *RC) {
  // Find the location Reg would belong...
  int SS = StackSlotForVirtReg[VirtReg];
  if (SS != -1)
    return SS;          // Already has space allocated?

  // Allocate a new stack object for this spill location...
  int FrameIdx = MF->getFrameInfo()->CreateSpillStackObject(RC->getSize(),
                                                            RC->getAlignment());

  // Assign the slot.
  StackSlotForVirtReg[VirtReg] = FrameIdx;
  return FrameIdx;
}

/// isLastUseOfLocalReg - Return true if MO is the only remaining reference to
/// its virtual register, and it is guaranteed to be a block-local register.
///
bool RAFast::isLastUseOfLocalReg(MachineOperand &MO) {
  // Check for non-debug uses or defs following MO.
  // This is the most likely way to fail - fast path it.
  MachineOperand *Next = &MO;
  while ((Next = Next->getNextOperandForReg()))
    if (!Next->isDebug())
      return false;

  // If the register has ever been spilled or reloaded, we conservatively assume
  // it is a global register used in multiple blocks.
  if (StackSlotForVirtReg[MO.getReg()] != -1)
    return false;

  // Check that the use/def chain has exactly one operand - MO.
  return &MRI->reg_nodbg_begin(MO.getReg()).getOperand() == &MO;
}

/// addKillFlag - Set kill flags on last use of a virtual register.
// wak: 最後の使用点だったら，setIsKillする
void RAFast::addKillFlag(const LiveReg &LR) {
  if (!LR.LastUse) return;
  MachineOperand &MO = LR.LastUse->getOperand(LR.LastOpNum);
  if (MO.isUse() && !LR.LastUse->isRegTiedToDefOperand(LR.LastOpNum)) {
    if (MO.getReg() == LR.PhysReg)
      MO.setIsKill();
    else
      LR.LastUse->addRegisterKilled(LR.PhysReg, TRI, true);
  }
}

/// killVirtReg - Mark virtreg as no longer available.
// wak: 仮想レジスタをもう利用できないとマークする
void RAFast::killVirtReg(LiveRegMap::iterator LRI) {
  wakAssertMyRegisterNotUsed(0, "1"); // wak

  addKillFlag(LRI->second);
  const LiveReg &LR = LRI->second;
  assert(PhysRegState[LR.PhysReg] == LRI->first && "Broken RegState mapping");
  PhysRegState[LR.PhysReg] = regFree;
  // Erase from LiveVirtRegs unless we're spilling in bulk.
  if (!isBulkSpilling)
    LiveVirtRegs.erase(LRI);
}

/// killVirtReg - Mark virtreg as no longer available.
void RAFast::killVirtReg(unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "killVirtReg needs a virtual register");
  LiveRegMap::iterator LRI = LiveVirtRegs.find(VirtReg);
  if (LRI != LiveVirtRegs.end())
    killVirtReg(LRI);
}

/// spillVirtReg - This method spills the value specified by VirtReg into the
/// corresponding stack slot if needed.
/// wak: VirtRegによって指定された仮想レジスタの値を，スタックスロットにspillする．
/// wak: Spillする仮想レジスタは，物理レジスタが割り付けられていなければダメ．
void RAFast::spillVirtReg(MachineBasicBlock::iterator MI, unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Spilling a physical register is illegal!");
  LiveRegMap::iterator LRI = LiveVirtRegs.find(VirtReg);
  assert(LRI != LiveVirtRegs.end() && "Spilling unmapped virtual register");
  spillVirtReg(MI, LRI);
}

/// spillVirtReg - Do the actual work of spilling.
/// wak: LRI->second.Dirtyなら，Spillする．
/// wak: その後，regFreeにする．最終使用点の場合は，killフラグが付く．
void RAFast::spillVirtReg(MachineBasicBlock::iterator MI,
                          LiveRegMap::iterator LRI) {
  LiveReg &LR = LRI->second;
  assert(PhysRegState[LR.PhysReg] == LRI->first && "Broken RegState mapping");

  if (LR.Dirty) {
    // If this physreg is used by the instruction, we want to kill it on the
    // instruction, not on the spill.
    bool SpillKill = LR.LastUse != MI;
    LR.Dirty = false;
    DEBUG(dbgs() << "Spilling " << PrintReg(LRI->first, TRI)
                 << " in " << PrintReg(LR.PhysReg, TRI));

    // wak: wak専用レジスタがspillされようとしていたら，やめさせる
    if (OptWakRegAlloc && isWakRegister(LR.PhysReg)) {
      if (OptWakDebugRegAlloc)
        errs() << "\nwak: register " << PrintReg(LR.PhysReg, TRI)
               << " is going to spill, but it is wak Register. Spill is canceled force !!\n";
      LR.LastUse = 0; // Don't kill register again @todo: これは必要か？
      goto end;
    }

    const TargetRegisterClass *RC = MRI->getRegClass(LRI->first);
    int FI = getStackSpaceFor(LRI->first, RC);
    DEBUG(dbgs() << " to stack slot #" << FI << "\n");
    TII->storeRegToStackSlot(*MBB, MI, LR.PhysReg, SpillKill, FI, RC, TRI);
    ++NumStores;   // Update statistics

    // If this register is used by DBG_VALUE then insert new DBG_VALUE to
    // identify spilled location as the place to find corresponding variable's
    // value.
    if (MachineInstr *DBG = LiveDbgValueMap.lookup(LRI->first)) {
      const MDNode *MDPtr =
        DBG->getOperand(DBG->getNumOperands()-1).getMetadata();
      int64_t Offset = 0;
      if (DBG->getOperand(1).isImm())
        Offset = DBG->getOperand(1).getImm();
      DebugLoc DL;
      if (MI == MBB->end()) {
        // If MI is at basic block end then use last instruction's location.
        MachineBasicBlock::iterator EI = MI;
        DL = (--EI)->getDebugLoc();
      }
      else
        DL = MI->getDebugLoc();
      if (MachineInstr *NewDV =
          TII->emitFrameIndexDebugValue(*MF, FI, Offset, MDPtr, DL)) {
        MachineBasicBlock *MBB = DBG->getParent();
        MBB->insert(MI, NewDV);
        DEBUG(dbgs() << "Inserting debug info due to spill:" << "\n" << *NewDV);
        LiveDbgValueMap[LRI->first] = NewDV;
      }
    }

    if (SpillKill)
      LR.LastUse = 0; // Don't kill register again
  }

  end:                          // wak
  killVirtReg(LRI);
}

/// spillAll - Spill all dirty virtregs without killing them.
void RAFast::spillAll(MachineInstr *MI) {
  if (LiveVirtRegs.empty()) return;
  isBulkSpilling = true;
  // The LiveRegMap is keyed by an unsigned (the virtreg number), so the order
  // of spilling here is deterministic, if arbitrary.
  for (LiveRegMap::iterator i = LiveVirtRegs.begin(), e = LiveVirtRegs.end();
       i != e; ++i)
    spillVirtReg(MI, i);
  LiveVirtRegs.clear();
  isBulkSpilling = false;
}

/// usePhysReg - Handle the direct use of a physical register.
/// Check that the register is not used by a virtreg.
/// Kill the physreg, marking it free.
/// This may add implicit kills to MO->getParent() and invalidate MO.

/// wak: 物理レジスタを直接使用するようにする．物理レジスタが仮想レジスタに使われ
///      いないかをチェックし，物理レジスタをkillし，regFreeにする．これは，もし
///      かするとMO->getParent()に暗黙killやMOの無効化をするかもしれない．
/// wak: なぜFree？なぜKILL？
///      regFreeにするのは，これから使ってもいいよ．という意味．
///      Killは何でだろう？
void RAFast::usePhysReg(MachineOperand &MO) {
  wakAssertMyRegisterNotUsed(0, "2"); // wak

  unsigned PhysReg = MO.getReg();
  assert(TargetRegisterInfo::isPhysicalRegister(PhysReg) &&
         "Bad usePhysReg operand");

  switch (PhysRegState[PhysReg]) {
  case regDisabled:           // wak: 処理しない
    break;
  case regReserved:
    PhysRegState[PhysReg] = regFree;
    // Fall through
  case regFree:
    UsedInInstr.set(PhysReg);
    MO.setIsKill();
    return;
  default:
    // The physreg was allocated to a virtual register. That means the value we
    // wanted has been clobbered.
    llvm_unreachable("Instruction uses an allocated register");
  }

  // wak: もしかして，スーパーレジスタが予約済みかも？
  // Maybe a superregister is reserved?
  for (const unsigned *AS = TRI->getAliasSet(PhysReg); unsigned Alias = *AS; ++AS) {
    switch (PhysRegState[Alias]) {
    case regDisabled:           // wak: 処理しない
      break;
    case regReserved:
      assert(TRI->isSuperRegister(PhysReg, Alias) &&
             "Instruction is not using a subregister of a reserved register");
      // Leave the superregister in the working set.
      PhysRegState[Alias] = regFree;
      UsedInInstr.set(Alias);
      MO.getParent()->addRegisterKilled(Alias, TRI, true);
      return;
    case regFree:
      if (TRI->isSuperRegister(PhysReg, Alias)) {
        // Leave the superregister in the working set.
        UsedInInstr.set(Alias);
        MO.getParent()->addRegisterKilled(Alias, TRI, true);
        return;
      }
      // Some other alias was in the working set - clear it.
      PhysRegState[Alias] = regDisabled;
      break;
    default:
      llvm_unreachable("Instruction uses an alias of an allocated register");
    }
  }

  // All aliases are disabled, bring register into working set.
  PhysRegState[PhysReg] = regFree;
  UsedInInstr.set(PhysReg);
  MO.setIsKill();
}

/// definePhysReg - Mark PhysReg as reserved or free after spilling any
/// virtregs. This is very similar to defineVirtReg except the physreg is
/// reserved instead of allocated.

// wak: 物理レジスタに割り当てられているの仮想レジスタをspillした後，物理レジスタ
//      をregReservedまたはregFreeにする．これは，defineVirtReg()とよく似ているが，
//      物理レジスタを「確保する」の代わりに「予約する」という違いがある．
void RAFast::definePhysReg(MachineInstr *MI, unsigned PhysReg,
                           RegState NewState) {
  wakAssertMyRegisterNotUsed(PhysReg, "3"); // wak

  UsedInInstr.set(PhysReg);
  switch (unsigned VirtReg = PhysRegState[PhysReg]) {
  case regDisabled:
    break;
  default:
    // wak: 仮想レジスタに割り付けられていたら，Spillする．
    spillVirtReg(MI, VirtReg);
    // Fall through.
  case regFree:
  case regReserved:
    PhysRegState[PhysReg] = NewState;
    return;
  }

  // This is a disabled register, disable all aliases.
  PhysRegState[PhysReg] = NewState;
  for (const unsigned *AS = TRI->getAliasSet(PhysReg);
       unsigned Alias = *AS; ++AS) {
    UsedInInstr.set(Alias);
    switch (unsigned VirtReg = PhysRegState[Alias]) {
    case regDisabled:
      break;
    default:
      spillVirtReg(MI, VirtReg);
      // Fall through.
    case regFree:
    case regReserved:
      PhysRegState[Alias] = regDisabled;
      if (TRI->isSuperRegister(PhysReg, Alias))
        return;
      break;
    }
  }
  wakAssertMyRegisterNotUsed(0, "4"); // wak
}


// calcSpillCost - Return the cost of spilling clearing out PhysReg and
// aliases so it is free for allocation.
// Returns 0 when PhysReg is free or disabled with all aliases disabled - it
// can be allocated directly.
// Returns spillImpossible when PhysReg or an alias can't be spilled.
// wak: 物理レジスタをspillするためのコストを返す．
//   戻り値:
//     0:
//       物理レジスタがフリー，または，すべてのエイリアスが無効．
//       直接割り当てることができる．
//     spillImpossible:
//       物理レジスタまたはエイリアスをspillする事が出来ない
unsigned RAFast::calcSpillCost(unsigned PhysReg) const {
  // wak: @memo: このあたりをうまく使って，確保したレジスタでspillImpossibleを渡したらだめかな？
  if (UsedInInstr.test(PhysReg))
    return spillImpossible;
  switch (unsigned VirtReg = PhysRegState[PhysReg]) {
  case regDisabled:
    break;
  case regFree:
    return 0;
  case regReserved:
    // wak: これから使う予定があるから，Spillしてはダメ
    return spillImpossible;
  default:
    return LiveVirtRegs.lookup(VirtReg).Dirty ? spillDirty : spillClean;
  }

  // This is a disabled register, add up const of aliases.
  unsigned Cost = 0;
  for (const unsigned *AS = TRI->getAliasSet(PhysReg);
       unsigned Alias = *AS; ++AS) {
    if (UsedInInstr.test(Alias))
      return spillImpossible;
    switch (unsigned VirtReg = PhysRegState[Alias]) {
    case regDisabled:
      break;
    case regFree:
      ++Cost;
      break;
    case regReserved:
      return spillImpossible;
    default:
      Cost += LiveVirtRegs.lookup(VirtReg).Dirty ? spillDirty : spillClean;
      break;
    }
  }
  return Cost;
}


/// assignVirtToPhysReg - This method updates local state so that we know
/// that PhysReg is the proper container for VirtReg now.  The physical
/// register must not be used for anything else when this is called.
///
/// wak: ローカルステートを更新する．すなわち，PhysRegが適切なVirtRegのためのコン
///      テナである？
///      物理レジスタは，別の用途に使われていてはいけない．

void RAFast::assignVirtToPhysReg(LiveRegEntry &LRE, unsigned PhysReg) {
  DEBUG(dbgs() << "Assigning " << PrintReg(LRE.first, TRI) << " to "
               << PrintReg(PhysReg, TRI) << "\n");
  PhysRegState[PhysReg] = LRE.first;
  assert(!LRE.second.PhysReg && "Already assigned a physreg");
  LRE.second.PhysReg = PhysReg;
}

/// allocVirtReg - Allocate a physical register for VirtReg.
// wak: 物理レジスタを論理レジスタのために割り当てる
void RAFast::allocVirtReg(MachineInstr *MI, LiveRegEntry &LRE, unsigned Hint) {
  const unsigned VirtReg = LRE.first;

  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Can only allocate virtual registers");

  const TargetRegisterClass *RC = MRI->getRegClass(VirtReg);

  // Ignore invalid hints.
  if (Hint && (!TargetRegisterInfo::isPhysicalRegister(Hint) ||
               !RC->contains(Hint) || !Allocatable.test(Hint))) {
    // wak: TwoAddressInstructionPassでできたCOPYは，
    //      RC->contains(Hint) == trueになり，Hintが0になっている
    //      他のCOPYは調べていないのでわからないが，なぜだろう？
    Hint = 0;
  }

  // Take hint when possible.
  if (Hint) {
    switch(calcSpillCost(Hint)) {
    default:
      definePhysReg(MI, Hint, regFree);
      // Fall through.
    case 0:
      return assignVirtToPhysReg(LRE, Hint);
    case spillImpossible:
      break;
    }
  }

  TargetRegisterClass::iterator AOB = RC->allocation_order_begin(*MF);
  TargetRegisterClass::iterator AOE = RC->allocation_order_end(*MF);

  if (!mustUseWakPhysReg) {     // wak: とてもよくない実装
    // First try to find a completely free register.
    for (TargetRegisterClass::iterator I = AOB; I != AOE; ++I) {
      unsigned PhysReg = *I;
      if (PhysRegState[PhysReg] == regFree && !UsedInInstr.test(PhysReg) &&
          Allocatable.test(PhysReg))
        return assignVirtToPhysReg(LRE, PhysReg);
    }
  }

  DEBUG(dbgs() << "Allocating " << PrintReg(VirtReg) << " from "
               << RC->getName() << "\n");

  unsigned BestReg = 0, BestCost = spillImpossible;
  for (TargetRegisterClass::iterator I = AOB; I != AOE; ++I) {
    if (!Allocatable.test(*I))
      continue;
    unsigned Cost = calcSpillCost(*I);
    // Cost is 0 when all aliases are already disabled.
    if (Cost == 0 && !mustUseWakPhysReg) // wak
      return assignVirtToPhysReg(LRE, *I);
    if (Cost < BestCost)
      BestReg = *I, BestCost = Cost;
  }

  if (mustUseWakPhysReg)        // wak
    BestReg = mustUseWakPhysReg;

  if (BestReg) {
    definePhysReg(MI, BestReg, regFree);
    return assignVirtToPhysReg(LRE, BestReg);
  }

  // Nothing we can do.
  std::string msg;
  raw_string_ostream Msg(msg);
  Msg << "Ran out of registers during register allocation!";
  if (MI->isInlineAsm()) {
    Msg << "\nPlease check your inline asm statement for "
        << "invalid constraints:\n";
    MI->print(Msg, TM);
  }
  report_fatal_error(Msg.str());
}

/// defineVirtReg - Allocate a register for VirtReg and mark it as dirty.
// wak: 仮想レジスタのためのレジスタを（管理メモリ領域に？）確保し，DIRTYとマークする．
RAFast::LiveRegMap::iterator
RAFast::defineVirtReg(MachineInstr *MI, unsigned OpNum,
                      unsigned VirtReg, unsigned Hint) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  LiveRegMap::iterator LRI;
  bool New;
  tie(LRI, New) = LiveVirtRegs.insert(std::make_pair(VirtReg, LiveReg()));
  LiveReg &LR = LRI->second;
  if (New) {
    // If there is no hint, peek at the only use of this register.
    if ((!Hint || !TargetRegisterInfo::isPhysicalRegister(Hint)) &&
        MRI->hasOneNonDBGUse(VirtReg)) {
      const MachineInstr &UseMI = *MRI->use_nodbg_begin(VirtReg);
      // It's a copy, use the destination register as a hint.
      if (UseMI.isCopyLike())
        Hint = UseMI.getOperand(0).getReg();
    }
    allocVirtReg(MI, *LRI, Hint);
  } else if (LR.LastUse) {
    // Redefining a live register - kill at the last use, unless it is this
    // instruction defining VirtReg multiple times.
    if (LR.LastUse != MI || LR.LastUse->getOperand(LR.LastOpNum).isUse())
      addKillFlag(LR);
  }
  assert(LR.PhysReg && "Register not assigned");
  LR.LastUse = MI;
  LR.LastOpNum = OpNum;
  LR.Dirty = true;
  UsedInInstr.set(LR.PhysReg);
  return LRI;
}

/// reloadVirtReg - Make sure VirtReg is available in a physreg and return it.
RAFast::LiveRegMap::iterator
RAFast::reloadVirtReg(MachineInstr *MI, unsigned OpNum,
                      unsigned VirtReg, unsigned Hint) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  LiveRegMap::iterator LRI;
  bool New;
  tie(LRI, New) = LiveVirtRegs.insert(std::make_pair(VirtReg, LiveReg()));
  LiveReg &LR = LRI->second;
  MachineOperand &MO = MI->getOperand(OpNum);

  // wak: すでに，レジスタ強制していた場合，それをもう一度割り付ける
  if (OptWakDebugRegAlloc)
    errs() << "wak: Reload info: VReg = " << PrintReg(VirtReg, TRI)
           << ", forcedReg = " << PrintReg(wakForceVRegToPRegMap[VirtReg], TRI) << "\n";
  if (wakForceVRegToPRegMap[VirtReg]) {
    assert(mustUseWakPhysReg == 0 && "レジスタ強制中にReloadが発生した");
    mustUseWakPhysReg = Hint = wakForceVRegToPRegMap[VirtReg];
    if (OptWakDebugRegAlloc)
      errs() << "wak: Relaoding already force allocated register ("
             << PrintReg(mustUseWakPhysReg, TRI) <<  ")\n";
  }

  if (New) {
    allocVirtReg(MI, *LRI, Hint);
    const TargetRegisterClass *RC = MRI->getRegClass(VirtReg);
    int FrameIndex = getStackSpaceFor(VirtReg, RC);
    DEBUG(dbgs() << "Reloading " << PrintReg(VirtReg, TRI) << " into "
                 << PrintReg(LR.PhysReg, TRI) << "\n");
    // wak: wak専用レジスタがreloadされようとしたら，やめさせる
    if (OptWakRegAlloc && isWakRegister(LR.PhysReg)) {
      if (OptWakDebugRegAlloc)
        errs() << "wak: register " << PrintReg(LR.PhysReg, TRI)
               << " is going to reload, but it is wak Register. Reload is canceled force !!\n";
      // do nothing
    } else
      TII->loadRegFromStackSlot(*MBB, MI, LR.PhysReg, FrameIndex, RC, TRI);
    ++NumLoads;
  } else if (LR.Dirty) {
    if (isLastUseOfLocalReg(MO)) {
      DEBUG(dbgs() << "Killing last use: " << MO << "\n");
      if (MO.isUse())
        MO.setIsKill();
      else
        MO.setIsDead();
    } else if (MO.isKill()) {
      DEBUG(dbgs() << "Clearing dubious kill: " << MO << "\n");
      MO.setIsKill(false);
    } else if (MO.isDead()) {
      DEBUG(dbgs() << "Clearing dubious dead: " << MO << "\n");
      MO.setIsDead(false);
    }
  } else if (MO.isKill()) {
    // We must remove kill flags from uses of reloaded registers because the
    // register would be killed immediately, and there might be a second use:
    //   %foo = OR %x<kill>, %x
    // This would cause a second reload of %x into a different register.
    DEBUG(dbgs() << "Clearing clean kill: " << MO << "\n");
    MO.setIsKill(false);
  } else if (MO.isDead()) {
    DEBUG(dbgs() << "Clearing clean dead: " << MO << "\n");
    MO.setIsDead(false);
  }
  assert(LR.PhysReg && "Register not assigned");
  LR.LastUse = MI;
  LR.LastOpNum = OpNum;
  UsedInInstr.set(LR.PhysReg);


  // wak: もう一度，同じ物理レジスタを割り付けることができたか？
  if (mustUseWakPhysReg) {
    if (LRI->second.PhysReg != mustUseWakPhysReg) {
      errs() << "wak: error: I want reload " << PrintReg(VirtReg, TRI)
             << " to " << PrintReg(mustUseWakPhysReg, TRI)
             << " but " << PrintReg(LRI->second.PhysReg, TRI) << " was allocated.\n";
      llvm_unreachable("failed to reload my register");
    }
    mustUseWakPhysReg = 0;
  }

  return LRI;
}

// setPhysReg - Change operand OpNum in MI the refer the PhysReg, considering
// subregs. This may invalidate any operand pointers.
// Return true if the operand kills its register.
bool RAFast::setPhysReg(MachineInstr *MI, unsigned OpNum, unsigned PhysReg) {
  MachineOperand &MO = MI->getOperand(OpNum);
  if (!MO.getSubReg()) {
    MO.setReg(PhysReg);
    return MO.isKill() || MO.isDead();
  }

  // Handle subregister index.
  MO.setReg(PhysReg ? TRI->getSubReg(PhysReg, MO.getSubReg()) : 0);
  MO.setSubReg(0);

  // A kill flag implies killing the full register. Add corresponding super
  // register kill.
  if (MO.isKill()) {
    MI->addRegisterKilled(PhysReg, TRI, true);
    return true;
  }
  return MO.isDead();
}

// Handle special instruction operand like early clobbers and tied ops when
// there are additional physreg defines.
void RAFast::handleThroughOperands(MachineInstr *MI,
                                   SmallVectorImpl<unsigned> &VirtDead) {
  DEBUG(dbgs() << "Scanning for through registers:");
  SmallSet<unsigned, 8> ThroughRegs;
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg()) continue;
    unsigned Reg = MO.getReg();
    if (!TargetRegisterInfo::isVirtualRegister(Reg))
      continue;
    if (MO.isEarlyClobber() || MI->isRegTiedToDefOperand(i) ||
        (MO.getSubReg() && MI->readsVirtualRegister(Reg))) {
      if (ThroughRegs.insert(Reg))
        DEBUG(dbgs() << ' ' << PrintReg(Reg));
    }
  }

  // If any physreg defines collide with preallocated through registers,
  // we must spill and reallocate.
  DEBUG(dbgs() << "\nChecking for physdef collisions.\n");
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg() || !MO.isDef()) continue;
    unsigned Reg = MO.getReg();
    if (!Reg || !TargetRegisterInfo::isPhysicalRegister(Reg)) continue;
    UsedInInstr.set(Reg);
    if (ThroughRegs.count(PhysRegState[Reg]))
      definePhysReg(MI, Reg, regFree);
    for (const unsigned *AS = TRI->getAliasSet(Reg); *AS; ++AS) {
      UsedInInstr.set(*AS);
      if (ThroughRegs.count(PhysRegState[*AS]))
        definePhysReg(MI, *AS, regFree);
    }
  }

  SmallVector<unsigned, 8> PartialDefs;
  DEBUG(dbgs() << "Allocating tied uses and early clobbers.\n");
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg()) continue;
    unsigned Reg = MO.getReg();
    if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
    if (MO.isUse()) {
      unsigned DefIdx = 0;
      if (!MI->isRegTiedToDefOperand(i, &DefIdx)) continue;
      DEBUG(dbgs() << "Operand " << i << "("<< MO << ") is tied to operand "
        << DefIdx << ".\n");
      LiveRegMap::iterator LRI = reloadVirtReg(MI, i, Reg, 0);
      unsigned PhysReg = LRI->second.PhysReg;
      setPhysReg(MI, i, PhysReg);
      // Note: we don't update the def operand yet. That would cause the normal
      // def-scan to attempt spilling.
    } else if (MO.getSubReg() && MI->readsVirtualRegister(Reg)) {
      DEBUG(dbgs() << "Partial redefine: " << MO << "\n");
      // Reload the register, but don't assign to the operand just yet.
      // That would confuse the later phys-def processing pass.
      LiveRegMap::iterator LRI = reloadVirtReg(MI, i, Reg, 0);
      PartialDefs.push_back(LRI->second.PhysReg);
    } else if (MO.isEarlyClobber()) {
      // Note: defineVirtReg may invalidate MO.
      LiveRegMap::iterator LRI = defineVirtReg(MI, i, Reg, 0);
      unsigned PhysReg = LRI->second.PhysReg;
      if (setPhysReg(MI, i, PhysReg))
        VirtDead.push_back(Reg);
    }
  }

  // Restore UsedInInstr to a state usable for allocating normal virtual uses.
  UsedInInstr.reset();
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg() || (MO.isDef() && !MO.isEarlyClobber())) continue;
    unsigned Reg = MO.getReg();
    if (!Reg || !TargetRegisterInfo::isPhysicalRegister(Reg)) continue;
    UsedInInstr.set(Reg);
    for (const unsigned *AS = TRI->getAliasSet(Reg); *AS; ++AS)
      UsedInInstr.set(*AS);
  }

  // Also mark PartialDefs as used to avoid reallocation.
  for (unsigned i = 0, e = PartialDefs.size(); i != e; ++i)
    UsedInInstr.set(PartialDefs[i]);
}

void RAFast::AllocateBasicBlock() {
  wakForceVRegToPRegMap.clear();       // wak: レジスタ強制マップをクリア
  wakResetRegisterLotate();            // wak
  mustUseWakPhysReg = false;           // wak
  wakMyRegisterAllocated = false;      // wak

  DEBUG(dbgs() << "\nAllocating " << *MBB);

  // FIXME: This should probably be added by instruction selection instead?
  // If the last instruction in the block is a return, make sure to mark it as
  // using all of the live-out values in the function.  Things marked both call
  // and return are tail calls; do not do this for them.  The tail callee need
  // not take the same registers as input that it produces as output, and there
  // are dependencies for its input registers elsewhere.
  // wak: 最後の命令が，returnする命令で，call命令で以外の場合
  // wak: @todo call命令でreturnするケースがあるのか？isReturn()は単に終端という意味か？
  if (!MBB->empty() && MBB->back().getDesc().isReturn() &&
      !MBB->back().getDesc().isCall()) {
    // wak: 一番最後の命令
    MachineInstr *Ret = &MBB->back();

    for (MachineRegisterInfo::liveout_iterator
         I = MF->getRegInfo().liveout_begin(),
         E = MF->getRegInfo().liveout_end(); I != E; ++I) {
      // wak: 全てのliveoutレジスタ（レジスタ使ったMIの戻り値）について...
      // wak: ここで扱うレジスタは，物理レジスタ
      assert(TargetRegisterInfo::isPhysicalRegister(*I) &&
             "Cannot have a live-out virtual register.");

      // Add live-out registers as implicit uses.
      // wak: live-outレジスタを暗黙の使用点にする．
      // wak: ？一番最後の命令に，この命令までにkillされるレジスタを教える？
      // wak: ？この命令で，もうこのレジスタは使われないよ．ということ．
      // wak: I   = MachineRegisterInfo
      // wak: TRI = TargetRegisterInfo
      // wak: live in: 関数の引数のレジスタ
      // wak: live out: 関数の戻り値のレジスタ
      if (OptWakDebugRegAlloc)
        errs() << "wak: addRegisterKilled(" << TRI->getName(*I) << ")\n";

      // addRegisterKilled:
      //   We have determined MI kills a register. Look for the operand that
      // uses it and mark it as IsKill. If AddIfNotFound is true, add a
      // implicit operand if it's not found. Returns true if the operand exists / is added.
      // 
      // wak: KILLは，後続の命令では決して使われないことを示す．
      //      そうすると，うっかりお掃除されたりしないのだろうか...？
      //      データ構造上掃除されても，KILLレジスタを変更さえしなければよい．
      Ret->addRegisterKilled(*I, TRI, true);
    }
  }

  // wak: これからPhysRegStateを使うから，初期化する（前のデータが残ってるかもしれないので）
  PhysRegState.assign(TRI->getNumRegs(), regDisabled);
  assert(LiveVirtRegs.empty() && "Mapping not cleared form last block?");

  // wak: Myレジスタ確保
  // errs() << "wak: disable register X86::R15 at head of AllocateBasicBlock()\n";
  // definePhysReg(NULL, X86::R15, regDisabled);
  // wakMyRegisterAllocated = true;
  wakAssertMyRegisterNotUsed(0, "first"); // wak

  MachineBasicBlock::iterator MII = MBB->begin();

  // Add live-in registers as live.
  // wak: live-inレジスタをliveにする．
  for (MachineBasicBlock::livein_iterator I = MBB->livein_begin(),
         E = MBB->livein_end(); I != E; ++I) {
    if (Allocatable.test(*I))
      definePhysReg(MII, *I, regReserved);

    wakAssertMyRegisterNotUsed(0, "6"); // wak
  }

  SmallVector<unsigned, 8> VirtDead;
  SmallVector<MachineInstr*, 32> Coalesced; // wak: レジスタ合併

  // wak: @todo: regFree, regReservedはどんないみ？
  // wak: ここからレジスタ割付けを行う．
  //      各命令のレジスタを3回ずつスキャンして割付けを行う
  //        1. 準備
  //        2. 使用点のレジスタを割り付ける
  //        3. 定義点のレジスタを割り付ける
  // wak: 割付り中の命令の生存区間の合併しているかは，CopyDstを見ればよさそう
  //      生存区間の合併（coalesce）とは...
  //      例えば， a = b というコピー命令があったとき，
  //      aとbに同じレジスタを割り付けて，コピー命令を不要にする方法．
  // 
  // Otherwise, sequentially allocate each instruction in the MBB.
  while (MII != MBB->end()) {
    // wak: MBBの各命令について...

    MachineInstr *MI = MII++;
    const TargetInstrDesc &TID = MI->getDesc();

    // wak: この命令までのレジスタの使用状況を表示？
    DEBUG({
        dbgs() << "\n>> " << *MI << "Regs:";
        for (unsigned Reg = 1, E = TRI->getNumRegs(); Reg != E; ++Reg) {
          if (PhysRegState[Reg] == regDisabled) continue;
          dbgs() << " " << TRI->getName(Reg);
          switch(PhysRegState[Reg]) {
          case regFree:
            break;
          case regReserved:
            dbgs() << "*";
            break;
          default:
            dbgs() << '=' << PrintReg(PhysRegState[Reg]);
            if (LiveVirtRegs[PhysRegState[Reg]].Dirty)
              dbgs() << "*";
            assert(LiveVirtRegs[PhysRegState[Reg]].PhysReg == Reg &&
                   "Bad inverse map");
            break;
          }
        }
        dbgs() << '\n';
        // Check that LiveVirtRegs is the inverse.
        for (LiveRegMap::iterator i = LiveVirtRegs.begin(),
             e = LiveVirtRegs.end(); i != e; ++i) {
           assert(TargetRegisterInfo::isVirtualRegister(i->first) &&
                  "Bad map key");
           assert(TargetRegisterInfo::isPhysicalRegister(i->second.PhysReg) &&
                  "Bad map value");
           assert(PhysRegState[i->second.PhysReg] == i->first &&
                  "Bad inverse map");
        }
      });

    // Debug values are not allowed to change codegen in any way.
    if (MI->isDebugValue()) {
      bool ScanDbgValue = true;
      while (ScanDbgValue) {
        ScanDbgValue = false;
        for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
          MachineOperand &MO = MI->getOperand(i);
          if (!MO.isReg()) continue;
          unsigned Reg = MO.getReg();
          if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
          LiveDbgValueMap[Reg] = MI;
          LiveRegMap::iterator LRI = LiveVirtRegs.find(Reg);
          if (LRI != LiveVirtRegs.end())
            setPhysReg(MI, i, LRI->second.PhysReg);
          else {
            int SS = StackSlotForVirtReg[Reg];
            if (SS == -1) {
              // We can't allocate a physreg for a DebugValue, sorry!
              DEBUG(dbgs() << "Unable to allocate vreg used by DBG_VALUE");
              MO.setReg(0);
            }
            else {
              // Modify DBG_VALUE now that the value is in a spill slot.
              int64_t Offset = MI->getOperand(1).getImm();
              const MDNode *MDPtr =
                MI->getOperand(MI->getNumOperands()-1).getMetadata();
              DebugLoc DL = MI->getDebugLoc();
              if (MachineInstr *NewDV =
                  TII->emitFrameIndexDebugValue(*MF, SS, Offset, MDPtr, DL)) {
                DEBUG(dbgs() << "Modifying debug info due to spill:" <<
                      "\t" << *MI);
                MachineBasicBlock *MBB = MI->getParent();
                MBB->insert(MBB->erase(MI), NewDV);
                // Scan NewDV operands from the beginning.
                MI = NewDV;
                ScanDbgValue = true;
                break;
              } else {
                // We can't allocate a physreg for a DebugValue; sorry!
                DEBUG(dbgs() << "Unable to allocate vreg used by DBG_VALUE");
                MO.setReg(0);
              }
            }
          }
          wakAssertMyRegisterNotUsed(0, "7"); // wak
        }
      }

      wakAssertMyRegisterNotUsed(0, "8"); // wak

      // Next instruction.
      continue;
    }

    // If this is a copy, we may be able to coalesce.
    unsigned CopySrc = 0, CopyDst = 0, CopySrcSub = 0, CopyDstSub = 0;
    // wak: getOpcode()がTargetOpcode::COPYの時
    // wak: @todo TargetOpcode::COPYとは何か？どんな命令か？
    if (MI->isCopy()) {
      CopyDst = MI->getOperand(0).getReg();
      CopySrc = MI->getOperand(1).getReg();
      CopyDstSub = MI->getOperand(0).getSubReg();
      CopySrcSub = MI->getOperand(1).getSubReg();
    }

    // Track registers used by instruction.
    UsedInInstr.reset();

    // wak: 一回目のスキャン．物理レジスタの使用点と早期破壊オペランドの使用点をマークする．
    //      仮想レジスタを用いる最後のオペランドを探す？
    // First scan.
    // Mark physreg uses and early clobbers as used.
    // Find the end of the virtreg operands
    unsigned VirtOpEnd = 0;        // wak: 最後に仮想レジスタが出てきたところ
    bool hasTiedOps = false;
    bool hasEarlyClobbers = false; // wak: 早期破壊オペランド: GCCのインラインasmの'&'
    bool hasPartialRedefs = false; // wak: レジスタの一部を再定義する？
    bool hasPhysDefs = false;      // wak: この命令内に，物理レジスタの定義点がある
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg()) continue;
      unsigned Reg = MO.getReg();
      if (!Reg) continue;
      if (TargetRegisterInfo::isVirtualRegister(Reg)) {
        VirtOpEnd = i+1;
        if (MO.isUse()) {
          hasTiedOps = hasTiedOps ||
                                TID.getOperandConstraint(i, TOI::TIED_TO) != -1;
        } else {
          if (MO.isEarlyClobber())
            hasEarlyClobbers = true;
          if (MO.getSubReg() && MI->readsVirtualRegister(Reg))
            hasPartialRedefs = true;
        }
        continue;
      }
      if (!Allocatable.test(Reg)) continue;
      if (MO.isUse()) {
        usePhysReg(MO);
      } else if (MO.isEarlyClobber()) {
        // wak: GCC inline 制約修飾子 '&'
        errs() << "wak: warn: EarlyClobber found\n";
        // wak: 暗黙的に定義されるか，MBBの中で(?)もう使われなければ，regFree．
        //      そうでなければ，regReserved
        definePhysReg(MI, Reg, (MO.isImplicit() || MO.isDead()) ?
                               regFree : regReserved);
        hasEarlyClobbers = true;
      } else
        hasPhysDefs = true;
    }

    // The instruction may have virtual register operands that must be allocated
    // the same register at use-time and def-time: early clobbers and tied
    // operands. If there are also physical defs, these registers must avoid
    // both physical defs and uses, making them more constrained than normal
    // operands.
    // Similarly, if there are multiple defs and tied operands, we must make
    // sure the same register is allocated to uses and defs.
    // We didn't detect inline asm tied operands above, so just make this extra
    // pass for all inline asm.
    // wak: 命令の中には，もしかすると定義点と使用点で同じレジスタを使わなければ
    //      ならないものがあるかもしれない（早期破壊とtiedオペランド）．これらの
    //      レジスタは，物理定義／使用の両方共において，通常のオペランドとして使
    //      われないように制約を付けなければならない．
    // 
    // wak: 同様に，複数の定義とtiedオペランドがあるならば，同じレジスタを割り付
    //      けなければならない．我々は，inline asmのtiedオペランドを上記のように
    //      検出しない，so just make this extra pass for all inline asm．
    if (MI->isInlineAsm() || hasEarlyClobbers || hasPartialRedefs ||
        (hasTiedOps && (hasPhysDefs || TID.getNumDefs() > 1))) {
      handleThroughOperands(MI, VirtDead);
      // Don't attempt coalescing when we have funny stuff going on.
      CopyDst = 0;
      // Pretend we have early clobbers so the use operands get marked below.
      // This is not necessary for the common case of a single tied use.
      hasEarlyClobbers = true;
    }

    // Second scan.
    // Allocate virtreg uses.
    // wak: 仮想レジスタの使用点を割り付ける
    for (unsigned i = 0; i != VirtOpEnd; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg()) continue;
      unsigned Reg = MO.getReg();
      if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
      if (MO.isUse()) {         // wak: 使用点？
        // wak: なぜに'reload'？Spillされているかもしれないから？
        LiveRegMap::iterator LRI = reloadVirtReg(MI, i, Reg, CopyDst);

        // wak: 「使用点」なので，どこぞで定義された物理レジスタがある（はず）．
        unsigned PhysReg = LRI->second.PhysReg;

        // wak: コピー元が使用点（仮想レジスタor物理レジスタ）と
        //      同じであれば: CopySrcに物理レジスタを代入する
        //      異なれば: 合併中止
        // wak: @todo CopySrcと使用点の仮想レジスタが同じ場合があるのか？
        CopySrc = (CopySrc == Reg || CopySrc == PhysReg) ? PhysReg : 0;
        if (setPhysReg(MI, i, PhysReg))
          killVirtReg(LRI);     // wak: 割り付けたから，VRegをkill
      }
    }

    // MachineFunctionのレジスタ情報に物理レジスタを使ってるよマークを付ける
    MRI->addPhysRegsUsed(UsedInInstr); // wak: MRI |= UsedInInstr

    // Track registers defined by instruction - early clobbers and tied uses at
    // this point.
    UsedInInstr.reset();
    if (hasEarlyClobbers) {
      for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
        MachineOperand &MO = MI->getOperand(i);
        if (!MO.isReg()) continue;
        unsigned Reg = MO.getReg();
        if (!Reg || !TargetRegisterInfo::isPhysicalRegister(Reg)) continue;
        // Look for physreg defs and tied uses.
        if (!MO.isDef() && !MI->isRegTiedToDefOperand(i)) continue;
        UsedInInstr.set(Reg);
        for (const unsigned *AS = TRI->getAliasSet(Reg); *AS; ++AS)
          UsedInInstr.set(*AS);
      }
    }

    unsigned DefOpEnd = MI->getNumOperands();
    // wak: call命令？
    if (TID.isCall()) {
      // Spill all virtregs before a call. This serves two purposes: 1. If an
      // exception is thrown, the landing pad is going to expect to find
      // registers in their spill slots, and 2. we don't have to wade through
      // all the <imp-def> operands on the call instruction.

      // wak: callの前に，すべてのレジスタをspillする．目的は，以下の二つ．
      //      1. もし例外が発生したら，....
      //      2. 我々は，call命令のすべての<imp-def> operandsを苦労してかき分けていく必要はない．
      DefOpEnd = VirtOpEnd;
      DEBUG(dbgs() << "  Spilling remaining registers before call.\n");
      spillAll(MI);

      // The imp-defs are skipped below, but we still need to mark those
      // registers as used by the function.
      SkippedInstrs.insert(&TID);
    }

    // Third scan.
    // Allocate defs and collect dead defs.
    // wak: 定義点のレジスタを割り付けて，deadな定義を集める
    int wakCount = 0;
    for (unsigned i = 0; i != DefOpEnd; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || !MO.isDef() || !MO.getReg() || MO.isEarlyClobber())
        continue;
      unsigned Reg = MO.getReg();

      // wak: 物理レジスタが決まっている場合
      if (TargetRegisterInfo::isPhysicalRegister(Reg)) {
        if (!Allocatable.test(Reg)) continue;
        definePhysReg(MI, Reg, (MO.isImplicit() || MO.isDead()) ?
                               regFree : regReserved);
        continue;
      }

      // wak: 選んで決める場合

      // wak: ECC関連の場合，レジスタ強制のために準備
      // wak: WARNING!! See changes to llvm-2.9/lib/Target/X86/X86InstrInfo.cpp
      if (OptWakRegAlloc && MI->eccComputeInfo) {
        // すでに物理レジスタが割り付けられていたら，なにもしない
        if (!TargetRegisterInfo::isVirtualRegister(MO.getReg()))
          goto wak_alloc;

        // @todo: キャストした時はどうなるか？

        if (MI->eccComputeInfo == EccComputeInfo::LoadVariable)
          wakRegisterLotate();  // 使うレジスタを変更する


        unsigned wantToAllocateReg = wakRegisterForBytes();

        if (MI->eccComputeInfo == EccComputeInfo::ComputeEccRelatedTwoAddr) {
          // TwoAddressInstructionPassによって，
          //                             + (1) A = B
          //    A = B op C   ==trans==>  |
          //                             + (2) A = A op C
          // このように変換される場合がある．
          // この場合は，(1)のAとBに同じレジスタを割り付けておく．
          // すなわち，
          //                             + (1) RegA = RegA
          //    A = B op C   ==trans==>  |
          //                             + (2) RegA = RegA op RegB
          // な感じにする．
          // このRegAはCOPYのオペランドから探し，RegBはwakRegisterForBytes()で決める

          unsigned TAInstRegA = MI->getOperand(1).getReg();
          if (!TargetRegisterInfo::isPhysicalRegister(TAInstRegA)) {
            if (TargetRegisterInfo::isPhysicalRegister(wakForceVRegToPRegMap[TAInstRegA]))
              TAInstRegA = wakForceVRegToPRegMap[TAInstRegA];
            else {
              // 計画破綻
              errs() << "wak: error: Failed to determine physical register for "
                     << PrintReg(Reg, TRI) << "(TAInstRegA = "
                     << PrintReg(TAInstRegA, TRI) << ")\n";
              llvm_unreachable("failed to allocate my register");
            }
          }
          wantToAllocateReg = TAInstRegA;

          // Two...によって，2アドレス命令向けに命令の変換（分割）がされる．
          // これが，命令のオペランド順が都合の悪いように変換されるため，
          // このタイミングでも，強制レジスタを交換する．
          //
          // 例えば，次の3個の命令は，
          //
          //   (1) %vreg3 = LoadVariable 'a'  # use R14
          //   (2) %vreg6 = LoadVariable 'b'  # use R15
          //   (3) [ECC R(Rel)] %vreg7<def> = SUB32rr %vreg3, %vreg6<kill>, %EFLAGS<imp-def,dead>
          //                    ; GR32:%vreg7,%vreg3,%vreg6 dbg:add3_test.c:30:2
          //                    ## ECC related at InstrEmitter.cpp:710 <= SelectionDAGBuilder.cpp:2445
          //
          // 「%vreg7 = a - b」という処理を行う．この(3)の命令が，以下の2個の命令に変換される．
          //
          //   prepend:
          //     [ECC R(RelTA)] %vreg7<def> = COPY %vreg3
          //                    ; GR32:%vreg7,%vreg3 dbg:add3_test.c:30:2
          //                    ## ECC related at TwoAddressInstructionPass.cpp:1189 <= ?:0
          //   rewrite to:
          //     [ECC R(Rel)]   %vreg7<def> = SUB32rr %vreg7, %vreg6<kill>, %EFLAGS<imp-def,dead>
          //                    ; GR32:%vreg7,%vreg6 dbg:add3_test.c:30:2
          //                    ## ECC related at InstrEmitter.cpp:710 <= SelectionDAGBuilder.cpp:2445，
          //
          // すなわち，
          //   %vreg7 = a             # use R14
          //   %vreg7 = %vreg7 - b    # So, use R14
          //
          // となり，計算結果を入れるレジスタがR14になる．
          // したがって，単純にLoadVariableでレジスタを交換していくアルゴリズムでは，
          // 次の値を格納するレジスタはR14になるため，衝突し，上書きされてしまう．
          // この問題を回避するために，この場面でも使うレジスタを交換する．
          //
          // メモ:
          //   CommuteInstruction()してくれると，交換しなくてもよいが，
          //   する場合としない場合があるので，レジスタ強制時はしないように変更した
          //   そもそも，killやdeadの情報が壊れているので正しくできていないかもしれない．
          //   see. lib/CodeGen/TwoAddressInstructionPass.cpp:901
          wakRegisterLotate();  // 使うレジスタを変更する
        } else {
          const TargetRegisterClass *RC = MRI->getRegClass(MO.getReg());
          if (OptWakDebugRegAlloc)
            errs() << "wak: Operand size is " << RC->getSize() << "bytes\n";
          wantToAllocateReg = wakRegisterForBytes(RC->getSize());
        }

        wakCount++;
        assert(wakCount == 1 && "定義点オペランドに2回以上，Myレジスタを割付けようとしている．");
        mustUseWakPhysReg = wantToAllocateReg;
        CopySrc = wantToAllocateReg;   // wak: HintにMyレジスタを指定して，それを割り付けてもらう．それ以外のケースもある．
        CopyDst = 0; // cancel coalescing;
      }
      wak_alloc:

      if (mustUseWakPhysReg && OptWakDebugRegAlloc)
        errs() << "wak: I want allocate " << PrintReg(Reg, TRI)
               << " to " << PrintReg(mustUseWakPhysReg, TRI) << "\n";

      LiveRegMap::iterator LRI = defineVirtReg(MI, i, Reg, CopySrc);
      unsigned PhysReg = LRI->second.PhysReg;

      if (OptWakRegAlloc && MI->eccComputeInfo) {
        if (PhysReg != mustUseWakPhysReg) {
          // wak: レジスタ強制失敗
          errs() << "wak: error: I want allocate " << PrintReg(Reg, TRI)
                 << " to " << PrintReg(mustUseWakPhysReg, TRI)
                 << " but " << PrintReg(PhysReg, TRI) << " was allocated.\n";
          llvm_unreachable("failed to allocate my register");
        }
        wakForceVRegToPRegMap.insert(std::make_pair(Reg, PhysReg)); // 割付けたレジスタを記憶する

        mustUseWakPhysReg = 0;
        if (MI->eccComputeInfo == EccComputeInfo::ComputeEccRelatedEnd) {
          // 一連のECC値計算が終わった．
          wakResetRegisterLotate();
        }
      }

      if (setPhysReg(MI, i, PhysReg)) {
        VirtDead.push_back(Reg);
        CopyDst = 0; // cancel coalescing;
      } else
        CopyDst = (CopyDst == Reg || CopyDst == PhysReg) ? PhysReg : 0;
    }

    // Kill dead defs after the scan to ensure that multiple defs of the same
    // register are allocated identically. We didn't need to do this for uses
    // because we are crerating our own kill flags, and they are always at the
    // last use.
    // wak: 仮想レジスタでdeadしたのをkillに設定する
    for (unsigned i = 0, e = VirtDead.size(); i != e; ++i)
      killVirtReg(VirtDead[i]);
    VirtDead.clear();           // wak: killしたから，お掃除

    MRI->addPhysRegsUsed(UsedInInstr); // wak: MRI |= UsedInInstr

    if (CopyDst && CopyDst == CopySrc && CopyDstSub == CopySrcSub) {
      DEBUG(dbgs() << "-- coalescing: " << *MI);
      Coalesced.push_back(MI);
    } else {
      DEBUG(dbgs() << "<< " << *MI);
    }

    DEBUG(dbgs() << "\n--\n\n");
    // wak: 次の命令へ
  }
  // wak: レジスタ割付けループ終了

  // Spill all physical registers holding virtual registers now.
  DEBUG(dbgs() << "Spilling live registers at end of block.\n");
  spillAll(MBB->getFirstTerminator());

  // Erase all the coalesced copies. We are delaying it until now because
  // LiveVirtRegs might refer to the instrs.
  for (unsigned i = 0, e = Coalesced.size(); i != e; ++i)
    MBB->erase(Coalesced[i]);
  NumCopies += Coalesced.size();

  DEBUG(MBB->dump());
  wakAssertMyRegisterNotUsed(0, "end of AllocateBasicBlock()"); // wak
}

/// runOnMachineFunction - Register allocate the whole function
///
bool RAFast::runOnMachineFunction(MachineFunction &Fn) {
  DEBUG(dbgs() << "********** FAST REGISTER ALLOCATION **********\n"
               << "********** Function: "
               << ((Value*)Fn.getFunction())->getName() << '\n');
  MF = &Fn;
  MRI = &MF->getRegInfo();
  TM = &Fn.getTarget();
  TRI = TM->getRegisterInfo();
  TII = TM->getInstrInfo();

  UsedInInstr.resize(TRI->getNumRegs());
  Allocatable = TRI->getAllocatableSet(*MF);

  // initialize the virtual->physical register map to have a 'null'
  // mapping for all virtual registers
  StackSlotForVirtReg.resize(MRI->getNumVirtRegs());

  // Loop over all of the basic blocks, eliminating virtual register references
  for (MachineFunction::iterator MBBi = Fn.begin(), MBBe = Fn.end();
       MBBi != MBBe; ++MBBi) {
    MBB = &*MBBi;
    AllocateBasicBlock();
  }

  // wak: ここまでで，割付け自体は終わってるっぽい
  // dbgs() << "wak dump\n";
  // MF->dump();

  // Make sure the set of used physregs is closed under subreg operations.
  // wak: RAXがclosedなら，EAXやAXもclosedにする？
  // wak: @todo: MachineRegisterInfoとは何か．（コンパイル途中のレジスタ使用状況か？）
  MRI->closePhysRegsUsed(*TRI);

  // Add the clobber lists for all the instructions we skipped earlier.
  for (SmallPtrSet<const TargetInstrDesc*, 4>::const_iterator
         I = SkippedInstrs.begin(), E = SkippedInstrs.end(); I != E; ++I) {
    if (const unsigned *Defs = (*I)->getImplicitDefs())
      while (*Defs) {
        // wak: 指定したレジスタを関数で使用しているとマークする．
        // wak: このメソッドは，レジスタ割付けが終わってから呼び出す
        MRI->setPhysRegUsed(*Defs++);
      }
  }

  SkippedInstrs.clear();
  StackSlotForVirtReg.clear();
  LiveDbgValueMap.clear();
  return true;
}

FunctionPass *llvm::createFastRegisterAllocator() {
  return new RAFast();
}
