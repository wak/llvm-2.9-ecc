#ifndef LLVM_WAKOPTIONS_H
#define LLVM_WAKOPTIONS_H

#include "llvm/Support/CommandLine.h" // for cl::opt
#include "llvm/Support/raw_ostream.h" // for errs()

// #include "llvm/WakOptions.h"
// Impl: lib/Support/WakOptions.cpp

namespace llvm {
  extern cl::opt<bool> OptEccIR;
  extern cl::opt<bool> OptWakDebugAroundMBB;
  extern cl::opt<bool> OptWakDebugAroundTargetMachine;
  extern cl::opt<bool> OptWakDebugSDB;
  extern cl::opt<bool> OptWakDebugISel;
  extern cl::opt<bool> OptWakDebugPass;
  extern cl::opt<bool> OptWakDebugEmitter;
  extern cl::opt<bool> OptWakDebugSay;
  extern cl::opt<bool> OptWakAddFunctionPass;
  extern cl::opt<bool> OptWakAddMachineFunctionPass;
  extern cl::opt<bool> OptWakAddDuplicateInsnTestPass;
  extern cl::opt<bool> OptWakAddX86DuplicateInsnTestPass;
  extern cl::opt<bool> OptWakHammingEccPass;

  /* ECC挿入（IR実装） */
  extern cl::opt<bool> OptWakInsertEccPass;
  extern cl::opt<bool> OptWakInsertEccStore;
  extern cl::opt<bool> OptWakInsertEccLoad;

  /* レジスタ強制 */
  extern cl::opt<bool> OptWakRegAlloc;

  /* 開発補助 */
  extern cl::opt<bool> OptWakColor;
  extern cl::opt<bool> OptWakDebugInsertEcc;
  extern cl::opt<bool> OptWakDebugRegAlloc;
}

#endif
