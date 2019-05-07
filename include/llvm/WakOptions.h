#ifndef LLVM_WAKOPTIONS_H
#define LLVM_WAKOPTIONS_H

#include "llvm/Support/CommandLine.h" // for cl::opt
#include "llvm/Support/raw_ostream.h" // for errs()

// #include "llvm/WakOptions.h"

namespace llvm {
  extern cl::opt<bool> OptWakDebugAroundMBB;
  extern cl::opt<bool> OptWakDebugAroundTargetMachine;
  extern cl::opt<bool> OptWakDebugSDB;
  extern cl::opt<bool> OptWakDebugISel;
  extern cl::opt<bool> OptWakDebugPass;
  extern cl::opt<bool> OptWakDebugEmitter;
  extern cl::opt<bool> OptWakDebugSay;
  extern cl::opt<bool> OptWakAddFunctionPass;
  extern cl::opt<bool> OptWakAddMachineFunctionPass;
}

#endif
