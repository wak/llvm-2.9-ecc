#include "llvm/Support/CommandLine.h"
#include "llvm/WakOptions.h"

namespace llvm {
#define DEF_SWITCH(id, name, description) cl::opt<bool> id (name, cl::Hidden, cl::desc(description))

// ECC用
DEF_SWITCH(OptWakRegAlloc, "wak-reg-alloc", "レジスタ強制する");
DEF_SWITCH(OptWakInsertEccPass , "wak-insert-ecc"      , "ECC用の命令を追加する（IR実装: WakInsertEccPass）");
DEF_SWITCH(OptWakInsertEccStore, "wak-insert-ecc-store", "[storeのみ]ECC用の命令を追加する（IR実装: WakInsertEccPass）");
DEF_SWITCH(OptWakInsertEccLoad , "wak-insert-ecc-load" , "[loadのみ]ECC用の命令を追加する（IR実装: WakInsertEccPass）");

// デバッグ用
DEF_SWITCH(OptEccIR, "wak-print-ecc", "命令ダンプにECC情報を付ける");
DEF_SWITCH(OptWakColor, "wak-color", "カラー表示する");
DEF_SWITCH(OptWakDebugInsertEcc, "wak-debug-insert-ecc", "ECC命令を追加するパスで，どの命令を処理しているかを詳細に表示する");
DEF_SWITCH(OptWakDebugRegAlloc , "wak-debug-reg-alloc" , "レジスタ割付けを行うパスで，処理情報を詳細に表示する");

// 過去の遺物
DEF_SWITCH(OptWakDebugAroundTargetMachine   , "wak-debug-target-machine", "LLVMTargetMachine.cpp関連のデバッグ情報を表示");
DEF_SWITCH(OptWakDebugAroundMBB             , "wak-debug-around-mbb"    , "MachineBasicBlock関連のデバッグ情報を出す(命令選択付近)");
DEF_SWITCH(OptWakDebugSDB                   , "wak-debug-sdb "          , "SelectionDAGBuilder.cppのデバッグ情報");
DEF_SWITCH(OptWakDebugISel                  , "wak-debug-isel"          , "命令選択のデバッグ情報");
DEF_SWITCH(OptWakDebugPass                  , "wak-debug-pass"          , "パス関連のデバッグ情報");
DEF_SWITCH(OptWakDebugEmitter               , "wak-debug-emitter"       , "asm,binaryエミッタ関連");
DEF_SWITCH(OptWakDebugSay                   , "wak-debug-say"           , "誰が呼ばれたか自己主張");
DEF_SWITCH(OptWakAddFunctionPass            , "wak-add-FP"              , "FunctionPass WakTestをパスに追加");
DEF_SWITCH(OptWakAddMachineFunctionPass     , "wak-add-MFP"             , "MachineFunctionPass WakEccCheckPassをパスに追加");
DEF_SWITCH(OptWakAddDuplicateInsnTestPass   , "wak-add-DIT"             , "ECC値複製用のを命令を挿入するパスを追加 (WakDuplicateInsnTestPass)");
DEF_SWITCH(OptWakAddX86DuplicateInsnTestPass, "wak-add-DIT-MFP"         , "MachineFunctionPass WakDuplicateTestPassをパスに追加");
DEF_SWITCH(OptWakHammingEccPass             , "wak-add-hamming-FP"      , "MachineFunctionPass WakDuplicateTestPassをパスに追加");

#undef DEF_SWITCH
}
