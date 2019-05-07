#include "llvm/Support/CommandLine.h"
#include "llvm/WakOptions.h"

namespace llvm {
cl::opt<bool>
OptEccIR("wak-print-ecc", cl::Hidden,
         cl::desc("命令ダンプにECC情報を付ける"));


cl::opt<bool>
OptWakDebugAroundTargetMachine("wak-debug-target-machine", cl::Hidden,
                               cl::desc("LLVMTargetMachine.cpp関連のデバッグ情報を表示"));


cl::opt<bool>
OptWakDebugAroundMBB("wak-debug-around-mbb", cl::Hidden,
                    cl::desc("MachineBasicBlock関連のデバッグ情報を出す(命令選択付近)"));

cl::opt<bool>
OptWakDebugSDB("wak-debug-sdb", cl::Hidden,
               cl::desc("SelectionDAGBuilder.cppのデバッグ情報"));


cl::opt<bool>
OptWakDebugISel("wak-debug-isel", cl::Hidden,
                cl::desc("命令選択のデバッグ情報"));

cl::opt<bool>
OptWakDebugPass("wak-debug-pass", cl::Hidden,
                cl::desc("パス関連のデバッグ情報"));

cl::opt<bool>
OptWakDebugEmitter("wak-debug-emitter", cl::Hidden,
                   cl::desc("asm,binaryエミッタ関連"));

cl::opt<bool>
OptWakDebugSay("wak-debug-say", cl::Hidden,
                   cl::desc("誰が呼ばれたか自己主張"));

cl::opt<bool>
OptWakAddFunctionPass("wak-add-FP", cl::Hidden,
                      cl::desc("FunctionPass WakTestをパスに追加"));

cl::opt<bool>
OptWakAddMachineFunctionPass("wak-add-MFP", cl::Hidden,
                             cl::desc("MachineFunctionPass WakEccCheckPassをパスに追加"));


cl::opt<bool>
OptWakInsertEccPass("wak-insert-ecc", cl::Hidden,
                    cl::desc("ECC用の命令を追加する（IR実装: WakInsertEccPass）"));
cl::opt<bool>
OptWakInsertEccStore("wak-insert-ecc-store", cl::Hidden,
                     cl::desc("[storeのみ]ECC用の命令を追加する（IR実装: WakInsertEccPass）"));
cl::opt<bool>
OptWakInsertEccLoad("wak-insert-ecc-load", cl::Hidden,
                    cl::desc("[loadのみ]ECC用の命令を追加する（IR実装: WakInsertEccPass）"));


cl::opt<bool>
OptWakAddDuplicateInsnTestPass("wak-add-DIT", cl::Hidden,
                               cl::desc("ECC値複製用のを命令を挿入するパスを追加 (WakDuplicateInsnTestPass)"));

cl::opt<bool>
OptWakAddX86DuplicateInsnTestPass("wak-add-DIT-MFP", cl::Hidden,
                                  cl::desc("MachineFunctionPass WakDuplicateTestPassをパスに追加"));


cl::opt<bool>
OptWakHammingEccPass("wak-add-hamming-FP", cl::Hidden,
                     cl::desc("MachineFunctionPass WakDuplicateTestPassをパスに追加"));


cl::opt<bool> OptWakRegAlloc("wak-reg-alloc", cl::Hidden,cl::desc("レジスタ強制する"));

cl::opt<bool> OptWakColor("wak-color", cl::Hidden,cl::desc("カラー表示する"));

}
