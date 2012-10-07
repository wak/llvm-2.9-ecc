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
#include "llvm/WakOptions.h"

using namespace llvm;

namespace {

  class WakInsertEcc : public FunctionPass {
  public:
    static char ID;
    Function *FUNC;

    WakInsertEcc() : FunctionPass(ID) {
      initializeWakInsertEccPass(*PassRegistry::getPassRegistry());
    }

  private:

    unsigned int getPointerSizeInBits() const {
      switch (FUNC->getParent()->getPointerSize()) {
      case Module::Pointer32:
        return 32;
      case Module::Pointer64:
        return 64;
      case Module::AnyPointerSize:
        report_fatal_error("getPointerSizeInBits(): unknown pointer size\n");
      default:
        assert(0 && "unknown pointer type");
      }
      assert(0 && "No here");
    }

    unsigned int getTypeSizeInBits(const Type *type) {
      unsigned size = 0;

      if (const PointerType *ptrTy = dyn_cast<PointerType>(type)) {
        const Type *containedTy = ptrTy->getContainedType(0);
        if (containedTy->isPointerTy())
          // (1) int *p;  => sizeof(*p) = sizeof(int)
          size = getPointerSizeInBits();
        else
          // (2) int **p; => sizeof(*p) = sizeof(int *)
          size = containedTy->getPrimitiveSizeInBits();
      } else {
        // (3) int v; => sizeof(v) = sizeof(int)
        size = type->getPrimitiveSizeInBits();
      }
      return size;
    }

    // 普通のLOAD命令を，次の処理を行う関数への呼び出しに変更する
    //   (1) 保護されたメモリ領域から値を取得する
    //   (2) 取得した値に誤りがないか，誤り検出を行う
    //   IF 誤りなし
    //     (3) 値を戻り値として返す
    //   ELSE 
    //     IF 訂正可能？
    //       (4) 誤り訂正
    //       (5) 値を戻り値として返す
    //     ELSE
    //       (6) どうするか判断を仰ぐ（ユーザ定義ハンドラの呼び出し）
    //
    //   関数一覧
    //     数値用（汎用）: uint64_t ecc_load_integer(void *app, uint64_t bits)
    //     ポインタ用    : void *ecc_load_pointer(void *app, uint64_t bits) 数値用でも代用できるかも
    void insertLoadValueInstruction(BasicBlock::iterator &insn_iter) {
      LoadInst *loadInst = dyn_cast<LoadInst>(insn_iter);
      const Type *dataType = loadInst->getType();

      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(loadInst->getParent(), ++insn_iter);

      // ロード関数を取得
      // @todo 型によって関数を変える
      Function *load_function = FUNC->getParent()->getFunction("ecc_load_integer");
      if (load_function == NULL)
        report_fatal_error("ECC load function not found (ecc_load_*)");

      // ロードするサイズを取得
      unsigned int size = getTypeSizeInBits(dataType);

      // 関数呼出し命令挿入
      // load_function((void *) pointerOperand, target_size_in_bits);
      Instruction *data = 
        B.CreateCall2(load_function,
                      B.CreatePointerCast(loadInst->getPointerOperand(), B.getInt8PtrTy()),
                      B.getInt64(size));

      // 1,8,16,32,64,128
      // uint64_tを本来の型にキャスト
      Value *cast = data;
      if (dataType->isPointerTy()) {
        cast = B.CreateIntToPtr(data, dataType);
      } else if (dataType->isIntegerTy()) {
        if (dataType->getPrimitiveSizeInBits() > 64)
          report_fatal_error("ECC load: over 64bit load not supported");
        else if (dataType->getPrimitiveSizeInBits() < 64)
          cast = B.CreateTrunc(data, dataType);
      }

      errs() << "cast: orig=";
      dataType->print(errs());
      errs() << ", from=";
      data->getType()->print(errs());
      errs() << ", to=";
      cast->getType()->print(errs());
      errs() << "\n";

      data->setDebugLoc(loadInst->getDebugLoc());

      // 本来のLOADへのすべての参照を入れ替える
      loadInst->replaceAllUsesWith(cast);
    }

    // アプリケーションのデータに対応する，ECCデータ領域を更新するための命令を挿入する
    void insertUpdateEccDataInstruction(BasicBlock::iterator &insn_iter) {
      StoreInst *storeInst = dyn_cast<StoreInst>(insn_iter);

      // @todo: ここでiteratorを++しておくので，呼び出し元もちゃんといけるか？
      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(storeInst->getParent(), ++insn_iter);

      // 更新関数を取得
      Function *update_bits_function = FUNC->getParent()->getFunction("ecc_update_ecc_data_bits");
      if (update_bits_function == NULL)
        report_fatal_error("ECC update function not found (ecc_update_ecc_data_bits)");

      // 更新される（された）サイズを取得
      unsigned int size = getTypeSizeInBits(storeInst->getPointerOperand()->getType());

      // 関数呼出し命令挿入
      // ecc_update_ecc_data_bits((void *) pointerOperand, target_size_in_bits);
      Instruction *call =
        B.CreateCall2(update_bits_function,
                      B.CreatePointerCast(storeInst->getPointerOperand(), B.getInt8PtrTy()),
                      B.getInt64(size));
      call->setDebugLoc(storeInst->getDebugLoc());
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

    // 単純に，ポインタがいくつ挟まっているかを数える．
    int countPointerWraps(const Type *type) {
      int count;

      errs() << "(count: ";
      for (count = 0; type->isPointerTy(); count++) {
        if (count != 0)
          errs() << "->";
        type = type->getContainedType(0);
        type->print(errs());
        if (const ArrayType *aryTy = dyn_cast<ArrayType>(type)) {
          type = aryTy->getContainedType(0);
          errs() << "=";
          type->print(errs());
        }
      }
      errs() << ") ";
      return count;
    }

    Value *checkPointerHasEccValue(Value *pointerOperand) {
      assert(isPointerOperation(pointerOperand, false));

      // ポインタルール
      //   ポインタ変数に_ecc_(level = 0)を指定した場合，
      //   実体とlevel個分のポインタを保護する．
      //   level = -1の時は，ポインタを辿る全てを保護する．
      //
      //   - 例1: int **p __attribute__((ecc(0)));
      //     - p   = ... なし
      //     - *p  = ... なし
      //     - **p = ... 保護
      //   - 例1: int **p __attribute__((ecc(1)));
      //     - p   = ... なし
      //     - *p  = ... 保護
      //     - **p = ... 保護
      //
      // アルゴリズム（代入時）
      //   1. 祖先を辿り，ECC付きかチェックする
      //   2. levelを取得する
      //   3. 代入する型をチェックし，ポインタの階層がlevel内かチェックする
      //
      // アルゴリズム（参照時）
      //   - 代入時アルゴリズムでOK?

      // isPointerOperation()でポインタであることはチェックしてある
      Value *currentValue = pointerOperand;
      while (true) {
        if (currentValue->isecc)
          break;

        if (LoadInst *loadInst = dyn_cast<LoadInst>(currentValue)) {
          // ポインタを剥がす場合は，load命令が入る
          currentValue = loadInst->getPointerOperand();
        } else if (GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(currentValue)) {
          // 配列の要素や構造体メンバを参照する場合は，GEP命令が入る
          currentValue = gepInst->getPointerOperand();
        } else  if (ConstantExpr *expr = dyn_cast<ConstantExpr>(currentValue)) {
          if (expr->getOpcode() == Instruction::GetElementPtr &&
              expr->getNumOperands() > 0) {
            currentValue = expr->getOperand(0);
          } else {
            errs() << "[Unsupported ConstantExpr: " << expr->getOpcode() << "  INST: " << Instruction::GetElementPtr << "]\n";
          }
        } else {
          break;
        }
      }

      if (currentValue && !currentValue->isecc)
        return NULL;

      int protectRefLevel = currentValue->ecc_reference_level;
      int toWraps         = countPointerWraps(pointerOperand->getType());
      int rootWraps       = countPointerWraps(currentValue->getType());
      assert(toWraps <= rootWraps);

      errs() << "RefLevel = " << protectRefLevel << ", root = " << rootWraps << ", to = " << toWraps;

      if (protectRefLevel < 0)
        return currentValue;

      // StoreのpointerOperandは，変更したいアドレスへのポインタなので，
      // 一つポインタが多く被さっている．そのため，+1が必要．
      if (toWraps <= protectRefLevel + 1)
        return currentValue;

      return NULL;
    }

    bool isPointerOperation(Value *pointerOperand, bool debug = true) {
      const Type *destTy = pointerOperand->getType();

      // storeのオペランドはポインタのはず
      assert(destTy->isPointerTy() && "store's operand must pointer");

      // 間にload命令が挟まっている場合は，ポインタを剥がして代入するケース
      if (dyn_cast<LoadInst>(pointerOperand)) {
        if (debug)
          errs() << "[pointer] ";
        return true;
      }

      // 間にgep命令が挟まっている場合は，構造体メンバか配列要素
      if (dyn_cast<GetElementPtrInst>(pointerOperand)) {
        if (debug)
          errs() << "[array element or struct member] ";
        return true;
      }

      // アドレスが固定の配列・構造体メンバ（例: グローバル変数）
      if (ConstantExpr *expr = dyn_cast<ConstantExpr>(pointerOperand)) {
        if (debug)
          errs() << "[array element or struct member (constant)] ";
        if (expr->getOpcode() == Instruction::GetElementPtr &&
            expr->getNumOperands() > 0) {
          return true;
        } else {
          errs() << "[Unsupported ConstantExpr: OP: " << expr->getOpcode() << "  INST: " << Instruction::GetElementPtr << "] ";
        }
      }

      if (const PointerType *ptrTy = dyn_cast<PointerType>(destTy)) {
        if (!ptrTy->getContainedType(0)->isPointerTy()) {
          // loadが挟まっておらず，ポインタを一つ剥がした結果，それがポインタでな
          // ければ，実体．int i; i = value;
          return false;
        }
      }

      return true;
    }

    Value *getEccProtectedOperand(Value *pointerOperand) {
      // pointerOperandは，アドレス操作されて取得されたものか？
      // （ポインタ・配列・構造体メンバ）
      if (isPointerOperation(pointerOperand)) {
        if (Value *operand = checkPointerHasEccValue(pointerOperand))
          return operand;
        return NULL;
      }

      // 変数アクセス
      if (pointerOperand->isecc) {
        errs() << "[var] ";
        return pointerOperand;
      }

      return NULL;
    }

    bool insertInstructionsForStore(Function &F) {
      bool changed = false;

      errs() << "- [ECC: for store ]\n";

      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // 各ブロックを処理する

        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // 各命令を処理する
          errs() << "\n";
          errs() << " inst: " << I->getOpcodeName() << " ";

          StoreInst *store_insn = dyn_cast<StoreInst>(I);
          if (store_insn == NULL)
            continue;

          Value *ecc = getEccProtectedOperand(store_insn->getPointerOperand());
          if (ecc) {
            errs() << "\n  Store instruction with ecc lvalue found [" << ecc << "].\n";
            store_insn->print(errs()); errs() << "\n";
            insertUpdateEccDataInstruction(I);
            changed = true;
          }
        }
      }
      errs() << "\n";

      return changed;
    }

    bool insertInstructionsForLoad(Function &F) {
      bool changed = false;

      errs() << "- [ECC: for read ]\n";
      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // for each block
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // for each instruction
          errs() << "\n";
          errs() << " inst: " << I->getOpcodeName() << " ";

          LoadInst *loadInst = dyn_cast<LoadInst>(I);

          if (loadInst == NULL)
            continue;
          if (loadInst->getPointerOperand()->isecc) {
            errs() << "\n  Load instruction with ecc found.\n";
            loadInst->print(errs()); errs() << "\n";
            insertLoadValueInstruction(I);
            changed = true;
          }
        }
      }

      return changed;
    }

    virtual bool runOnFunction(Function &F) {
      bool changed = false;
      bool both = (!OptWakInsertEccStore && !OptWakInsertEccLoad);
      FUNC = &F;

      errs() << "--- [Wak insert duplicate instuction test] ---\n";
      errs() << "Function: ";
      errs().write_escaped(F.getName());
      errs() << "\n\n";

      if (both || OptWakInsertEccStore) // 代入時に複製を作る
        if (insertInstructionsForStore(F))
          changed = true;
      if (both || OptWakInsertEccLoad) // 参照時に複製を作る
        if (insertInstructionsForLoad(F))
          changed = true;

      errs() << "\n-------------------- [wak] --------------------\n\n";
      return changed;
    }

    virtual const char *getPassName() const {
      return "WakInsertEccPass";
    }
  };
}

char WakInsertEcc::ID = 0;
INITIALIZE_PASS(WakInsertEcc, "wak-insert-ecc",
                "Wak insert ECC code", false, false)

FunctionPass *llvm::createWakInsertEccPass(const TargetLowering *tli) {
  return new WakInsertEcc();
}
