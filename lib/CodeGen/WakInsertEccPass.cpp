// 汎用ECC実装
//   具体的なアルゴリズムやデータ構造は，ライブラリに任せている．

#define DEBUG_TYPE "wak-insert-ecc"
#include <sstream>
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
#include "llvm/Support/Wak.h"

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

    void warning(StringRef message, const Instruction *I) {
      errs() << "\x1b[1;31mECC";
      if (I && !I->getDebugLoc().isUnknown()) {
        const DebugLoc &Loc = I->getDebugLoc();
        errs() << ":" << Loc.getLine() << ":" << Loc.getCol();
      }
      errs() << ": warning: " << message << "\x1b[m\n";
    }

    // uint64_t ecc_load_integer(void *app_addr, uint64_t nr_bits)
    Function *getEccLoadFunction(void) {
      LLVMContext &C = FUNC->getContext();
      Function *func =
        cast<Function>(FUNC->getParent()->
                       getOrInsertFunction("ecc_load_integer",
                                           IntegerType::getInt64Ty(C),
                                           Type::getInt8PtrTy(C, 0),
                                           IntegerType::getInt64Ty(C),
                                           NULL));
      return func;
    }

    // void ecc_update_ecc_data_bits(void *app_addr, uint64_t nr_bits)
    Function *getEccUpdateEccDataBitsFunction(void) {
      LLVMContext &C = FUNC->getContext();
      Function *func =
        cast<Function>(FUNC->getParent()->
                       getOrInsertFunction("ecc_update_ecc_data_bits",
                                           Type::getVoidTy(C),
                                           Type::getInt8PtrTy(C, 0),
                                           IntegerType::getInt64Ty(C),
                                           NULL));
      return func;
    }

    // ターゲットアーキテクチャのポインタのサイズを返す
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

    // 引数注意
    //   type: サイズが知りたい型へのポインタ
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

    // 定義を遡ってeccComputeInfoを削除する
    void clearEccComputeInfo(User *U) {
      if (U == NULL)
        return;
      U->eccComputeInfo = EccComputeInfo::None;
      for (unsigned i = 0; i < U->getNumOperands(); i++) {
        Value *V = U->getOperand(i);
        clearEccComputeInfo(dyn_cast<User>(V));
      }
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
    // 関数一覧
    //   数値用（汎用）: uint64_t ecc_load_integer(void *app, uint64_t bits)
    //   ポインタ用    : void *ecc_load_pointer(void *app, uint64_t bits) 数値用でも代用できるかも
    int insertLoadValueInstruction(BasicBlock::iterator insn_iter) {
      int nr_inserted_instructions = 0;
      LoadInst *loadInst = dyn_cast<LoadInst>(insn_iter);
      const Type *dataType = loadInst->getType();

      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(loadInst->getParent(), ++insn_iter);

      // ロード関数を取得
      // @todo 型によって関数を変える
      Function *load_function = getEccLoadFunction();
      if (load_function == NULL)
        report_fatal_error("ECC load function not found (ecc_load_*)");

      // ロードするサイズを取得
      unsigned int size = getTypeSizeInBits(loadInst->getPointerOperand()->getType());

      // ロード関数の呼出し命令を挿入
      //   load_function((void *) pointerOperand, target_size_in_bits);
      Instruction *loadFuncCall = 
        B.CreateCall2(load_function,
                      B.CreatePointerCast(loadInst->getPointerOperand(), B.getInt8PtrTy()),
                      B.getInt64(size));
      nr_inserted_instructions++;

      // i1,i8,i16,i32,i64,i128,   f....
      // uint64_tを本来の型にキャスト
      Value *cast = loadFuncCall;
      if (dataType->isPointerTy()) {
        cast = B.CreateIntToPtr(loadFuncCall, dataType);
        nr_inserted_instructions++;
      } else if (dataType->isIntegerTy()) {
        if (dataType->getPrimitiveSizeInBits() > 64)
          report_fatal_error("ECC load: over 64bit load not supported");
        else if (dataType->getPrimitiveSizeInBits() < 64) {
          cast = B.CreateTrunc(loadFuncCall, dataType);
          nr_inserted_instructions++;
        }
      }

      // キャスト情報確認
      if (true) {
        errs() << "  cast: orig=";
        dataType->print(errs());
        errs() << ", from=";
        loadFuncCall->getType()->print(errs());
        errs() << ", to=";
        cast->getType()->print(errs());
        errs() << "\n  inserted: ";
        loadFuncCall->print(errs());
        errs() << "\n";
      }

      // デバッガで停止できるようにする
      loadFuncCall->setDebugLoc(loadInst->getDebugLoc());

      // このLOAD命令への挿入は，オペランドを保護すべきであるために行われた．した
      // がって，この戻り値を用いてポインタを辿る場合も，保護すべきである．
      // 
      // @todo: 本当か？例外ケースは？
      cast->isecc = true;
      cast->ecc_reference_level = -1;

      // @todo: [bug] 暗黙キャスト考慮しないと．例: c = a + (int) b;
      cast->isEccRelated           = true;
      cast->eccComputeInfo         = EccComputeInfo::ComputeEccRelated;
      loadFuncCall->isEccRelated   = true;
      loadFuncCall->eccComputeInfo = EccComputeInfo::LoadVariable;

      // 本来のLOADへのすべての参照を入れ替える
      loadInst->replaceAllUsesWith(cast);

      return nr_inserted_instructions;
    }

    // アプリケーションのデータに対応する，ECCデータ領域を更新するための命令を挿入する
    int insertUpdateEccDataInstruction(BasicBlock::iterator insn_iter) {
      StoreInst *storeInst = dyn_cast<StoreInst>(insn_iter);

      IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> > B(storeInst->getParent(), ++insn_iter);

      // 更新関数を取得
      Function *update_bits_function = getEccUpdateEccDataBitsFunction();
      if (update_bits_function == NULL)
        report_fatal_error("ECC update function not found (ecc_update_ecc_data_bits)");

      // 更新される（された）サイズを取得
      unsigned int size = getTypeSizeInBits(storeInst->getPointerOperand()->getType());

      // 更新関数の呼出し命令を挿入
      //   ecc_update_ecc_data_bits((void *) pointerOperand, target_size_in_bits);
      Instruction *call =
        B.CreateCall2(update_bits_function,
                      B.CreatePointerCast(storeInst->getPointerOperand(), B.getInt8PtrTy()),
                      B.getInt64(size));

      // デバッガで停止できるようにする
      call->setDebugLoc(storeInst->getDebugLoc());

      return 2;                 // 挿入した命令は2個
    }

    // 単純に，ポインタがいくつ挟まっているかを数える．
    int countPointerWraps(const Type *type) {
      int count;

      errs() << "  (count: ";
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

    // オペランド（ポインタが指す先）が保護されているかを調べる．
    // 保護されていれば，その定義点を返す．
    Value *checkPointerHasEccValue(const Value *pointerOperand, bool level_check = true) {
      /*
       * [ポインタルール]
       *   ポインタ変数に_ecc_(level = 0)を指定した場合，
       *   実体とlevel個分のポインタを保護する．
       *   level = -1の時は，ポインタを辿る全てを保護する．
       *
       *   - 例1: int **p __attribute__((ecc(0)));
       *     - p   = ... なし
       *     - *p  = ... なし
       *     - **p = ... 保護
       *   - 例1: int **p __attribute__((ecc(1)));
       *     - p   = ... なし
       *     - *p  = ... 保護
       *     - **p = ... 保護
       *
       * [アルゴリズム（代入時）]
       *   1. 祖先を辿り，ECC付きかチェックする
       *   2. levelを取得する
       *   3. 代入する型をチェックし，ポインタの階層がlevel内かチェックする
       *
       * [アルゴリズム（参照時）]
       *   - 代入時アルゴリズムと同様
       */

      // 最初の定義点を探す．
      // shouldUsePointerRule()でポインタであることはチェックしてある
      const Value *currentValue = pointerOperand;
      while (true) {
        if (currentValue->isecc)
          break;

        if (const LoadInst *loadInst = dyn_cast<LoadInst>(currentValue)) {
          // ポインタを剥がす場合は，load命令が入る
          currentValue = loadInst->getPointerOperand();
        } else if (const GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(currentValue)) {
          // 配列の要素や構造体メンバを参照する場合は，GEP命令が入る
          // ただのアドレス計算に使われているだけなので，基底ポインタを取得する
          currentValue = gepInst->getPointerOperand();
        } else if (const ConstantExpr *expr = dyn_cast<ConstantExpr>(currentValue)) {
          if (expr->getOpcode() == Instruction::GetElementPtr &&
              expr->getNumOperands() > 0) {
            currentValue = expr->getOperand(0);
          } else {
            errs() << "[Unsupported ConstantExpr: " << expr->getOpcode()
                   << "  INST: " << Instruction::GetElementPtr << "]\n";
          }
        } else if (const CastInst *castInst = dyn_cast<CastInst>(currentValue)) {
          // @todo: 他のケースもあるはず．余計な影響を与えないかチェック
          // char *p, **pp = &p;
          //if (castInst->getDestTy()->isPointerTy())
          currentValue = castInst->getOperand(0);
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

      errs() << "RL = " << protectRefLevel << ", root = " << rootWraps << ", to = " << toWraps << " => ";

      if (!level_check) {
        errs() << "protected (no level_check)\n";
        return const_cast<Value *>(currentValue);
      }

      if (protectRefLevel < 0) {
        errs() << "protected\n";
        return const_cast<Value *>(currentValue);
      }

      // StoreのpointerOperandは，変更したいアドレスへのポインタなので，
      // 一つポインタが多く被さっている．そのため，+1が必要．
      if (toWraps <= protectRefLevel + 1) {
        errs() << "protected\n";
        return const_cast<Value *>(currentValue);
      }

      errs() << "not-protected\n";
      return NULL;
    }

    // 引数: pointerOperand: loadまたはstore命令のオペランド
    // 戻り値: 操作対象は，ポインタルールを適用すべきか？
    bool shouldUsePointerRule(Value *pointerOperand, bool debug = true) {
      const Type *destTy = pointerOperand->getType();

      // store,loadのオペランドはポインタのはず
      assert(destTy->isPointerTy() && "operand must pointer");


      if (CastInst *castInst = dyn_cast<CastInst>(pointerOperand))
        return shouldUsePointerRule(castInst->getOperand(0));

      // 間にload命令が挟まっている場合は，ポインタを剥がして代入するケース
      if (dyn_cast<LoadInst>(pointerOperand)) {
        if (debug)
          errs() << "  [pointer] ";
        return true;
      }

      // 間にgep命令が挟まっている場合は，構造体メンバか配列要素位置を計算するケース
      if (dyn_cast<GetElementPtrInst>(pointerOperand)) {
        if (debug)
          errs() << "  [array element or struct member] ";
        return true;
      }

      // アドレスが固定の配列・構造体メンバ（例: グローバル変数）
      if (ConstantExpr *expr = dyn_cast<ConstantExpr>(pointerOperand)) {
        if (debug)
          errs() << "  [array element or struct member (constant)] ";
        if (expr->getOpcode() == Instruction::GetElementPtr &&
            expr->getNumOperands() > 0) {
          return true;
        } else {
          errs() << "  [Unsupported ConstantExpr: OP: " << expr->getOpcode()
                 << "  INST: " << Instruction::GetElementPtr << "] ";
        }
      }

      if (const PointerType *ptrTy = dyn_cast<PointerType>(destTy)) {
        if (!ptrTy->getContainedType(0)->isPointerTy()) {
          // loadが挟まっておらず，ポインタを一つ剥がしたものがポインタでなければ，
          // 実体を更新または取得するケース．
          //   例: int ecc; ecc = value;
          //   例: int ecc; to = ecc;
          return false;
        }
      }

      // ポインタ変数を更新するケース
      return true;
    }

    // 操作するオペランドを保護すべきかどうか，判定する
    // Return: 保護必要->定義点, 保護不要->NULL
    Value *getEccProtectedOperand(Value *pointerOperand) {
      // pointerOperandは，アドレス操作されて取得されたものか？
      // （ポインタ・配列・構造体メンバ）
      if (shouldUsePointerRule(pointerOperand)) {
        if (Value *operand = checkPointerHasEccValue(pointerOperand))
          return operand;
        errs() << "\n";
        return NULL;
      }

      // 非ポインタ変数アクセス
      if (pointerOperand->isecc) {
        errs() << "  [var] ";
        errs() << "\n";
        return pointerOperand;
      }

      return NULL;
    }

    // ポインタの定義レベルで，ECC型の変換が安全かを確認する
    void checkChangeEccPointerDefinition(const Value *from, const Value *to, const Instruction *inst) {
      if (from == NULL)
        return;
      if (to && to->ecc_reference_level == -1)
        return;

      if (to == NULL)
        // 左辺値に保護情報がない
        warning("change ecc pointer to non-ecc pointer", inst);
      else if (to->ecc_reference_level < from->ecc_reference_level) {
        // ECC保護レベルが足りない
        std::stringstream message;
        message << "change ecc pointer to lack-reference level pointer (lv.";
        message << from->ecc_reference_level;
        message << " -> lv.";
        message << to->ecc_reference_level;
        message << ")";
        warning(message.str(), inst);
      }
    }

    // 警告: non-ECCポインタへECCポインタを代入する
    void checkAssignEccPointerToNonEccPointer(const StoreInst *storeInst) {
      if (!storeInst->getPointerOperand()->getType()->getContainedType(0)->isPointerTy())
        return;
      if (!storeInst->getValueOperand()->getType()->isPointerTy())
        return;

      errs() << "\x1b[1;33m  check warn: \x1b[m\n";

      const Value *lValue = checkPointerHasEccValue(storeInst->getPointerOperand(), false);
      const Value *rValue = checkPointerHasEccValue(storeInst->getValueOperand(), false);

      checkChangeEccPointerDefinition(rValue, lValue, storeInst);
    }

    void fixEccComputeInfo(Function &F) {
      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // 各ブロックを処理する
        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // 各命令を処理する
          Instruction *Inst = I;
          bool allOperandsAreEccRelated = true;

          if (Inst->eccComputeInfo)
            continue;

          // Store命令で，代入する値がECC値であれば，ECC計算関連の終わり
          if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
            if (SI->getValueOperand()->eccComputeInfo) {
              Inst->isEccRelated   = true;
              Inst->eccComputeInfo = EccComputeInfo::ComputeEccRelatedEnd;
              continue;
            }
          }

          for (unsigned i = 0; i < Inst->getNumOperands(); i++) {
            Value *V = Inst->getOperand(i);
            if (!V->eccComputeInfo) { // 何かしらECC関連の処理がなければ
              allOperandsAreEccRelated = false;
              break;
            }
          }

          if (!allOperandsAreEccRelated) {
            errs() << "wak: [debug] clear ComputeEccRelated of ";
            Inst->dump();
            clearEccComputeInfo(Inst);
            continue;
          }

          if (dyn_cast<ReturnInst>(Inst)) {
            // Return命令の場合は，そこで計算が終わり．
            Inst->isEccRelated   = true;
            Inst->eccComputeInfo = EccComputeInfo::ComputeEccRelatedEnd;
          } else if (dyn_cast<StoreInst>(Inst)) {
            // Store命令の場合は，そこで計算が終わり．
            Inst->isEccRelated   = true;
            Inst->eccComputeInfo = EccComputeInfo::ComputeEccRelatedEnd;
          } else {
            // すべてのオペランドがECC計算関連なので，この命令も計算に含める
            errs() << "wak: [debug] attach ComputeEccRelated to ";
            Inst->dump();
            Inst->isEccRelated   = true;
            Inst->eccComputeInfo = EccComputeInfo::ComputeEccRelated;
          }
        }
      }
    }

    bool insertInstructions(Function &F) {
      bool doStore = (OptWakInsertEccPass || OptWakInsertEccStore);
      bool doLoad  = (OptWakInsertEccPass || OptWakInsertEccLoad);

      errs() << "Wak Insert ECC Pass Configuration";
      errs() << "\n  store: " << doStore;
      errs() << "\n  load : " << doLoad;
      errs() << "\n\n";

      bool changed = false;
      int inserted_inst_count = 0;

      errs() << "- [ECC: for store and load]\n";

      for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        // 各ブロックを処理する

        for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
          // 各命令を処理する

          // 今，どの命令を処理しているのかを表示する
          errs() << " inst: ";
          if (I->getOpcode() == Instruction::Load || I->getOpcode() == Instruction::Store)
            errs() << "\x1b[1;31m"; // Change color to bold red
          if (inserted_inst_count > 0) {
            inserted_inst_count--;
            errs() << "\x1b[1;35m"; // Change color to bold magenta
          }
          errs() << I->getOpcodeName(); errs() << "\x1b[m\t";
          I->print(errs());
          errs() << "\n";

          // @memo 以下の処理で，新たに命令が挿入される場合がある．挿入された命令
          // は，次のループで見つかる．現状では，storeとload命令が挿入されること
          // は無いため，問題ない．

          // 代入時に複製を作る
          StoreInst *storeInst = dyn_cast<StoreInst>(I);
          if (doStore && storeInst) {
            Value *shouldProtect = getEccProtectedOperand(storeInst->getPointerOperand());
            if (shouldProtect) {
              errs() << "\x1b[1;36m  Store instruction with ecc lvalue found [" << shouldProtect << "].\x1b[m\n";
              inserted_inst_count = insertUpdateEccDataInstruction(I);
              changed = true;
              errs() << "\n";
            }

            checkAssignEccPointerToNonEccPointer(storeInst);
          }

          // 参照時に複製を作る
          LoadInst *loadInst = dyn_cast<LoadInst>(I);
          if (doLoad && loadInst) {
            Value *shouldProtect = getEccProtectedOperand(loadInst->getPointerOperand());
            if (shouldProtect) {
              errs() << "\x1b[1;36m  Load instruction with ecc found [" << shouldProtect << "].\x1b[m\n";
              inserted_inst_count = insertLoadValueInstruction(I);
              changed = true;
              errs() << "\n";
            }
          }
        }
      }
      errs() << "\n";

      return changed;
    }

    virtual bool runOnFunction(Function &F) {
      bool changed = false;
      FUNC = &F;

      errs() << "--- [Wak insert duplicate instuction test] ---\n";
      errs() << "Function: ";
      errs().write_escaped(F.getName());
      errs() << "\n\n";

      changed = insertInstructions(F);
      if (changed) {
        fixEccComputeInfo(F);
      }

      errs() << "\n-------------------- [wak] --------------------\n\n";
      return changed;
    }

    virtual const char *getPassName() const {
      return "WakInsertEccPass";
    }
  };
}

char WakInsertEcc::ID = 0;
INITIALIZE_PASS(WakInsertEcc, "wak-insert-ecc", "Wak insert ECC code", false, false)

FunctionPass *llvm::createWakInsertEccPass(const TargetLowering *tli) {
  return new WakInsertEcc();
}
