#ifndef LLVM_SYSTEM_WAK_H
#define LLVM_SYSTEM_WAK_H

namespace llvm {
#define COLOR_RED(string) "\x1b[1;31m" string COLOR_END
#define COLOR_RED_BEG     "\x1b[1;31m"
#define COLOR_GREEN_BEG   "\x1b[1;32m"
#define COLOR_YELLOW_BEG  "\x1b[1;33m"
#define COLOR_BLUE_BEG    "\x1b[1;34m"
#define COLOR_MAGENTA_BEG "\x1b[1;35m"
#define COLOR_CYAN_BEG    "\x1b[1;36m"
#define COLOR_WHITE_BEG   "\x1b[1;37m"

#define COLOR_RED_R_BEG     "\x1b[7;31m"
#define COLOR_GREEN_R_BEG   "\x1b[7;32m"
#define COLOR_YELLOW_R_BEG  "\x1b[7;33m"
#define COLOR_BLUE_R_BEG    "\x1b[7;34m"
#define COLOR_MAGENTA_R_BEG "\x1b[7;35m"
#define COLOR_CYAN_R_BEG    "\x1b[7;36m"
#define COLOR_WHITE_R_BEG   "\x1b[7;37m"

#define COLOR_END "\x1b[m"

  struct EccComputeInfo {
    enum {
      None = 0,
      LoadVariable,          // FirstLoadとOtherLoadの合体版．うえ二つは消すかも
      ComputeEccRelated,     // ECCの値同士を計算する
      ComputeEccRelatedTwoAddr, // TwoAddressInstructionPass用
      ComputeEccRelatedEnd   // ECCの値同士を計算する流れの終わり？
    };

    static const char *getInfoName(unsigned info) {
      switch (info) {
#define DEF(v) case v: return #v;
#define DEF2(v, s) case v: return s;
        DEF2(None, "-");
        DEF2(LoadVariable, "Load");
        DEF2(ComputeEccRelated, "Rel");
        DEF2(ComputeEccRelatedTwoAddr, "R-TA");
        DEF2(ComputeEccRelatedEnd, "END");
#undef DEF
#undef DEF2
      }
      return "?";
    }
  };
}

#endif
