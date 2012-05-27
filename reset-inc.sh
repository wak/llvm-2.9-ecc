#!/bin/sh

find -name '*.inc' -exec git rm --cached {} \;

rm -f lib/Target/X86/*.inc

git add include/llvm/CompilerDriver/Main.inc
git add lib/Support/Unix/Host.inc
git add lib/Support/Unix/Memory.inc
git add lib/Support/Unix/Mutex.inc
git add lib/Support/Unix/Path.inc
git add lib/Support/Unix/PathV2.inc
git add lib/Support/Unix/Process.inc
git add lib/Support/Unix/Program.inc
git add lib/Support/Unix/RWMutex.inc
git add lib/Support/Unix/ThreadLocal.inc
git add lib/Support/Unix/TimeValue.inc
git add lib/Support/Unix/system_error.inc
git add lib/Support/Unix/Signals.inc
git add lib/Support/Windows/DynamicLibrary.inc
git add lib/Support/Windows/Host.inc
git add lib/Support/Windows/Memory.inc
git add lib/Support/Windows/Mutex.inc
git add lib/Support/Windows/Path.inc
git add lib/Support/Windows/PathV2.inc
git add lib/Support/Windows/Process.inc
git add lib/Support/Windows/Program.inc
git add lib/Support/Windows/RWMutex.inc
git add lib/Support/Windows/Signals.inc
git add lib/Support/Windows/ThreadLocal.inc
git add lib/Support/Windows/TimeValue.inc
git add lib/Support/Windows/explicit_symbols.inc
git add lib/Support/Windows/system_error.inc
git add lib/Support/regengine.inc
git add test/TableGen/Include.inc
