#!/bin/sh

timestamp=`date '+%Y%m%d'`
llvm="llvm-$timestamp.diff"
clang="clang-$timestamp.diff"

git diff HEAD > $llvm
scp $llvm galaxy:diffs/
rm $llvm

cd tools/clang
git diff HEAD > $clang
scp $clang galaxy:diffs/
rm $clang
