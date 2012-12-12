#! /bin/sh

getCounter() {
	local count=

	# Create count file if not exist
	if ! [ -f $1 ]; then
		echo 0 > $1
	fi

	exec 3< $1
	read count 0<&3
	exec 3<&-

	[ "X$count" = "X" ] && count="0"
	echo $count

	# Increment count
	echo $(($count + 1)) > $1
}

num=$(printf "%03d" $(getCounter ".backup.count"))

timestamp=`date '+%Y%m%d'`
llvm="${num}-llvm-${timestamp}.diff"
clang="${num}-clang-${timestamp}.diff"

git diff HEAD > $llvm
scp $llvm galaxy:diffs/
diffstat -C $llvm
rm $llvm

echo
echo

cd tools/clang
git diff HEAD > $clang
scp $clang galaxy:diffs/
diffstat -C $clang
rm $clang
