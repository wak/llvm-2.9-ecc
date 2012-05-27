#!/bin/zsh

function backup-tar {
	if ! askyn "run tarball mode?"; then
		return
	fi
	local file=llvm-`date '+%Y-%m-%d-%H-%M-%S'`.tar.bz2
	local tempfile=/tmp/$file
	tar cvjf $tempfile . \
		--exclude='.git' \
		--exclude="GTAGS" \
		--exclude="GRTAGS" \
		--exclude="GSYMS" \
		--exclude="GPATH" \
		--exclude="Debug+Asserts" \
		--exclude="Debug+Profile+Asserts" \
		--exclude="Debug+Profile" \
		--exclude="Debug" \
		--exclude="ARM" \
		--exclude="Mips" \
		--exclude="SystemZ" \
		--exclude="Alpha" \
		--exclude="Sparc" \
		--exclude="Blackfin" \
		--exclude="CellSPU" \
		--exclude="PowerPC" \
		--exclude="test" .

	scp $tempfile galaxy:.hidden/backup/research/llvm-tars/$file
	rm $tempfile
}

function backup-sync {
	if askyn "run sync mode?"; then
		rsync -avz \
			--exclude='.git/' \
			--exclude="/G*"   \
			--exclude="Debug+Asserts/" \
			--exclude="Debug+Profile+Asserts/" \
			--exclude="Debug+Profile/" \
			--exclude="Debug/" \
			--exclude="/test/" \
			. galaxy:.hidden/backup/research/LLVM
	fi
}

if [ "x$1" = "x--tar" ]; then
	backup-tar
else
	backup-sync
fi
