#!/bin/sh
dir=$(cd "$(dirname $0)"; pwd)
# XXX: use realpath or readlink -f
scmfile="$dir/uim-fmt-ja.scm"

foldonly=f
indchange=f
while getopts fg:im:t: f
do
	case $f in
	f) foldonly=t;;
	g) goal=$OPTARG;;
	i) indchange=t;;
	m) max=$OPTARG;;
	t) tab=$OPTARG;;
	esac
done
shift `expr $OPTIND - 1`

if [ ! -t 0 ]; then	# data from stdin
	nkf -e | uim-sh "$scmfile" "$foldonly" "$indchange" "$goal" "$max" "$tab"
else
	for f
	do
		if [ -f "$f" ]; then
			nkf -e "$f" | uim-sh "$scmfile" "$foldonly" "$indchange" "$goal" "$max" "$tab"
		fi
	done
fi
