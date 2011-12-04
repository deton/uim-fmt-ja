#!/bin/sh
scmfile=$(readlink -f $(dirname $0)/uim-fmt-ja.scm)
if [ ! -t 0 ]; then	# data from stdin
	nkf -e | uim-sh $scmfile
else
	for f
	do
		if [ -f $f ]; then
			nkf -e $f | uim-sh $scmfile
		fi
	done
fi
