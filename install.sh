#!/bin/sh
srcdir=$(dirname $0)
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp "$srcdir/fmt-ja.scm" "$srcdir/fmt-ja-custom.scm" "$scmdir"
cp "$srcdir/pixmaps/fmt-ja.png" "$srcdir/pixmaps/fmt-ja_dark_background.png" "$pixmapsdir"
cp "$srcdir/pixmaps/fmt-ja.svg" "$srcdir/pixmaps/fmt-ja_dark_background.svg" "$pixmapsdir"
uim-module-manager --register fmt-ja
