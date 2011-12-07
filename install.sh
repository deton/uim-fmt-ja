#!/bin/sh
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp fmt-ja.scm fmt-ja-custom.scm"$scmdir"
cp fmt-ja.png fmt-ja_dark_background.png "$pixmapsdir"
cp fmt-ja.svg fmt-ja_dark_background.svg "$pixmapsdir"
uim-module-manager --register fmt-ja
