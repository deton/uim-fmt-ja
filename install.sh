#!/bin/sh
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp fmt-ja.scm fmt-ja-custom.scm "$scmdir"
cp pixmaps/fmt-ja.png pixmaps/fmt-ja_dark_background.png "$pixmapsdir"
cp pixmaps/fmt-ja.svg pixmaps/fmt-ja_dark_background.svg "$pixmapsdir"
uim-module-manager --register fmt-ja
