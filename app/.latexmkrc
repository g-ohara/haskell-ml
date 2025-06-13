#!/usr/bin/env perl

# Search Literate Haskell sources as default files. (Default value: ('*.tex'))
@default_files = ('*.tex', '*.lhs');

$lualatex = 'lualatex -shell-escape -interaction=nonstopmode';
$pdf_mode	= 4; # generates pdf via lualatex

# Prevent latexmk from removing PDF after typeset.
$pvc_view_file_via_temporary = 0;
