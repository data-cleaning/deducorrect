#!/bin/bash

doc=$1

R -e "Sweave('deducorrect-${1}.Rnw')"
pdflatex deducorrect-${1}.tex
bibtex deducorrect-${1}
pdflatex deducorrect-${1}.tex
pdflatex deducorrect-${1}.tex


