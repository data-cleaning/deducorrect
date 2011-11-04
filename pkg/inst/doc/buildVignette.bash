
R -e "Sweave('deducorrect-vignette.Rnw')"
pdflatex deducorrect-vignette.tex
bibtex deducorrect-vignette
pdflatex deducorrect-vignette.tex
pdflatex deducorrect-vignette.tex


