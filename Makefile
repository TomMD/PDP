all:
	ghc --make -O -hidir obj -odir obj -o pdp8 Main.hs

qc:
	ghc --make -O -hidir obj -odir obj -o qc QC.hs

paper:
	pdflatex report.tex
