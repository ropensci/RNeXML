
all:
	make README.md
	make RNeXML_tutorial.Rnw
	make RNeXML_tutorial.pdf
	make vignettes

RNeXML_tutorial.Rnw: README.md
	pandoc README.md -o RNeXML_tutorial.tex
	mv RNeXML_tutorial.tex RNeXML_tutorial.Rnw

RNeXML_tutorial.pdf: README.md
	pandoc README.md -o RNeXML_tutorial.pdf

vignettes:
	cp RNeXML_tutorial.Rnw ../../vingettes/
	cp RNeXML_tutorial.pdf ../../vingettes/


README.md: README.Rmd 
	Rscript -e 'require(knitr); knit("README.Rmd")'
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md


coffee: 
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md
