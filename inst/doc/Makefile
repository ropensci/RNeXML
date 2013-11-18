
vignette: README.Rmd
	Rscript -e 'require(knitr); knit("README.Rmd")'
	pandoc README.md -o RNeXML_tutorial.tex
	mv RNeXML_tutorial.tex RNeXML_tutorial.Rnw
	pandoc README.md -o RNeXML_tutorial.pdf
	cp RNeXML_tutorial.Rnw ../../vingettes/
	cp RNeXML_tutorial.pdf ../../vingettes/


readme: README.Rmd 
	Rscript -e 'require(knitr); knit("README.Rmd")'
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md


coffee: 
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md
