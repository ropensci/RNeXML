
README: README.Rmd
	Rscript -e 'require(knitr); knit("README.Rmd")'
	pandoc README.md -o reml.tex
	mv reml.tex reml.Rnw
	pandoc README.md -o reml.pdf
	cp reml.Rnw ../../READMEs/reml.Rnw
	cp reml.pdf ../../READMEs/reml.pdf

readme: 
	Rscript -e 'require(knitr); knit("README.Rmd")'
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md


coffee: 
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md
