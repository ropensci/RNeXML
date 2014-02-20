
all:
	make README.md
	make ../../README.md

README.md: README.Rmd 
	Rscript -e 'require(knitr); knit("README.Rmd")'

../../README.md: README.md
	cp README.md ../../README.md
	sed -i 's/```r/```coffee/' ../../README.md


