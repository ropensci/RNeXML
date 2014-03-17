
all:
	make README.md
	make ../../../README.md
	make ../../../vignettes/README.Rmd

README.md: README.Rmd 
	Rscript -e 'require(knitr); knit("README.Rmd")'

../../../README.md: README.md
	cp README.md ../../../README.md
	sed -i 's/```r/```coffee/' ../../../README.md

../../../vignettes/README.Rmd: README.Rmd
	cp README.Rmd ../../../vignettes/README.Rmd
	sed -i 's/socialR::flickr.url/imgur_upload/' ../../../vignettes/README.Rmd


manuscript.md: manuscript.Rmd
	Rscript -e 'require(knitr); knit("manuscript.Rmd")'

manuscript.pdf: manuscript.Rmd
	Rscript -e 'require(knitr); knit("manuscript.Rmd", "tmp.md")'
	pandoc tmp.md --template elsarticle.latex -o manuscript.pdf


clean: 
	rm -f *.xml tmp.md
	rm -f ../../../vignettes/*.xml
