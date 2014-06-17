all:
	make tutorial.md
	make ../README.md
	make ../vignettes/tutorial.Rmd
	make manuscript.pdf


tutorial.md: tutorial.Rmd 
	Rscript -e 'require(knitr); opts_knit[["set"]](upload.fun = imgur_upload); knit("tutorial.Rmd")'

../README.md: tutorial.md
	mv tutorial.md ../README.md

../vignettes/tutorial.Rmd: tutorial.Rmd
	cp tutorial.Rmd ../vignettes/

manuscript.pdf: manuscript.Rmd
	Rscript components/make_pdf.R manuscript.Rmd

clear-cache:
	rm -rf components/cache

restore-cache:
	Rscript components/restore-cache.R

clean:
	rm -rf tutorial.md
	rm -rf figure/
	rm -f tmp.md
	rm -f RNeXML-manuscript-cache.tar.gz
	rm -f ../tests/testthat/*.xml
