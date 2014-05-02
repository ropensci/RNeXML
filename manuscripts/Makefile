
all:
	make tutorial.md
	make ../README.md
	make ../vignettes/tutorial.Rmd
	make manuscript.md
	make manuscript.pdf


tutorial.md: tutorial.Rmd 
	Rscript -e 'require(knitr); opts_knit[["set"]](upload.fun = imgur_upload); knit("tutorial.Rmd")'
	rm -rf figure/ 

../README.md: tutorial.md
	cp tutorial.md ../README.md
	Rscript components/github_highlighting.R ../README.md

../vignettes/tutorial.Rmd: tutorial.Rmd
	cp tutorial.Rmd ../vignettes/


manuscript.md: manuscript.Rmd
	Rscript -e 'require(knitr); require(methods); opts_knit[["set"]](upload.fun = imgur_upload); knit("manuscript.Rmd")'
	Rscript components/github_highlighting.R manuscript.md

manuscript.pdf: manuscript.Rmd
	Rscript -e 'require(knitr); opts_chunk[["set"]](dev='pdf'); knit("manuscript.Rmd", "tmp.md")'
	pandoc tmp.md --template elsarticle.latex -o manuscript.pdf
	rm tmp.md 


clear-cache:
	rm -rf components/cache

restore-cache:
	cd components
	wget http://two.ucdavis.edu/~cboettig/data/RNeXML-manuscript-cache.tar.gz
	tar -xvf RNeXML-manucript-cache.tar.gz
	rm RNeXML-manuscript-cache.tar.gz
	cd ..

clean:
	rm -rf figure/
	rm -f tmp.md
	rm -f RNeXML-manuscript-cache.tar.gz
	rm -f ../tests/testthat/*.xml
