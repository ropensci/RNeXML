
all:
	make README.md
	make ../README.md
	make ../vignettes/README.Rmd
	make manuscript.md
	make manuscript.pdf


README.md: README.Rmd 
	Rscript -e 'require(knitr); opts_knit[["set"]](upload.fun = imgur_upload); knit("README.Rmd")'

../README.md: README.md
	cp README.md ../README.md
	Rscript github_highlighting.R ../README.md

../vignettes/README.Rmd: README.Rmd
	cp README.Rmd ../vignettes/README.Rmd


manuscript.md: manuscript.Rmd
	Rscript -e 'require(knitr); require(methods); opts_knit[["set"]](upload.fun = imgur_upload); knit("manuscript.Rmd")'
	Rscript github_highlighting.R manuscript.md

manuscript.pdf: manuscript.Rmd
	Rscript -e 'require(knitr); opts_chunk[["set"]](dev='pdf'); knit("manuscript.Rmd", "tmp.md")'
	pandoc tmp.md --template elsarticle.latex -o manuscript.pdf
	rm tmp.md 

clean: 
	rm -f *.xml tmp.md
	rm -f ../*.xml
	rm -f ../tests/testthat/*.xml
	rm -f ../vignettes/*.xml
	rm -rf cache/
	rm -rf figure/
