all:
	make manuscript.pdf
	make supplement.pdf

manuscript.pdf: manuscript.Rmd
	Rscript -e "rmarkdown::render('manuscript.Rmd')"

supplement.pdf: supplement.Rmd
	Rscript -e "rmarkdown::render('supplement.Rmd')"

clear-cache:
	rm -rf components/cache

restore-cache:
	Rscript components/restore-cache.R

clean:
	rm -f ../tests/testthat/*.xml
