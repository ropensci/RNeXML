all:
	make manuscript.pdf

manuscript.pdf: manuscript.Rmd
	Rscript -e "rmarkdown::render('manuscript.Rmd')"

clear-cache:
	rm -rf components/cache

restore-cache:
	Rscript components/restore-cache.R

clean:
	rm -f ../tests/testthat/*.xml
