Instructions for compiling manuscripts
--------------------------------------

```bash
make
```



## Caching


I've also enabled caching.  It can be annoying to have to have to rerun all the R code just to make a textual change to the manuscript or readme.  The cache is ignored by git so the first time you run `make` all the code will run, and thereafter you will have the cache. The caching is modestly intelligent, in that if you edit a chunk it will be rerun by default, (as will chunks with declared dependencies on it). See `knitr`'s [caching documentation](http://yihui.name/knitr/demo/cache/) for details.

You can clear the cache by deleting it, or just use `make clear-cache`.

### Restoring the default cache

You can obtain my current cache by using `make restore-cache`.

### Picking up at a particular chunk

For the sake of modular reproducibility, it can also be desirable to pick up from somewhere in the middle of the manuscript and not want to have to run all the previous code just to try out one line (not really an issue in this paper, but more generally).  Caching is therefore modular by chunk, allowing you to restore the results of a particular chunk to investigate. Again see `knitr`'s [caching documentation](http://yihui.name/knitr/demo/cache/) for details.

