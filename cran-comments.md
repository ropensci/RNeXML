Dear CRAN maintainers,

This update fixes a minor bug in a unit test which was introduced by a recent change to the geiger package.
Slow-running tests and slow-running vignettes are now sped up or skipped/precomputed as well.  (Apologies, these
tests and vignettes take only a few minutes to run on my test machine, but apparently are substantially slower
on the CRAN machines.)

Carl



