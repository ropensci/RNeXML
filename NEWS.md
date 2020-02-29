NEWS
====

For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/ropensci/RNeXML/issues)
* [The commit log](https://github.com/ropensci/RNeXML/commits/master)

v2.4.3
------

- This update fixes a minor bug in a unit test for compatibilty 
  with R-devel (4.0.0) as requested by CRAN (#245)

v2.4.2
------

- This update fixes a minor bug in a unit test which was introduced by a recent change to the geiger package.

v2.4.0
------

- Makes various tests more robust, and uses symbolic address for nexml.org (#238)
- Provides a real `summary()` and improved pretty-print (#237)
- Makes `character(0)` metadata value behave as empty string (#236)
- Fixes detection of having to split matrix by class (#235)
- Switch over to Additional_repositories for CRAN (#229)
- Do not add `ter` namespace by default. (#227)
- Replace taxize with taxald (#226)
- Fixes how metadata arguments are passed on to `add_basic_meta()` (#220)
- Fixes CDAO namespace definition [#219]
- Enables handling of files with `rootedge` [218]


v2.3.0
-------


This release addresses several aspects improving the handling of metadata:

- `add_meta()` now works for trees and characters (#213, PR #217)
- Handles nested meta elements properly (#196, PR #197)

Misc fixes:

- enable handling of `rootEdge` (#207, PR #218)
- Replaces taxize backend with faster alternative `taxadb` method. (#224, PR #226).
 This remains only a suggested package and has much lighter dependencies as well.  
- add hex

v2.2.0
------

- Fixes various (previously broken) aspects of handling polymorphic
  and uncertain states for discrete (non-molecular) and continuous
  characters, including obtaining a character matrix (#174), ensuring
  proper column types (#188), and serializing to NeXML (#192).
- Adds the optional ability to, in addition to the character matrix,
  obtain a concordant formatted matrix of state types (standard,
  polymorphic, uncertain).
- Fixes loss of certain literal-valued metadata when serializing to
  NeXML. #193
- Drops package phylobase as dependency. (Also removes circular
  dependency chain, because phylobase depends on RNeXML.)

v2.1.2
------

- Fix failing checks on CRAN that require a network connection

v2.1.1 
------

- avoid rdf-based tests on solaris architecture, where suggested
  package rdflib is not available. (CRAN request.)

v2.1.0 2018-05-05
------

- `taxize` as Suggests only
- drop `rrdf` in favor of `rdflib`
- drop `Sxslt` in favor of `xslt`


v2.0.8 2017-11-17
------

- patch for compatibility with upcoming release of `testthat`

v2.0.7 2016-06-28
------

- Bugfixes following release of new dplyr and new tidyr dependencies

v2.0.6 2016-03-07
------

- Migrate Additional_repositories to new address for OmegaHat project.

v2.0.5  2015-12-31
-------

- `get_metadata()`, `get_taxa()` now return much richer `data.frames` instead of named vectors. 
  This is potentially a non-backwards compatible change if scripts use the output of these
  functions as lists (#129).  See updated metadata vignette.  This introduces new dependencies
  `dplyr` and `lazyeval`. 
- more robust `nexml_read()` method for URLs, (#123)
- Avoid assuming the namespace prefix `nex` for nexml elements (#51, #124, #126). Includes a
  fix server-side on the NeXML validator as well.
- `nexml_validate()` points to the new validator. (#126)


v2.0.4 2015-10-14
-------

- Fix compatibility issue with recent phytools release.

v2.0.3 2015-05-27
------

- Upgrade tests to be compatible with newest testthat (0.10.0), bumps testthat dependency version up (#119) thanks @hadley

v2.0.2 2015-05-01
------

- Add four new vignettes describing the use of various advanced
  features in the package: the use of SPARQL queries, advanced
	use of metadata features, an example of how to extend NeXML
	with simmap data as the use case, and documentation on the 
	central S4 data structure used in the package.
- Implements the use of Title Case in the package title, as
  requested (on several occasions) by the CRAN maintainers.


v2.0.1 2014-12-26
-------

- Update DESCRIPTION to provide a standard `install.packages()` compatible repository for `rrdf`, as per request from the CRAN team.

v2.0.0   2014-12-06
---------

* add URL and BugReports to Description. [#103](https://github.com/ropensci/RNeXML/issues/103)

* for consistency with other `add_` methods, the `nexml` object is now the _last_, not the _first_, 
argument to `add_basic_meta`.  As this changes the function API, it could break code that does not
explicitly name the arguments, so we release this as 2.0.0


v1.1.3 2014-08-06
------

Minor bugfix

* Fixes typo that caused validator to fail when nexml.org couldn't be reached

v1.1.2  2014-07-19
-------

Less aggressive unit-tests

* nexml_validate now returns NULL if the validation cannot be performed. Unit tests now consider either TRUE or NULL as acceptable.   
* Just skips the uuid unit test if uuid package is not available
* Documented versioning practice in NEWS
* Unit tests relying on the Figshare API are not run (without failing) if authentication to figshare server fails
* Documentation updated to include examples for all functions

v1.1-0 2014-07-18
------

Initial Release 
