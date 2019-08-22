
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RNeXML <img src="man/figures/logo.svg" align="right" alt="" width="120" />

[![DOI](https://zenodo.org/badge/11856817.svg)](https://zenodo.org/badge/latestdoi/11856817)
[![Build
Status](https://api.travis-ci.org/ropensci/RNeXML.png)](https://travis-ci.org/ropensci/RNeXML)
[![Windows build
status](https://ci.appveyor.com/api/projects/status/dhiwp5blx2ns2yba/branch/master?svg=true)](https://ci.appveyor.com/project/cboettig/rnexml/branch/master)
[![CRAN
status](https://www.r-pkg.org/badges/version/RNeXML)](https://cran.r-project.org/package=RNeXML)
[![codecov.io](https://codecov.io/github/ropensci/RNeXML/coverage.svg?branch=master)](https://codecov.io/github/ropensci/RNeXML?branch=master)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/RNeXML)

  - Maintainer: Carl Boettiger
  - Authors: Carl Boettiger, Scott Chamberlain, Hilmar Lapp, Kseniia
    Shumelchyk, Rutger Vos
  - License: BSD-3
  - [Issues](https://github.com/ropensci/RNeXML/issues): Bug reports,
    feature requests, and development discussion.

An extensive and rapidly growing collection of richly annotated
phylogenetics data is now available in the NeXML format. NeXML relies on
state-of-the-art data exchange technology to provide a format that can
be both validated and extended, providing a data quality assurance and
adaptability to the future that is lacking in other formats. See [Vos et
al
2012](http://doi.org/10.1093/sysbio/sys025 "NeXML: Rich, Extensible, and Verifiable Representation of Comparative Data and Metadata.")
for further details on the NeXML format.

## How to cite

RNeXML has been published in the following article:

> Boettiger C, Chamberlain S, Vos R and Lapp H (2016). “RNeXML: A
> Package for Reading and Writing Richly Annotated Phylogenetic,
> Character, and Trait Data in R.” *Methods in Ecology and Evolution*,
> **7**, pp. 352-357.
> [doi:10.1111/2041-210X.12469](http://doi.org/10.1111/2041-210X.12469)

Although the published version of the article is pay-walled, the source
of the manuscript, and a much better rendered PDF, are included in this
package (in the `manuscripts` folder). You can also find it [freely
available on arXiv](http://arxiv.org/abs/1506.02722).

## Installation

The latest stable release of RNeXML is on CRAN, and can be installed
with the usual `install.packages("RNeXML")` command. Some of the more
specialized functionality described in the Vignettes (such as RDF
manipulation) requires additional packages which can be installed using:

``` r
install.packages("RNeXML", deps = TRUE)
```

The development version can be installed using:

``` r
remotes::install_github("ropensci/RNeXML")
```

## Getting Started

See the vignettes below for both a general quick start and an overview
of more specialized features.

  - [A Brief Introduction to
    RNeXML](https://docs.ropensci.org/RNeXML/articles/intro)
  - [RNeXML: A Package for Reading and Writing Richly Annotated
    Phylogenetic, Character, and Trait Data in
    R](https://github.com/ropensci/RNeXML/tree/master/manuscripts)
    (published in MEE).
  - [Handling Metadata in
    RNeXML](https://docs.ropensci.org/RNeXML/articles/metadata)
  - [The `nexml` S4
    Object](https://docs.ropensci.org/RNeXML/articles/S4)
  - [Semantic data & SPARQL with
    RNeXML](https://docs.ropensci.org/RNeXML/articles/sparql)
  - [Extending NeXML: an example based on
    simmap](https://docs.ropensci.org/RNeXML/articles/simmap)

-----

[![ropensci
footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
