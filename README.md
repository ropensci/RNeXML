RNeXML: The next-generation phylogenetics format comes to R
=====================================================

An R package for reading, writing, integrating and publishing data using the Ecological Metadata Language (EML) format.   

* **Note:** This package is still in active development and not yet submitted to CRAN.  Functions and documentation may be incomplete and subject to change.  
* Maintainer: Carl Boettiger
* Authors: Carl Boettiger, Scott Chamberlain, Hilmar Lapp, Kseniia Shumelchyk, Rutger Vos
* License: BSD-3 
* [Issues](https://github.com/ropensci/RNeXML/issues): Bug reports, feature requests, and development discussion.

An extensive and rapidly growing collection of richly annotated phylogenetics data is now available in the NeXML format. NeXML relies on state-of-the-art data exchange technology to provide a format that can be both validated and extended, providing a data quality assurance and and adaptability to the future that is lacking in other formats [Vos et al 2012](http://doi.org/10.1093/sysbio/sys025 "NeXML: Rich, Extensible, and Verifiable Representation of Comparative Data and Metadata."). 





Getting Started
---------------

The development version of RNeXML is [available on Github](https://github.com/ropensci/RNeXML).  With the `devtools` package installed on your system, RNeXML can be installed using:


```r
library(devtools)
install_github("RNeXML", "ropensci")
```




Read in a `nexml` file into the `ape::phylo` format:


```r
library(RNeXML)
f <- system.file("examples", "trees.xml", package="RNeXML")
tr <- nexml_read(f, "phylo")
plot(tr[[1]])
```

![plot of chunk unnamed-chunk-3](http://farm8.staticflickr.com/7366/10300074655_271d380ab6_o.png) ![plot of chunk unnamed-chunk-3](http://farm4.staticflickr.com/3671/10299999004_1cc1c4c3b3_o.png) 


Write an `ape::phylo` tree into the `nexml` format:


```r
data(bird.orders)
nexml_write(bird.orders, "test.xml")
```

```
## [1] "test.xml"
```


<!--
Extract metadata from the NeXML file: 


```r
ex2 <- nexml_read(f, "nexml")
metadata(ex2)
```

-->

--------------------------------------------

### Advanced annotation


Add metadata to a NeXML tree:  


```r
  history <- new("meta", 
      content = "Mapped from the bird.orders data in the ape package using RNeXML",
      datatype = "xsd:string", id = "meta5144", property = "skos:historyNote", 
      'xsi:type' = "LiteralMeta")
  modified <- new("meta",
                  content = "2013-10-04", datatype = "xsd:string", id = "meta5128",
                  property = "prism:modificationDate", 'xsi:type' = "LiteralMeta")
  website <- new("meta", 
                 href = "http://carlboettiger.info", 
                 rel = "foaf:homepage", 'xsi:type' = "ResourceMeta")
  nexml_write(bird.orders, 
              file = "example.xml", 
              additional_metadata = list(history, modified, website), 
              additional_namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#",
                                        prism = "http://prismstandard.org/namespaces/1.2/basic/",
                                        foaf = "http://xmlns.com/foaf/0.1/"))
```

```
## [1] "example.xml"
```

```r

```






<!--

```r
 nexml_write(bird.orders, "birds.xml", 
            author="Carl Boettiger <cboettig@ropensci.org>", 
            title = "example NeXML file for bird orders", 
            description = "Example phylogeny taken from the ape documentation showing the major bird orders", 
            in_publication = "doi:10.1010/fakedoi.1234",
            add_itis = TRUE)
```

-->




