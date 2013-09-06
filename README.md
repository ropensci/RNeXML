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


```coffee
library(devtools)
install_github("RNeXML", "ropensci")
library(RNeXML)
```




Read in a `nexml` file into the `ape::phylo` format:


```coffee
f <- system.file("examples", "trees.xml", package = "RNeXML")
tr <- nexml_read(f, "phylo")
```

```
## Warning: Returning multiple trees as a multiPhylo ojbect
```

```coffee
plot(tr[[1]])
```

![](http://farm3.staticflickr.com/2810/9690187640_15a9f1793f_o.png) 


Write an `ape::phylo` tree into the `nexml` format:


```coffee
data(bird.orders)
nexml_write(bird.orders, "test.xml")
```


<!---
NOT IMPLEMENTED YET

Extract metadata from the NeXML file: 


```coffee
ex2 <- nexml_read("treebase1.xml", "nexml")
metadata(ex2)
```


Add metadata to a NeXML tree:  


```coffee
nexml_write(bird.orders, "birds.xml", author = "Carl Boettiger <cboettig@ropensci.org>", 
    title = "example NeXML file for bird orders", description = "Example phylogeny taken from the ape documentation showing the major bird orders", 
    in_publication = "doi:10.1010/fakedoi.1234", add_itis = TRUE)
```



-->



