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
```




Read in a `nexml` file into the `ape::phylo` format:


```coffee
library(RNeXML)
f <- system.file("examples", "trees.xml", package="RNeXML")
nexml <- nexml_read(f)
tr <- get_tree(nexml) # or: as(nexml, "phylo")
plot(tr[[1]])
```

![plot of chunk unnamed-chunk-3](http://farm4.staticflickr.com/3755/10911281774_421cdd110d_o.png) ![plot of chunk unnamed-chunk-3](http://farm8.staticflickr.com/7434/10911144615_4370f34c3d_o.png) 


Write an `ape::phylo` tree into the `nexml` format:


```coffee
data(bird.orders)
nexml_write(bird.orders, "test.xml")
```


Extract metadata from the NeXML file: 


```coffee
birds <- nexml_read("test.xml")
get_taxa(birds)
```

```
##          otu.label          otu.label          otu.label 
## "Struthioniformes"     "Tinamiformes"      "Craciformes" 
##          otu.label          otu.label          otu.label 
##      "Galliformes"     "Anseriformes"    "Turniciformes" 
##          otu.label          otu.label          otu.label 
##       "Piciformes"    "Galbuliformes"   "Bucerotiformes" 
##          otu.label          otu.label          otu.label 
##      "Upupiformes"    "Trogoniformes"    "Coraciiformes" 
##          otu.label          otu.label          otu.label 
##      "Coliiformes"     "Cuculiformes"   "Psittaciformes" 
##          otu.label          otu.label          otu.label 
##      "Apodiformes"   "Trochiliformes"  "Musophagiformes" 
##          otu.label          otu.label          otu.label 
##     "Strigiformes"    "Columbiformes"       "Gruiformes" 
##          otu.label          otu.label 
##    "Ciconiiformes"    "Passeriformes"
```

```coffee
get_metadata(birds) 
```

```
##                                             dc:date 
##                                        "2013-11-17" 
##                                          cc:license 
## "http://creativecommons.org/publicdomain/zero/1.0/"
```


--------------------------------------------


Add basic additional metadata:  


```coffee
  nexml_write(bird.orders, file="meta_example.xml",
              title = "My test title",
              description = "A description of my test",
              creator = "Carl Boettiger <cboettig@gmail.com>",
              publisher = "unpublished data",
              pubdate = "2012-04-01")
```


By default, `RNeXML` adds certain metadata, including the NCBI taxon id numbers for all named taxa.  This acts a check on the spelling and definitions of the taxa as well as providing a link to additional metadata about each taxonomic unit described in the dataset.  


### Advanced annotation


We can also add arbitrary metadata to a NeXML tree by define `meta` objects:


```coffee
modified <- meta(property = "prism:modificationDate",
                 content = "2013-10-04")
```


Advanced use requires specifying the namespace used.  Metadata follows the RDFa conventions.  Here we indicate the modification date using the prism vocabulary. This namespace is included by default, as it is used for some of the basic metadata shown in the previous example.  We can see from this list:


```coffee
RNeXML:::nexml_namespaces
```

```
##                                                      nex 
##                              "http://www.nexml.org/2009" 
##                                                      xsi 
##              "http://www.w3.org/2001/XMLSchema-instance" 
##                                                      xml 
##                   "http://www.w3.org/XML/1998/namespace" 
##                                                     cdao 
## "http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#" 
##                                                      xsd 
##                      "http://www.w3.org/2001/XMLSchema#" 
##                                                       dc 
##                       "http://purl.org/dc/elements/1.1/" 
##                                                  dcterms 
##                              "http://purl.org/dc/terms/" 
##                                                    prism 
##         "http://prismstandard.org/namespaces/1.2/basic/" 
##                                                       cc 
##                         "http://creativecommons.org/ns#" 
##                                                     ncbi 
##                  "http://www.ncbi.nlm.nih.gov/taxonomy#" 
##                                                       tc 
##          "http://rs.tdwg.org/ontology/voc/TaxonConcept#"
```


This next block defines a resource (link), described by the `rel` attribute as a homepage, a term in the `foaf` vocabulalry.  Becuase `foaf` is not a default namespace, we will have to provide its URL in the full definition below. 


```coffee
website <- meta(href = "http://carlboettiger.info", 
                rel = "foaf:homepage")
```


Here we create a history node using the `skos` namespace.  We can also add id values to any metadata element to make the element easier to reference externally: 


```coffee
  history <- meta(property = "skos:historyNote", 
                  content = "Mapped from the bird.orders data in the ape package using RNeXML",
                  id = "meta123")
```


Once we have created the `meta` elements, we can pass them to our `nexml_write` function, along with definitions of the namespaces.  


```coffee
  nexml_write(bird.orders, 
              file = "example.xml", 
              additional_metadata = list(history, modified, website), 
              additional_namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#",
                                        foaf = "http://xmlns.com/foaf/0.1/"))
```







