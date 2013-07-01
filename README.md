# RNeXML Project

R package that will allow access to phyloinformatic data in NeXML format. The package should add new functionality to R such as the possibility to manipulate NeXML objects in more various and refined way and compatibility with 'ape' objects.

-----------------------


### Installation procedure 

This installation procedure for RNeXML works only on Linux systems and has been tested on Ubuntu. It is assumed that a newer version of R installed (>= 2.15.1)

To install package locally (by now this package though CRAN is not available yet), please download all RNeXML repo archive, unpack it and go to the RNeXML root directory.
Then execute next command:

    R CMD INSTALL RNeXML

In case of succesful installation you will see appropriate messages like:
    
    * DONE (RNeXML)

To remove package you should use

    R CMD REMOVE RNeXML

### Project goals

The main task is to build a library that reads and writes NeXML data using one of the popular XML parsers for R. When reading the data, it should be possible to fetch trees, alignments and taxa as objects with a familiar "feel", e.g. by making them compatible with "ape" objects. Likewise, when writing, users should be able to pass in objects that come from other libraries. In addition, metadata annotions should be exposed as simple key/value pairs (with some extra syntax sugar) so that users can take advantage of the full richness that the NeXML standard enables.

