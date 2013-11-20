\name{nexml_write}
\alias{nexml_write}
\alias{write.nexml}
\title{Write nexml files}
\usage{
  nexml_write(x, file = NULL, title = NULL,
    description = NULL, creator = NULL,
    pubdate = Sys.Date(), rights = "CC0", publisher = NULL,
    citation = NULL, additional_metadata = NULL,
    additional_namespaces = NULL, add_identifiers = FALSE)
}
\arguments{
  \item{x}{any phylogeny object (e.g. phylo, phylo4, or
  internal type)}

  \item{file}{the name of the file to write out}

  \item{additional_metadata}{list of}

  \item{add_identifiers}{either logical or name of an
  identifier (see details or \code{\link{add_identifiers}})
  If TRUE, default identifier (NCBI) will be added.  If
  FALSE, no identifier is added.}
}
\value{
  Writes out a nexml file
}
\description{
  Write nexml files
}
\examples{
library(ape); data(bird.orders)
 nexml_write(bird.orders, file="example.xml")

 ## Adding citation to a publication
 nexml_write(bird.orders, file="example.xml",
             citation=citation("ape"))

 ## Adding custom metadata
 history <- meta(content="Mapped from the bird.orders data in the ape package using RNeXML",
                 datatype="xsd:string",
                 id="meta5144",
                 property="skos:historyNote")
 modified <- meta(content="2013-10-04", datatype="xsd:string", id="meta5128",
                 property="prism:modificationDate")
 website <- meta(href="http://carlboettiger.info", rel="foaf:homepage")
 nexml_write(bird.orders,
             file = "example.xml",
             additional_metadata = list(history, modified, website),
             additional_namespaces = c(skos="http://www.w3.org/2004/02/skos/core#",
                                       prism="http://prismstandard.org/namespaces/1.2/basic/",
                                       foaf = "http://xmlns.com/foaf/0.1/"))
}

