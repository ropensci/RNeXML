\name{nexml_read}
\alias{nexml_read}
\alias{read.nexml}
\title{Read NeXML files into various R formats}
\usage{
nexml_read(x, ...)
}
\arguments{
  \item{x}{Path to the file to be read in}

  \item{...}{Further arguments passed on to XML::xmlParse}
}
\description{
Read NeXML files into various R formats
}
\examples{
f <- system.file("examples", "trees.xml", package="RNeXML")
nexml_read(f)
}

