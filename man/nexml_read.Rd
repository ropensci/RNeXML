\name{nexml_read}
\alias{nexml_read}
\alias{read.nexml}
\title{Read NeXML files into various R formats}
\usage{
  nexml_read(x,
    type = c("nexmlTree", "phylo", "phylo4", "ouch", "matrix", "nexml"))
}
\arguments{
  \item{x}{Path to the file to be read in}

  \item{type}{the type of object to be returned.  If the
  file contains multiple trees, all will be read into the
  appropriate multi-tree container, or else returned as a
  list of such objects.}
}
\description{
  Read NeXML files into various R formats
}
\examples{
f <- system.file("examples", "trees.xml", package="RNeXML")
nexml_read(f)
}

