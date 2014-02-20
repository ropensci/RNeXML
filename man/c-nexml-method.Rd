\docType{methods}
\name{c,nexml-method}
\alias{c,nexml-method}
\title{Concatenate nexml files}
\usage{
\S4method{c}{nexml}(x, ...)
}
\arguments{
  \item{a}{nexml object, e.g. from write.nexml() or
  read.nexml()}

  \item{...}{additional nexml objects to be concatenated.
  must have unique ids on all elements}
}
\value{
a concatenated nexml file
}
\description{
Concatenate nexml files
}
\examples{
f1 <- system.file("examples", "trees.xml", package="RNeXML")
f2 <- system.file("examples", "comp_analysis.xml", package="RNeXML")
nex1 <- read.nexml(f1)
nex2 <- read.nexml(f2)
nex <- c(nex1, nex2)
}

