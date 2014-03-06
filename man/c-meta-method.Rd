\docType{methods}
\name{c,meta-method}
\alias{c,meta-method}
\title{Concatenate meta elements into a ListOfmeta}
\usage{
\S4method{c}{meta}(x, ..., recursive = FALSE)
}
\arguments{
  \item{x,...}{meta elements to be concatenated, e.g. see
  \code{\link{meta}}}

  \item{recursive}{logical, if 'recursive=TRUE', the
  function descends through lists and combines their
  elements into a vector. Currently not implemented. so
  "c(meta, c(meta))" will not work.}
}
\value{
a listOfmeta object containing multiple meta elements.
}
\description{
Concatenate meta elements into a ListOfmeta
}
\examples{
c(meta(content="example", property="dc:title"),
  meta(content="Carl", property="dc:creator"))
}

