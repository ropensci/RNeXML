\name{get_metadata}
\alias{get_metadata}
\title{retrieve all available metadata}
\arguments{
  \item{object}{a representation of the nexml object from
  which the data is to be retrieved}

  \item{level}{the level in the nexml for which the
  metadata is desired}
}
\value{
  a named character vector containing all available
  metadata.  names indicate `property` (or `rel` in the
  case of links/resourceMeta), while values indicate the
  `content` (or `href` for links).
}
\description{
  retrieve all available metadata

  get all top-level metadata More extensible than hardwired
  functions
}
\seealso{
  \code{\link{get_item}}
}

