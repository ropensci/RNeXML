\name{get_metadata}
\alias{get_metadata}
\title{retrieve all available metadata}
\usage{
get_metadata(object, level = c("nexml", "otus", "otu", "trees", "tree",
  "edge", "node"))
}
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
}
\seealso{
  \code{\link{get_item}}
}

