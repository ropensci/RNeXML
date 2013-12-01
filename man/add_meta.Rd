\name{add_meta}
\alias{add_meta}
\title{Add metadata to a nexml file}
\usage{
add_meta(meta, nexml = new("nexml"), level = c("nexml", "otus", "trees",
  "characters"), i = 1, at_id = NULL)
}
\arguments{
  \item{meta}{a meta S4 object, e.g. ouput of the function
  \code{\link{meta}}, or a list of these meta objects}

  \item{a}{nexml (S4) object}

  \item{level}{the level at which the metadata annotation
  should be added.}

  \item{i}{for otus, trees, characters: if there are
  multiple such blocks, which one should be annotated?
  Default is first/only block.}

  \item{at_id}{the id of the element to be annotated.
  Optional, advanced use only.}
}
\value{
  the updated nexml object
}
\description{
  Add metadata to a nexml file
}
\examples{
modified <- meta(property = "prism:modificationDate",
                content = "2013-10-04")
 add_meta(modified)
}
\seealso{
  \code{\link{add_trees}} \code{\link{add_characters}}
}

