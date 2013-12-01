\name{nexml_write}
\alias{nexml_write}
\alias{write.nexml}
\title{Write nexml files}
\usage{
nexml_write(x = new("nexml"), trees = NULL, characters = NULL,
  file = NULL, ...)
}
\arguments{
  \item{x}{a nexml object, or any phylogeny object (e.g.
  phylo, phylo4) that can be coerced into one. Can also be
  omitted, in which case a new nexml object will be
  constructed with the additional parameters specified.}

  \item{trees}{phylogenetic trees to add to the nexml file
  (if not already given in x) see \code{\link{add_trees}}
  for details.}

  \item{characters}{additional characters}

  \item{file}{the name of the file to write out}
}
\value{
  Writes out a nexml file
}
\description{
  Write nexml files
}
\examples{
## Write an ape tree to nexml, analgous to write.nexus:
 library(ape); data(bird.orders)
 write.nexml(bird.orders, file="example.xml")

 ## Assemble a nexml section by section and then write to file:
 library(geiger); data(geospiza)
 nexml <- add_trees(geospiza$phy) # creates new nexml
 nexml <- add_characters(geospiza$dat, nexml) # pass the nexml obj to append character data
 nexml <- add_basic_meta(nexml, title="my title", creator = "Carl Boettiger")
 nexml <- add_meta(meta("prism:modificationDate", format(Sys.Date())), nexml)
 write.nexml(nexml, file="example.xml")

 ## As above, but in one call (except for add_meta() call).
 write.nexml(trees = geospiza$phy, characters=geospiza$dat,
             title = "My title", creator="Carl Boettiger",
             file = "example.xml")

 ## Mix and match: identical to the section by section:
 nexml <- add_meta(meta("prism:modificationDate", format(Sys.Date())))
 write.nexml(x = nexml,
             trees = geospiza$phy, characters=geospiza$dat,
             title = "My title", creator="Carl Boettiger",
             file = "example.xml")
}
\seealso{
  \code{\link{add_trees}} \code{\link{add_characters}}
  \code{\link{add_meta}} \code{\link{nexml_read}}
}

