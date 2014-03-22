\name{get_taxa_meta}
\alias{get_otu_meta}
\alias{get_taxa_meta}
\title{get_taxa_meta}
\usage{
get_taxa_meta(nexml, what = "href")
}
\arguments{
  \item{nexml}{a nexml object}

  \item{what}{One of href, rel, id, or xsi:type}
}
\value{
the list of metadata for each taxon
}
\description{
Retrieve metadata of all species/otus otus (operational
taxonomic units) included in the nexml
}
\examples{
data(bird.orders)
birds <- add_trees(bird.orders)
birds <- taxize_nexml(birds, "NCBI")
get_taxa_meta(birds)
get_taxa_meta(birds, 'rel')
get_taxa_meta(birds, 'id')
get_taxa_meta(birds, 'xsi:type')
}
\seealso{
\code{\link{get_item}}
}

