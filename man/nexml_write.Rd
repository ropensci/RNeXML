\name{nexml_write}
\alias{nexml_write}
\alias{write.nexml}
\title{Write nexml files}
\usage{
nexml_write(x, file = "nexml.xml", title = NULL, description = NULL,
  creator = NULL, pubdate = Sys.Date(), rights = "CC0",
  publisher = NULL)
}
\arguments{
  \item{x}{any phylogeny object (e.g. phylo, phylo4, or
  internal type)}

  \item{file}{the name of the file to write out}
}
\value{
Writes out a nexml file
}
\description{
Write nexml files
}

