\name{add_characters}
\alias{add_characters}
\title{Add character data to a nexml object}
\usage{
add_characters(x, nexml = new("nexml"), append_to_existing_otus = FALSE)
}
\arguments{
  \item{x}{character data, in which character traits labels
  are column names and taxon labels are row names.  x can
  be in matrix or data.frame format.}

  \item{nexml}{a nexml object, if appending character table
  to an existing nexml object.  If ommitted will initiate a
  new nexml object.}
}
\description{
  Add character data to a nexml object
}

