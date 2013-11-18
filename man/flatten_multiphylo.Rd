\name{flatten_multiphylo}
\alias{flatten_multiphylo}
\title{Flatten a multiphylo object}
\usage{
flatten_multiphylo(object)
}
\arguments{
  \item{object}{a multiphylo object (list) containing 1 or
  multiphylo objects (lists of phylo objects)}
}
\description{
  NeXML has the concept of multiple <trees> nodes, each
  with multiple child <tree> nodes. This maps naturally to
  a list of multiphylo (heirachical multiphylo) objects.
  Sometimes this heirarchy conveys important structural
  information, so it is not discarded by default.
  Occassionally it is useful to flatten the structure
  though, hence this function.  Note that this discards the
  original structure, and the nexml file must be parsed
  again to recover it.
}

