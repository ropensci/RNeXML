% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_metadata.R
\name{get_metadata_values}
\alias{get_metadata_values}
\title{Get the value(s) for metadata}
\usage{
get_metadata_values(nexml, annotated = NULL, props)
}
\arguments{
\item{nexml}{a nexml object}

\item{annotated}{the nexml component object from which to obtain metadata
annotations, defaults to the nexml object itself}

\item{props}{a character vector of property names for which to extract
metadata annotations}
}
\value{
a named character vector, giving the values and names being the
property names
}
\description{
Extracts the values from the metadata annotations for the given property
or properties, and returns the result.
}
\details{
For matching property identifiers (i.e., URIs), prefixes in the input list
as well as in the \code{annotated} object will be expanded using the namespaces
of the \code{nexml} object. Names in the returned vector are mapped to the
(possibly prefixed) form in the input list.
}
