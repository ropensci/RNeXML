\name{meta}
\alias{meta}
\title{Constructor function for metadata nodes}
\usage{
  meta(property = character(0), datatype = character(0),
    content = character(0), rel = character(0),
    href = character(0), id = character(0),
    type = character(0), children = NULL)
}
\arguments{
  \item{property}{specify the ontological definition
  together with it's namespace, e.g. dc:title}

  \item{datatype}{optional RDFa field}

  \item{content}{content of the metadata field}

  \item{rel}{Ontological definition of the reference
  provided in href}

  \item{href}{A link to some reference}

  \item{id}{optional id element}

  \item{type}{optional xsi:type.  If not given, will use
  either "LiteralMeta" or "ResourceMeta" as determined by
  the presence of either a property or a href value.}
}
\description{
  Constructor function for metadata nodes
}
\details{
  User must either provide property+content or rel+href.
  Mixing these will result in potential garbage. The
  datatype attribute will be detected automatically from
  the class of the content argument.  Maps from R class to
  schema datatypes are as follows: character - xs:string,
  Date - xs:date, integer - xs:integer, numeric -
  xs:decimal logical - xs:boolean
}
\seealso{
  \code{\link{nexml_write}}
}

