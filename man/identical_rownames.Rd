\name{identical_rownames}
\alias{identical_rownames}
\title{Get character data.frame, accepts either nexml object, or a list of data.frames}
\usage{
identical_rownames(x)
}
\description{
  Get character data.frame, accepts either nexml object, or
  a list of data.frames
}
\examples{
\dontrun{
# library(RNeXML)
f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
nex <- read.nexml(f)
RNeXML:::get_characters(nex)

# with different row.names
char_list <- get_characters_list(nex)
row.names(char_list[[1]])[1:3] <- c("taxon_18","taxon_20","taxon_30")
RNeXML:::get_characters(char_list
}
}

