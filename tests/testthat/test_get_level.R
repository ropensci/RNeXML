testthat::context("get_level")

f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
nex <- read.nexml(f)
get_level(nex, "meta")
