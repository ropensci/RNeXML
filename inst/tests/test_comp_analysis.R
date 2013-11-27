context("Comparative analysis")


library(RNeXML)
library(geiger)

nexml <- read.nexml(system.file("examples", "comp_analysis.xml", package="RNeXML"))
traits <- get_characters_list(nexml)
tree <- get_tree(nexml)
fitContinuous(tree, traits[[1]])
fitDiscrete(tree, traits[[2]])

