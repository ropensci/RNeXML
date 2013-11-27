context("Comparative analysis")


library(RNeXML)
library(geiger)

nexml <- read.nexml(system.file("examples", "comp_analysis.xml", package="RNeXML"))
traits <- get_characters_list(nexml)
tree <- get_tree(nexml)
fitContinuous(tree, traits[[1]])
fitDiscrete(tree, traits[[2]])


data(geospiza)
add_trees(geospiza$phy)
nexml <- add_characters(geospiza$dat)
Schluter <- knitcitations::cite("10.2307/2408428") # DOI of the original reference for this data
write.nexml(nexml, "geospiza.xml", citation=Schluter) 

