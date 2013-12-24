
library(RNeXML)
library(geiger)

data(amphibia)
data(caudata)
data(chelonia)
data(geospiza)
data(primates)
data(whales)


# multiphylo, where two phylogenies have each nearly 3K taxa
runtime <- system.time(nexml_write(amphibia, file="tmp.xml")) # Slow

nexml_validate("tmp.xml")
nexml_write(trees = caudata$phy, characters = caudata$dat, file="tmp.xml")
nexml_write(trees = geospiza$phy, characters = geospiza$dat, file="tmp.xml")
nexml_write(trees = chelonia$phy, characters = chelonia$dat, file="tmp.xml")
nexml_write(trees = primates$phy, characters = primates$dat, file="tmp.xml")

# don't have a column as 
whales$dat <- whales$richness[[2]]
names(whales$dat) <- whales$richness[[1]] 
nexml_write(trees = whales$phy, characters = whales$dat, file="tmp.xml")

