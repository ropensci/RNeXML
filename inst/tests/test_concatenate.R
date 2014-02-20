context("concatenate method")


test_that("we can concatenate two files with unique ids", {
 f1 <- system.file("examples", "trees.xml", package="RNeXML")
 f2 <- system.file("examples", "comp_analysis.xml", package="RNeXML")
 nex1 <- read.nexml(f1)
 nex2 <- read.nexml(f2)
 expect_is(c(nex1, nex2), "nexml")

})


test_that("we get an error if the files to be concatenated have non-unique ids", {
 f1 <- system.file("examples", "trees.xml", package="RNeXML")
 nex1 <- read.nexml(f1)
 nex2 <- read.nexml(f1)
 expect_error(c(nex1, nex2),"ids are not unique")
})
