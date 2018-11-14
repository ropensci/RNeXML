context("serializing")


## More tests at lower-level serializing from S4 to XML in inheritance.R

test_that("We can serialize ape to S4 RNeXML into valid NeXML",{
  data(bird.orders)


  nexml <- as(bird.orders, "nexml") 

  as(nexml, "XMLInternalNode")
  ###  Higher level API tests
  nexml_write(bird.orders, file="test.xml")
  expect_true_or_null(nexml_validate("test.xml"))

 ##  Clean up
  unlink("test.xml")

  })


test_that("We can serialize parsed NeXML to S4 RNeXML into valid NeXML",{
  root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
  tree <- as(root, "nexml")
  nexml_write(tree, file="test.xml")

  ## validate
  expect_true_or_null(nexml_validate("test.xml"))

  ##  Clean up
  unlink("test.xml")

  })

test_that("We can correctly serialize XML literals as metadata", {
  nex1 <- read.nexml(system.file("examples", "phenex.xml", package="RNeXML"))
  m_xml1 <- get_metadata(nex1, "characters/format/states/state") %>%
    dplyr::filter(property == "ps:describesPhenotype")
  # write it out and then read it back in
  write.nexml(nex1, file="test.xml")
  expect_true_or_null(nexml_validate("test.xml"))
  nex2 <- read.nexml("test.xml")
  m_xml2 <- get_metadata(nex2, "characters/format/states/state") %>%
    dplyr::filter(property == "ps:describesPhenotype")
  # the XML literals in the form of string values should be the same
  testthat::expect_true(all(dplyr::arrange(m_xml1, state)[,"content"] ==
                              dplyr::arrange(m_xml2, state)[,"content"]))

  ## clean up
  unlink("test.xml")
})

#root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
#tree <- as(root, "nexml")
#tree@trees[[1]]@tree[[1]]@node[[4]]@meta
#as(root[["trees"]][["tree"]][[4]][["meta"]], "meta")


