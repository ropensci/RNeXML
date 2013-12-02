context("extract_metadata")

require(RNeXML)
nex <- add_basic_meta(
            title = "My test title",
            description = "A description of my test",
            creator = "Carl Boettiger <cboettig@gmail.com>",
            publisher = "unpublished data",
            pubdate = "2012-04-01",
            citation = citation("ape"))

test_that("we can extract various metadata", {
  write.nexml(nex, file = "example.xml")

  ## FIXME add the appropriate expect_that checks here
  nex <- read.nexml("example.xml")
  get_citation(nex)
  get_license(nex)
  get_metadata(nex)
  summary(nex)

  unlink("example.xml")
})


