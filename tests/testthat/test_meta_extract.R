context("extract_metadata")

nex <- add_basic_meta(
            title = "My test title",
            description = "A description of my test",
            creator = "Carl Boettiger <cboettig@gmail.com>",
            publisher = "unpublished data",
            pubdate = "2012-04-01",
            citation = citation("ape"))

test_that("we can extract metadata using the dedicated functions", {

  get_citation(nex)
  get_license(nex)
  get_metadata(nex)
  summary(nex)

  unlink("example.xml")
})



test_that("we can extract all available metadata at a specified level of the DOM", {
 get_metadata(nex) 
 get_metadata(nex, "trees") 
})


test_that("we can parse literal meta nodes with literal node content", {
  
  f <- system.file("examples", "ontotrace-result.xml", package="RNeXML")
  nex <- read.nexml(f)
  out <- get_metadata(nex, level = "nexml")
  matches <- sum(grepl("Phenoscape Knowledgebase", out$content))
  testthat::expect_true(matches > 0)
  
})

test_that("we can correctly parse ResourceMeta annotations", {
  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  meta <- get_metadata(nex)
  lic <- dplyr::filter(meta, (rel == "cc:license") | (property == "cc:license"))$href
  testthat::expect_equal(lic, "http://creativecommons.org/publicdomain/zero/1.0/")
})

test_that("we can parse nested meta with blank nodes", {

  skip_if_not(require(rdflib))
  skip_if_not_installed("xslt")
  skip_on_os("solaris")

  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  tmp <- tempfile()
  xml2::write_xml(RNeXML::get_rdf(f), tmp)
  triples <- rdflib::rdf_parse(tmp)
  ## Check the blank node
  df <- rdflib::rdf_query(triples, 
  "SELECT ?s ?p ?o WHERE 
   { ?s <http://purl.org/dc/elements/1.1/source> ?source .
     ?source ?p ?o
   }")
  testthat::expect_equal(dim(df), c(3,3))
})

