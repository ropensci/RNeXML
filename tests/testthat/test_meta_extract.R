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

test_that("we can correctly parse nested ResourceMeta annotations", {
  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  meta <- get_metadata(nex)
  lic <- dplyr::filter(meta, rel == "cc:license")$href
  testthat::expect_equal(lic, "http://creativecommons.org/publicdomain/zero/1.0/")

  # remove rows from nested meta elements (which there should be!)
  testthat::expect_true("meta" %in% colnames(meta))
  topMeta <- meta[is.na(meta[,"meta"]),]
  testthat::expect_lt(nrow(topMeta), nrow(meta))
  # there should be one dc:title at top level, and two if we include nested
  testthat::expect_gt(nrow(dplyr::filter(meta, property == "dc:title")),
                      nrow(dplyr::filter(topMeta, property == "dc:title")))
  # test that the ID referencing for self-joining is correct
  testthat::expect_true(all(meta[! is.na(meta[, "meta"]), "meta"] %in%
                              meta[! is.na(meta[, "Meta"]), "Meta"]))
  # ResourceMetas should _either_ have an href _or_ have nested meta elements
  topMeta <- cbind(topMeta, nkids = sapply(nex@meta, function(x) length(x@children)))
  rmeta <- dplyr::filter(topMeta, xsi.type == "ResourceMeta")
  testthat::expect_true(all(xor(is.na(rmeta[, "href"]), rmeta[, "nkids"] == 0)))
  testthat::expect_gt(max(rmeta[,"nkids"]), 0)
  # the sum of the children should equal the number of nested meta elements
  testthat::expect_equal(sum(rmeta[, "nkids"]),
                         nrow(meta) - nrow(topMeta))
})

test_that("we can parse LiteralMeta annotations with XML literals as values", {
  f <- system.file("examples", "phenex.xml", package="RNeXML")
  nex <- read.nexml(f)

  # the XML annotations for state elements should have been parsed
  states <- nex@characters[[1]]@format@states[[1]]@state # list of state objects
  testthat::expect_true(all(sapply(states, function(s) length(s@meta) >= 1)))
  testthat::expect_setequal(sapply(states, function(s) class(s@meta[[1]])),
                            "LiteralMeta")
  testthat::expect_true(all(sapply(states, 
                                   function(s) length(s@meta[[1]]@content)) > 0))
  testthat::expect_setequal(sapply(states, function(s) class(s@meta[[1]]@content)),
                            "XMLString")
  # the correct namespace definition should have been retained
  testthat::expect_false(any(sapply(states, 
                                   function(s) is.null(attr(s@meta[[1]]@content,
                                                            "namespaces")))))
  nsPrefix <- names(attr(states[[1]]@meta[[1]]@content, "namespaces"))
  testthat::expect_setequal(sapply(states,
                                   function(s) attr(s@meta[[1]]@content, "namespaces")),
                            get_namespaces(nex)[nsPrefix])

  # the XML literals are returned as meta content
  m <- get_metadata(nex, "characters/format/states/state")
  m_xml <- dplyr::filter(m, property == "ps:describesPhenotype")
  # should have one row for each state
  n_states <- sum(sapply(nex@characters[[1]]@format@states,
                         function(x) length(x@state)))
  testthat::expect_gte(n_states, 6) # 3 characters, at least 2 states each
  testthat::expect_length(m_xml[,1], n_states)
  # they are of type LiteralMeta
  testthat::expect_setequal(m_xml[,"xsi.type"], "LiteralMeta")
  # their content is non-empty, and a string that is valid XML
  testthat::expect_false(any(sapply(m_xml[,"content"], is.na)))
  testthat::expect_true(all(sapply(m_xml[,"content"], is.character)))
  testthat::expect_true(all(sapply(m_xml[,"content"], nchar) > 0))
  testthat::expect_true(all(sapply(m_xml[,"content"], XML::isXMLString)))
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

