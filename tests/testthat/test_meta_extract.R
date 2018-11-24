context("extract_metadata")

nex <- add_basic_meta(
            title = "My test title",
            description = "A description of my test",
            creator = "Carl Boettiger <cboettig@gmail.com>",
            publisher = "unpublished data",
            pubdate = "2012-04-01",
            citation = citation("ape"))

test_that("get_meta() and friends have some basic parameter checking", {
  testthat::expect_error(get_meta(nex, props = c()))
  testthat::expect_error(get_meta(nex, props = NULL))
  testthat::expect_error(get_meta(nex, "foo", props = "foo"))
})

test_that("get_meta() and friends return the correct types", {
  mlist <- get_meta(nex, props = "dcterms:modified")
  testthat::expect_is(mlist, "ListOfmeta")
  testthat::expect_equal(names(mlist), "dcterms:modified")
  mlist <- get_meta(nex, props = "foo")
  testthat::expect_is(mlist, "ListOfmeta")
  testthat::expect_length(mlist, 0)
  mlist <- get_meta(nex, props = c("dcterms:modified", "dc:description"))
  testthat::expect_is(mlist, "ListOfmeta")
  testthat::expect_equal(names(mlist), c("dcterms:modified", "dc:description"))

  testthat::expect_is(get_all_meta(nex), "ListOfmeta")
  citMeta <- get_meta(nex, props = "dcterms:references")
  testthat::expect_gt(length(citMeta), 0)
  testthat::expect_is(citMeta, "ListOfmeta")
  nested <- get_all_meta(citMeta[[1]])
  testthat::expect_gt(length(nested), 0)
  testthat::expect_is(nested, "ListOfmeta")

})

test_that("we can extract metadata using the dedicated functions", {

  testthat::expect_equivalent(get_citation(nex), format(citation("ape"), "text"))
  testthat::expect_equivalent(get_license(nex), "http://creativecommons.org/publicdomain/zero/1.0/")
  m <- get_metadata(nex)
  mlist <- get_all_meta(nex)
  testthat::expect_gte(length(m[,1]), 6)
  testthat::expect_length(m[,1], length(mlist))
  mvalues <- get_metadata_values(nex, props = c("dc:creator",
                                                "dc:title",
                                                "dc:description",
                                                "dcterms:publisher",
                                                "dcterms:modified"))
  testthat::expect_equivalent(mvalues["dc:creator"], "Carl Boettiger <cboettig@gmail.com>")
  testthat::expect_equivalent(mvalues["dc:title"], "My test title")
  testthat::expect_equivalent(mvalues["dc:description"], "A description of my test")
  testthat::expect_equivalent(mvalues["dcterms:publisher"], "unpublished data")
  testthat::expect_equivalent(mvalues["dcterms:modified"], "2012-04-01")
  summary(nex)
})



test_that("we can parse literal meta nodes with literal node content", {
  
  f <- system.file("examples", "ontotrace-result.xml", package="RNeXML")
  nex <- read.nexml(f)
  out <- get_metadata(nex, level = "nexml")
  matches <- sum(grepl("Phenoscape Knowledgebase", out$content))
  testthat::expect_true(matches > 0)
  
})

test_that("we can extract all available metadata at a specified level of the DOM", {
  f <- system.file("examples", "ontotrace-result.xml", package = "RNeXML")
  nex <- read.nexml(f)

  # otu metadata
  m.otu <- get_metadata(nex, "otus/otu")
  m.taxonId <- dplyr::filter(m.otu, property == "dwc:taxonID")
  testthat::expect_equal(nrow(m.taxonId), length(nex@otus[[1]]@otu))
  testthat::expect_gt(nrow(m.otu), nrow(m.taxonId))
  taxonIDs <- get_metadata_values(nex, nex@otus[[1]]@otu, "dwc:taxonID")
  testthat::expect_length(taxonIDs, nrow(m.taxonId))
  testthat::expect_equivalent(taxonIDs, m.taxonId[,"href"])

  # character metadata
  m.c <- get_metadata(nex, "characters/format/char")
  denotes <- expand_prefix(m.c[1,"property"], nex@namespaces)
  m.denotes <- dplyr::filter(m.c,
                             expand_prefix(property, nex@namespaces) == denotes)
  testthat::expect_equal(nrow(m.c), nrow(m.denotes))
  testthat::expect_equal(nrow(m.c), length(nex@characters[[1]]@format@char))
  entityIDs <- get_metadata_values(nex, nex@characters[[1]]@format@char, denotes)
  testthat::expect_length(entityIDs, nrow(m.denotes))
  testthat::expect_equivalent(entityIDs, m.denotes[,"href"])
})


test_that("we can correctly parse nested ResourceMeta annotations", {
  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  meta <- get_metadata(nex)
  lic <- dplyr::filter(meta, property == "cc:license")$href
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

test_that("metadata tables can be requested in simplified form", {
  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  meta1 <- get_metadata(nex, simplify = FALSE)
  meta2 <- get_metadata(nex, simplify = TRUE)
  
  testthat::expect_equal(dplyr::filter(meta1, rel == "cc:license")$href,
                         dplyr::filter(meta2, property == "cc:license")$href)
  removedCols <- c("ResourceMeta", "LiteralMeta", "rel")
  testthat::expect_true(all(removedCols %in% colnames(meta1)))
  testthat::expect_false(any(removedCols %in% colnames(meta2)))
})

test_that("ID assignments are correct and complete when meta are nested", {
  f <- system.file("examples", "phenex.xml", package="RNeXML")
  nex <- read.nexml(f)
  m.otu <- get_metadata(nex, level = "otus/otu")
  # the nested meta elements are actually there
  testthat::expect_equal(sum(m.otu[,"property"] == "dwc:catalogNumber",
                             na.rm = TRUE),
                         5)
  # the ID column for the containing element (here: otu) is populated in full
  testthat::expect_false(any(is.na(m.otu[,"otu"])))
  # the IDs for the containing element are not all the same
  testthat::expect_gt(length(unique(m.otu[,"otu"])), 1)
  # expect 5 groups if we group by otu ID
  testthat::expect_length(tapply(m.otu[,"xsi.type"], m.otu[,"otu"], length), 5)
  # expect 4 rows for each otu ID
  testthat::expect_true(all(tapply(m.otu[,"xsi.type"], m.otu[,"otu"], length) == 4))
  # expect 1 LiteralMeta and 3 ResourceMeta for each otu ID
  meta.grouped <- tapply(m.otu[,"xsi.type"],
                         list(m.otu[,"otu"], m.otu[,"xsi.type"]),
                         length)
  testthat::expect_true(all(meta.grouped[,"LiteralMeta"] == 1))
  testthat::expect_true(all(meta.grouped[,"ResourceMeta"] == 3))
  # for each otu ID, two meta are nested (have IDREF to a containing meta)
  meta.nested <- m.otu[!is.na(m.otu[,"meta"]),]
  testthat::expect_true(all(tapply(meta.nested[,"meta"], meta.nested[,"otu"],
                                   length) == 2))
  # but the two nested meta for each otu have the same IDREF
  testthat::expect_true(all(tapply(meta.nested[,"meta"], meta.nested[,"otu"],
                                   function(x) length(unique(x))) == 1))
  # the IDREFs of the nested meta match up with ID of containing meta
  xsi.type <- NULL # silence R check
  meta.cont <- dplyr::filter(m.otu, 
                             xsi.type == "ResourceMeta",
                             is.na(m.otu[,"href"]))
  testthat::expect_true(all(meta.nested[,"meta"] %in% meta.cont[,"Meta"]))
  # they are all different
  testthat::expect_length(unique(meta.nested[,"meta"]), 5)
  testthat::expect_length(unique(meta.cont[,"Meta"]), 5)
  # and every containing ID is referenced by exactly one of the nested IDREFs
  testthat::expect_equal(sort(meta.cont[,"Meta"]),
                         sort(unique(meta.nested[,"meta"])))
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

