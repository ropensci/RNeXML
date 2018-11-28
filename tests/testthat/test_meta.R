context("meta")


data(bird.orders)

test_that("We can add additional metadata", {
  ## The short version using an RNeXML API
  nex <- add_basic_meta(
              title = "My test title",
              description = "A description of my test",
              creator = "Carl Boettiger <cboettig@gmail.com>",
              publisher = "unpublished data",
              pubdate = "2012-04-01")
  write.nexml(nex, file = "meta_example.xml")

  expect_true_or_null(nexml_validate("meta_example.xml"))
  expect_is(nexml_read("meta_example.xml"), "nexml")

  unlink("meta_example.xml") # cleanup

})


test_that("We can add R bibentry type metadata", {
  ## The short version using an RNeXML API

  nex <- add_trees(bird.orders)
  nex <- add_basic_meta(nexml=nex, citation=citation("ape")) 
  write.nexml(nex, file = "meta_example.xml")

  expect_true_or_null(nexml_validate("meta_example.xml"))
  expect_is(nexml_read("meta_example.xml"), "nexml")
  unlink("meta_example.xml") # cleanup

})

test_that("Adding meta data has some basic error checking", {
  testthat::expect_error(add_meta(NULL, level = "nexml"))
  m <- meta("foo-rel", "a test")
  testthat::expect_error(add_meta(list(m, "not a meta")))
  testthat::expect_error(add_meta(m, level = "foo"))
})

test_that("ResourceMeta maps rel to property for simplicity of API", {
  m <- meta(rel = "foo-rel", href="http://example.com/")
  testthat::expect_is(m, "ResourceMeta")
  testthat::expect_error(m@property)
  testthat::expect_equal(slot(m, "rel"), "foo-rel")
  testthat::expect_equal(slot(m, "property"), "foo-rel")
  testthat::expect_error(m@property <- "bar-rel")
  testthat::expect_silent(slot(m, "property") <- "bar-rel")
  testthat::expect_equal(slot(m, "rel"), "bar-rel")
  testthat::expect_equal(slot(m, "property"), "bar-rel")
})

test_that("ResourceMeta maps meta to children for simplicity of API", {
  nested <- c(meta(property = "foo-prop", content = "foo"),
              meta(property = "bar-prop", content = "bar"))
  m <- meta(rel = "foo-rel", children = nested)
  testthat::expect_is(m, "ResourceMeta")
  testthat::expect_error(m@meta)
  testthat::expect_length(m@children, 2)
  testthat::expect_length(slot(m, "meta"), 2)
  testthat::expect_equal(slot(m, "meta"), m@children)
  testthat::expect_equivalent(sapply(slot(m, "meta"), slot, "content"),
                              c("foo", "bar"))
  m <- new("ResourceMeta", rel = "baz-rel") # meta() returns NULL if no "content"
  testthat::expect_length(m@children, 0)
  testthat::expect_error(m@meta <- nested)
  testthat::expect_silent(slot(m, "meta") <- nested)
  testthat::expect_length(slot(m, "meta"), 2)
  testthat::expect_equal(slot(m, "meta"), m@children)
  testthat::expect_equivalent(sapply(slot(m, "meta"), slot, "content"),
                              c("foo", "bar"))
})

test_that("Citation BibEntry objects are transformed to structured metadata", {
  nexml_cit <- nexml_citation(citation("RNeXML"))
  testthat::expect_is(nexml_cit, "list")
  testthat::expect_length(nexml_cit, 1)
  citrec <- nexml_cit[[1]]
  testthat::expect_is(citrec, "meta")
  testthat::expect_true(.hasSlot(citrec, "children"))
  testthat::expect_gt(length(citrec@children), 10)
  citxml <- as(citrec, "XMLInternalNode")
  testthat::expect_length(XML::xmlChildren(citxml), length(citrec@children))
  testthat::expect_equal(slot(citrec, "xsi:type"), "ResourceMeta")
  testthat::expect_false(grepl("bibliographicCitation", slot(citrec, "rel")))
  propvals <- sapply(citrec@children, 
                     function(x) if (.hasSlot(x, "property")) x@property else NULL)
  testthat::expect_true(any(grepl("bibliographicCitation", propvals)))
})

test_that("We can add additional metadata", {
  ## The short version using an RNeXML API
  nex <- add_trees(bird.orders)
  nex <- add_basic_meta(nexml = nex, citation=citation("ape")) 

  history <- meta(property = "skos:historyNote",
                  content = "Mapped from the bird.orders data in the ape package using RNeXML",
                  id = "meta5144")
  modified <- meta(property = "prism:modificationDate",
                  content = "2013-10-04")
  website <- meta(href = "http://carlboettiger.info", 
                 rel = "foaf:homepage")

  nex <- add_meta(list(history, modified, website), 
                  nex, 
                  namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#",
                                 prism = "http://prismstandard.org/namespaces/1.2/basic/", # check and remove duplicates
                                 foaf = "http://xmlns.com/foaf/0.1/"))

  nexml_write(nex, file = "meta_example.xml")  

  expect_true_or_null(nexml_validate("meta_example.xml"))
  expect_is(nexml_read("meta_example.xml"), "nexml")
  unlink("meta_example.xml") # cleanup

})


test_that("We can directly add additional metadata at arbitrary level", {
  nex <- add_trees(bird.orders)
  modified <- meta(property = "prism:modificationDate",
                  content = "2013-10-04")

  # For trees block, using API function
  nex <- add_meta(modified, nexml = nex, level = "trees")
  get_metadata(nex, "trees") %>%
    dplyr::filter(property == "prism:modificationDate") %>% 
    dplyr::select(content) -> 
    tmp
  expect_identical(tmp[[1]], modified@content)

  # For trees block, accessing object hierarchy directly
  nex@trees[[1]]@meta <- c(modified)
  get_metadata(nex, "trees") %>%
    dplyr::filter(property == "prism:modificationDate") %>%
    dplyr::select(content) ->
    tmp
  expect_identical(tmp[[1]], modified@content)

  # For otus block, using API function
  nex <- add_meta(modified, nexml = nex, level = "otus")
  get_metadata(nex, "otus") %>%
    dplyr::filter(property == "prism:modificationDate") %>%
    dplyr::select(content) ->
    tmp
  expect_identical(tmp[[1]], modified@content)

  # For otus block, accessing object hierarchy directly
  nex@otus[[1]]@meta <- c(modified)
  get_metadata(nex, "otus") %>%
    dplyr::filter(property == "prism:modificationDate") %>%
    dplyr::select(content) ->
    tmp
  expect_identical(tmp[[1]], modified@content)

  # For characters block, using API function
  testthat::expect_error(add_meta(modified, nexml = nex, level = "characters"))
  # one needs to have called add_characters() first, as otherwise there won't
  # be a characters block
  cdata <- data.frame(trait1 = c(1, 1, 0),
                      row.names = c("Aus bus", "Aus foo", "Aus bar"))
  nex <- add_characters(cdata, nexml = nex)
  # now this should work
  testthat::expect_silent(nex <- add_meta(modified, nexml = nex, level = "characters"))
  get_metadata(nex, "characters") %>%
    dplyr::filter(property == "prism:modificationDate") %>%
    dplyr::select(content) ->
    tmp
  expect_identical(tmp[[1]], modified@content)

  # For otus block, accessing object hierarchy directly
  nex@characters[[1]]@meta <- c(modified)
  get_metadata(nex, "characters") %>%
    dplyr::filter(property == "prism:modificationDate") %>%
    dplyr::select(content) ->
    tmp
  expect_identical(tmp[[1]], modified@content)

})



test_that("We can directly add additional metadata using concatenation notation", {
  nex <- add_trees(bird.orders)
  modified <- meta(property = "prism:modificationDate",
                  content = "2013-10-04")
  website <- meta(href = "http://carlboettiger.info", 
                 rel = "foaf:homepage")

  nex@trees[[1]]@meta <- c(modified)         # we can add just one element 
  nex@trees[[1]]@meta <- c(modified,website) # or more than one element

  get_metadata(nex, "trees") %>%
    dplyr::filter(property == "prism:modificationDate") %>% 
    dplyr::select(content) -> 
    tmp
  
  expect_identical(tmp[[1]], modified@content)
})





test_that("We can add arbitrary metadata", {
  rdfa <- '<xhtml:div typeof="foaf:Person" about="http://carlboettiger.info#me">
             <a rel="foaf:account" href="https://twitter.com/cboettig">twitter</a> 
             <a rel="foaf:account" href="https://github.com/cboettig">github</a>
           </xhtml:div>'
  parsed <- xmlRoot(xmlParse(rdfa))
  arbitrary_rdfa <- meta(property="eml:additionalMetadata", content="additional metadata", children = parsed)

  nex <- add_meta(arbitrary_rdfa, 
                  namespaces = c(foaf = "http://xmlns.com/foaf/0.1/", 
                                 eml = "eml://ecoinformatics.org/eml-2.1.1", 
                                 xhtml = "http://www.w3.org/1999/xhtml"))

  nexml_write(nex, file = "example.xml")


  
  expect_is(nexml_read("example.xml", "nexml"), "nexml")
  unlink("example.xml") # cleanup

})


test_that("we can write numeric types of meta elements and get correct datatype", {
          m <- meta(property="numericTest", content = 3.141)
          expect_is(m@content, "character")
          expect_match(m@datatype, ".*:decimal")
})

test_that("we can serialize nested meta elements", {

  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  s <- nex@meta[sapply(nex@meta,
                       function(x)
                         ("rel" %in% slotNames(x)) &&
                         (x@rel == "dc:source"))]
  out <- as(s$meta, "XMLInternalNode")
  out_m <- sapply(xmlChildren(out), xmlAttrs)
  testthat::expect_equal(dim(out_m), c(3, 3))
})
