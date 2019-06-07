context("rdf")

test_that("we can extract rdf-xml", {

  skip_if_not_installed("rdflib")
  skip_if_not_installed("xslt")
  skip_on_os("solaris")
  
  rdf <- get_rdf(system.file("examples/primates.xml", package="RNeXML"))
  tmp <- tempfile()  # so we must write the XML out first
  xml2::write_xml(rdf, tmp) 
  
  graph <- rdflib::rdf_parse(tmp)
  

  expect_is(graph, "rdf")
  
  rdflib::rdf_free(graph)
})

test_that("we can parse nested meta with blank nodes", {
  
  skip_if_not_installed("rdflib")
  skip_if_not_installed("xslt")
  skip_on_os("solaris")
  
  f <- system.file("examples", "meta_example.xml", package="RNeXML")
  nex <- read.nexml(f)
  tmp <- tempfile()
  xml2::write_xml(get_rdf(f), tmp)
  triples <- rdflib::rdf_parse(tmp)
  ## Check the blank node
  df <- rdflib::rdf_query(triples, 
                          "SELECT ?s ?p ?o WHERE 
                          { ?s <http://purl.org/dc/elements/1.1/source> ?source .
                          ?source ?p ?o
                          }")
  testthat::expect_equal(dim(df), c(3,3))
})

test_that("we can perform sparql queries with rdf", {
  skip_if_not_installed("rdflib")
  skip_if_not_installed("xslt")
  # skip_on_travis()
  skip_on_os("solaris")
  
  rdf <- get_rdf(system.file("examples/primates.xml", package="RNeXML"))
  tmp <- tempfile()  # so we must write the XML out first
  xml2::write_xml(rdf, tmp) 
  
  graph <- rdflib::rdf_parse(tmp)
  
  
  root <- rdflib::rdf_query(graph, 
                    "SELECT ?uri WHERE { 
    ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#rank> <http://rs.tdwg.org/ontology/voc/TaxonRank#Order> . 
    ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#toTaxon> ?uri    
}")
  
  expect_is(root, "data.frame")
  
  rdflib::rdf_free(graph)
  
})
