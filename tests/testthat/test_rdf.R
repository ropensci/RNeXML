context("rdf")

has_rdf <- require(rdflib)

test_that("we can extract rdf-xml", {

  skip_if_not(has_rdf)
  skip_on_os("solaris")
  
  rdf <- get_rdf(system.file("examples/primates.xml", package="RNeXML"))
  tmp <- tempfile()  # so we must write the XML out first
  xml2::write_xml(rdf, tmp) 
  
  graph <- rdf_parse(tmp)
  

  expect_is(graph, "rdf")
  
  rdf_free(graph)
})

test_that("we can perform sparql queries with rdf", {
  skip_on_travis()
  skip_on_os("solaris")
  
  rdf <- get_rdf(system.file("examples/primates.xml", package="RNeXML"))
  tmp <- tempfile()  # so we must write the XML out first
  xml2::write_xml(rdf, tmp) 
  
  graph <- rdf_parse(tmp)
  
  
  root <- rdf_query(graph, 
                    "SELECT ?uri WHERE { 
    ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#rank> <http://rs.tdwg.org/ontology/voc/TaxonRank#Order> . 
    ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#toTaxon> ?uri    
}")
  
  expect_is(root, "data.frame")
  
  rdf_free(graph)
  
})
