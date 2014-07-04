library(rrdf)

# load the graph as extracted from primates.xml by RDFa2RDFXML.xsl
graph <- load.rdf("/Users/rutger.vos/Documents/projects/RNeXML/inst/examples/primates_meta_xslt.xml")

# fetch the NCBI URI for the taxon that has rank 'Order', i.e. the root of the primates. The dot operator
# '.' between clauses implies a join, in this case
root <- sparql.rdf(graph, 
    "SELECT ?uri WHERE { 
        ?x <http://rs.tdwg.org/ontology/voc/TaxonConcept#rank> <http://rs.tdwg.org/ontology/voc/TaxonRank#Order> . 
        ?x <http://rs.tdwg.org/ontology/voc/TaxonConcept#toTaxon> ?uri    
    }"               
)

# compose query to fetch the children of the root
query <- paste("SELECT ?id WHERE { ?id <http://www.w3.org/2000/01/rdf-schema#subClassOf> <",root,"> }",sep="")

# run the query
children <- sparql.rdf(graph,query)

# I will leave the recursive traversal as an exercise for the reader ;-)