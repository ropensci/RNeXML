library(rrdf)
library(ape)

# load the graph as extracted from primates.xml by RDFa2RDFXML.xsl
graph <- load.rdf("/Users/rvosa/Documents/projects/RNeXML/inst/examples/primates_meta_xslt.xml")

# fetch the NCBI URI for the taxon that has rank 'Order', i.e. the root of the primates. The dot operator
# '.' between clauses implies a join, in this case
root <- sparql.rdf(graph, 
    "SELECT ?uri WHERE { 
        ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#rank> <http://rs.tdwg.org/ontology/voc/TaxonRank#Order> . 
        ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#toTaxon> ?uri    
    }"               
)

# define a recursive function to build newick
recurse <- function(node) {
  
    # fetch the taxonomic rank and id string
    rank_query <- paste(
        "SELECT ?rank ?id WHERE {
            ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#toTaxon> <",node,"> .
            ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#rank> ?rank
          }",
          sep=""
    );
    result <- sparql.rdf(graph,rank_query);    
  
    # if rank is terminal, return the name
    if ( result[1] == "http://rs.tdwg.org/ontology/voc/TaxonRank#Species" ) {
        name <- result[2];
        name <- gsub("^.+#","",name,perl=TRUE);
        return(name);
    }
    
    # recurse deeper
    else {
        child_query <- paste(
            "SELECT ?uri WHERE {
                ?id <http://www.w3.org/2000/01/rdf-schema#subClassOf> <",node,"> .
                ?id <http://rs.tdwg.org/ontology/voc/TaxonConcept#toTaxon> ?uri
            }",
            sep=""
        );
        children <- sparql.rdf(graph,child_query);
        return( paste( "(", paste( sapply( children, recurse ), sep="," ), ")", sep="" ) );
    }
}

# run it
newick <- paste( recurse(root), ";", sep="" );
plot( read.tree( text=newick ) );