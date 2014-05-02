---
layout: preprint
#layout: review, 11pt
title: "RNeXML: Parsing and Serializing the Next Generation of Phyloinformatic Data in R"
author: 
  - name: Carl Boettiger
    affiliation: cstar
    email: cboettig@gmail.com
    footnote: Corresponding author
  - name: Hilmar Lapp
    affiliation: NESCent
  - name: Scott Chamberlain
    affiliation: SimonFraser
  - name: Rutger Vos
    affiliation: NBC
address: 
  - code: cstar
    address: | 
      Center for Stock Assessment Research, 
      Department of Applied Math and Statistics, 
      University of California, Mail Stop SOE-2,
      Santa Cruz, CA 95064, USA
  - code: NESCent
    address: | 
      National Evolutionary Synthesis Center, Durham, NC, USA
  - code: SimonFraser
    address: |
      Department of Biology, Simon Fraser University, Burnaby, Canada
  - code: NBC
    address: | 
      NBC Naturalis, Leiden, the Netherlands
abstract: | 
      1. NeXML promises to be the next generation of phylogenetic
      informatics data and metadata exchange format, offering substantial
      improvements in reliability, extensibility, and richness over
      earlier standards such as NEXUS.

      2. In this paper we present the RNeXML package which provides
      a bridge between this rich and reliable data format and the
      extensive library of phylogenetic tools already available in R,
      with a particular emphasis on comparative methods. 

      3. RNeXML allows R users to both read and write NeXML files. Yet
      RNeXML is much richer than a basic parser: here we illustrate
      how the package facilitates adding and manipulating semantic 
      annotations.

      4. An essential feature of NeXML is to provide both the flexibility
      to be continually extended to meet the emerging needs of the phylogenetics
      community while remaining backwards-compatible with software developed
      for earlier versions.  Here we illustrate how RNeXML makes it easy
      to extend NeXML in this way.  
      
      5. RNeXML is perfectly suited for users to preserve and maintain their
      personal archives of phylogenetic and comparative trait data in consistent,
      query-able, and cross-platform format, or leverage the rapidly growing 
      public community archives of rich NeXML files.  



---

<!-- Emphasize: 
1) storing character data and tree data together, convenient for the comparative methods community
2) Adding metadata, including automated addition of metadata and semantic utilities 
3) Extending the NeXML standard  

-->





Though R provides the most extensive and widely used collection of
comparative phylogenetic methods tools, R users lack a convenient,
extensible format to exchange and archive comparative methods data.


## Background

### Challenges with existing formats
See [Vos _et al._ 2012].  

### Challenges with R formats

- Most R users rely on the NEXUS inherits the limitations of NEXUS file format

See [phylobase vignette, 2010]


<!--
A summar of some phylogenetic formats in R:

| Class     | R package   | R Object 
|-----------|-------------|-------
| phylo     | ape         | S3 
| phylo4    | phylobase   | S4
| phylo4d   | adephylo    | S4
| ouchtree  | ouch        | S4 
| treeshape | apTreeshape | list 
-->

### NeXML

See [Vos _et al._ 2012].

Mention of data sharing challenges and opportunities.  See [Drew et al 2013]




Why RNeXML?

* Why NeXML -- because NEXUS standard isn't a standard.  Developers extend
it arbitrarily to meet their needs, yielding mutually incompatible dialects. 
For example, NEXUS cannot represent horizontal gene transfer, ambiguous
use of symbols, cannot be extended reliably, and there is no way to
validate that it is proper NEXUS.  NeXML addresses all of these problems
[Vos _et al._ 2012] with a state-of-the-art data management format.

* Why RNeXML: Huge ecosystem of phylogenetics packages. (1) Read NeXML.
All would benefit by being able to read NeXML, both as a robust standard
free of the pitfalls of NEXUS, and more importantly, as a much richer
source of data thanks to its extendable annotation model.  (2) Write
NeXML.  R packages and R users add rich information to the phylogenies
they work with, but need a platform independent data exchange format to
share and archive this. In particular, we illustrate how NeXML can
be used to distribute both phylogenies and character trait data used in 
many popular phylogenetic comparative methods tools in R.  (3) Annotate 
and extend nexml. `RNeXML` provides convenient tools for adding metadata
to NeXML files, including the automated generation of metadata and examples
of how to extend the NeXML standard to represent new data features without
breaking validation or backwards compatability of the file type.  



## Introduction to RNeXML


A nexml file can be thought of as having three components: (1) phylogeny
data (2) character-trait data, and (3) metadata. `RNeXML` seeks to
provide a seemless way to convert these to and from their corresponding
R object representations, such as `ape::phylo` phylogenies, character
matrices (`data.frames`) and metadata lists (or for advanced users,
`XMLInternalDocument` and `rrdf` triplestores). Metadata can provide
information about the study in which the phylogeny was generated or
applied (such as authors, citations to publications, timestamps, and even
descriptions of methods), but can also be used to annotate individual
data components.  We illustrate how metadata annotations can be added to
the operational taxonomic units of the phylogeny and can be used to extend
the data representation to new uses such as stochastic character maps.

The current stable version of the `RNeXML` package can be installed from
the CRAN library using the standard installation method <!-- Actually not yet --> 

```coffee
install.packages("RNeXML")
```

The latest stable development version can be installed from Github using the `devtools` package:

```coffee
library(devtools)
install_github("RNeXML", "ropensci")
```


## Working with phylogenetic trees

#### Read methods




We begin by reading in an example NeXML file provided with the package.  


```coffee
f <- system.file("examples", "trees.xml", package="RNeXML")
nex <- nexml_read(f)
```


(For convenience `nexml_read` is also aliased as `read.nexml`, for
users familiar with the somewhat analogous `read.nexus` function).
The resulting `nex` object is an R object representation of the NeXML.
From this object we can extract any phylogenies it contains in the
`ape::phylo` format:


```coffee
phy <- get_trees(nex)
```


We can then leverage the rich suite of methods available from the R
phylogenetics community (including coercion into other representations,
such as `ouch` [@ouch] and `phylobase` [@phylobase] phylogeny
formats). For instance, here we use the standard plotting function from
the `ape` package to plot the resulting `ape::phylo` object returned by
`get_trees`.




```coffee
plot(phy)
```


NeXML can not only contain multiple phylogenetic trees in a single file,
but can also group sets of trees into separate blocks.  The `get_trees`
function attempts to return the most sensible R object for NeXML: if
only a single tree is available, it will return an `ape::phylo` object;
if multiple trees are available in a single `trees` block, it will return
an `ape::multiPhylo` object (a list of `phylo` objects).  If multiple
blocks are available, it will return a list of `multiPhylo` blocks.
While most users will find this behavior convenient, there are times
(such as looping over many NeXML files) when having a variable return
type is undisirable.  To always get back a list of `multiPhylo` objects,
even when only one tree is available, use `get_trees_list()`.

<!-- have we already said all this above?-->  <!-- no, seems appropriate to repeat here, -scott --> The ability to read in
phylogenetic trees in the `nexml` format opens up a wide and rapidly
growing array of resources to the R user.  For instance, a user can access
all the phylogenies available in TreeBASE through the `nexml` format.
`RNeXML` can read directly from a URL:


```coffee
nex = nexml_read("https://raw.github.com/rvosa/supertreebase/master/data/treebase/S100.xml")
```


Previously, this was possible using the `treebase` package [@treebase]
for R, which relied on the NEXUS parser provided in the `ape` package.
Due to inconsistencies present in the NEXUS standard [Vos _et al._
2012], a significant number <!-- State explicitly --> of TreeBASE NEXUS
files could not be read in this way.  Using the more precisely defined
NEXML standard, every valid NeXML file on TreeBASE can now successfully
be parsed to R objects.



#### Character data 

Like NEXUS files, NeXML files can also contain character data.  While
most researchers will be familiar with the use of character matrices in
NEXUS files as a container for molecular sequence data used to infer
the phylogenetic tree, we find this ideal for character data used in
comparative methods popular in R. Methods to get extract character data
work much like those for the phylogenetic data:


```coffee
comp_analysis <- system.file("examples", "comp_analysis.xml", package="RNeXML")
nex <- nexml_read(comp_analysis)
get_characters(nex)
```

```
         log snout-vent length reef-dwelling
taxon_8                 -3.278             0
taxon_9                  2.096             1
taxon_10                 3.137             0
taxon_1                  4.753             1
taxon_2                 -2.762             0
taxon_3                  2.105             0
taxon_4                 -4.950             0
taxon_5                  1.271             1
taxon_6                  6.259             1
taxon_7                  0.910             1
```


Returns a `data.frame` with columns as characters and rows as
taxa. Continuous and discrete characters are represented as seperate
blocks in NeXML files, but will be combined as seperate columns of a
single data frame by `get_characters()` if they correspond to the same
taxa. The `get_characters_list()` function will consistently recieve
a list of data frames corresponding to each character matrix block of
the NeXML.


#### Write methods 

Generating NeXML files from R is likewise straight forward.  Here we
write a phylogeny from the `ape::phylo` format out to a NeXML file:


```coffee
data(bird.orders)
nexml_write(bird.orders, file = "birds.xml")
```

```
[1] "birds.xml"
```


The first argument to `nexml_write` can be a `phylo` phylogeny,
`multiPhylo` list of phylogenies, or a `nexml` object -- anything that
can be coerced into the `nexml` class.  As we have seen, NeXML files
can contain multiple phylogenetic trees, as well as character matrices.
We can add both trees  and character data using `nexml_write`,


```coffee
library(geiger)
data(geospiza)
nexml_write(trees = geospiza$phy, characters = geospiza$dat, file="geospiza.xml")
```

```
[1] "geospiza.xml"
```



NeXML files can also be built up bit by bit using the `add_characters`,
`add_trees`, and `add_meta`.  Each of these functions can add data to an
existing `nexml` object or create a new nexml object if none is provided.
Here we first create a `nexml` object containing the phylogeny data,
and then add the character data to it:


```coffee
geiger_nex <- add_trees(geospiza$phy)
geiger_nex <- add_characters(geospiza$dat, geiger_nex)
```


The data need not share the same taxa.  We can append additional
phylogenies and characters corresponding to different taxa:


```coffee
data(primates)
geiger_nex <- add_trees(primates$phy, geiger_nex)
geiger_nex <- add_characters(primates$dat, geiger_nex)
```


This allows us to include multiple trees and character matrices to a single NeXML file.  
<!-- We can add relevant metadata to each tree, such as the citation information 
     indicating the paper in which it was first described.  -->


### Metadata

At the heart of the NeXML standard is the ability to encode arbitrary
metadata in precisely specified, machine-readable formats that maintain
backwards compabitibilty with any existing NeXML parser. NeXML uses the
"Resource Description Framework in Annotations", or RDFa [W3C 2013],
as described in [Vos et al 2011].  These annotations can be used to add 
information to any particular element of the data, (a tree, node, edge,
or taxonomic unit; a character matrix or specific trait, or even another 
metadata annotation or the NeXML data file as a whole). `RNeXML` provides
a range of tools for accessing metadata embedded in a NeXML file.  



### Writing NeXML metadata

Researchers cannot take advantage of NeXML metadata manipulation if
there is no metadata there.  The burden of adding good metadata remains a
significant barrier to increased sharing and reuse of scientific research
[Tenopir et al. 2011].  `RNeXML` seeks to reduce the burden of adding
high quality, machine-readable metadata to NeXML files. This occurs on
several levels:

- Automatic annotation.  

`RNeXML` automatically adds basic metadata by default such as a timestamp
of when the tree was produced and an open data license (CC0, compatible
with deposition on Dryad data repository), which can be configured
as needed.  To adjust these and other common metadata fields that should 
be part of any data deposition, `RNeXML` provides the `add_basic_meta()`
function.  This function does not assume any familiarity with the concepts
of namespaces or other more advanced metadata tools discussed later.  The
`add_basic_meta` takes an existing `nexml` object (or will create a new one
if none is provided, like the other `add_` functions), and any of the following
fields: `title`, `description`, `creator`, `pubdate`, `rights`, `publisher`,
`citation`.  Additional fields may be added to subsequent versions, see
`?add_basic_metadata` for the most up-to-date information.  Here we create
an new `nexml` object with the basic metadata for the `bird.orders` data
provided in the `ape` package:


```coffee
data(bird.orders)
birds <- add_trees(bird.orders)
birds <- add_basic_meta(birds,
  title = "Phylogeny of the Orders of Birds From Sibley and Ahlquist",

  description =" This data set describes the phylogenetic relationships of the
     orders of birds as reported by Sibley and Ahlquist (1990). Sibley
     and Ahlquist inferred this phylogeny from an extensive number of
     DNA/DNA hybridization experiments. The ``tapestry'' reported by
     these two authors (more than 1000 species out of the ca. 9000
     extant bird species) generated a lot of debates.

     The present tree is based on the relationships among orders. The
     branch lengths were calculated from the values of Delta T50H as
     found in Sibley and Ahlquist (1990, fig. 353).",

  citation = "Sibley, C. G. and Ahlquist, J. E. (1990) Phylogeny and
     classification of birds: a study in molecular evolution. New
     Haven: Yale University Press.",

  creator = "Sibley, C. G. and Ahlquist, J. E.")
```


Citations can also be added using R's `bibentry` type, which allows
a user to add citations to R packages, 


```coffee
birds <- add_basic_meta(birds, citation = citation("ape"))
```


or to papers by simply using the object's DOI:


```coffee
library("knitcitations")
geiger_nex <- add_basic_meta(geiger_nex, citation=cite("10.2307/2408428"))
```


(Which is the paper on which the `geospiza` phylogeny from the geiger dataset is based)


`RNeXML` also provides various functions to assist in automatically adding
other more specific metadata to various elements of the NeXML data.  



For instance, misspellings or inconsistent use of taxonomic names in phylogenetic
data are a common source of frustration in reusing phylogenetic trees. By 
associating the taxonomic labels against a specific, centralized database
we can catch typographical errors and indicate more precisely which classification
we are appealing to in the event of conflicting taxonomic definitions.  In this 
example, we indicate that we intend the bird orders listed in the tree to correspond
to the NCBI definitions 


```coffee
birds <- taxize_nexml(birds, "NCBI")
```


This function uses the `taxize` R library ([Chamberlain 2013]) to check each taxon
label against the NCBI database.  If a unique match is found, a metadata annotation
is added to the taxon providing the NCBI identification number to the taxonomic unit.
If no match is found, the user is warned to check for possible typographic errors in
the taxonomic labels provided.  <!-- what if multiple NCBI taxon IDs are found, e.g. 'Aotus'? -->


### Custom metadata extensions

So far our examples have relied on functions that have a built-in understanding of the 
kind of metadata being provided, and can thus generate the appropriate metadata with 
little further guidence.  In order to be machine-readable and understandable, 
NeXML requires all metadata annotations follow the RDFa format which explicitly declares 
the meaning of the annotation in a way a computer can understand.  For instance, the 
title we gave to the `bird.orders` phylogeny appears in the NeXML as:

```xml
<meta id="m2" property="dc:title" datatype="xsd:string"
      content="Phylogeny of the Orders of Birds From Sibley and Ahlquist"/> 
```

We see the title appears as the `content` of this `meta` element.  The `id` attribute
provides us with a unique way to refer to this meta element in this document.  The `datatype`
tells us that the content is a text string (as opposed to, say, a numerical or boolean value).  
The `property` is the attribute that most interests us here; it tells us that the content 
is a title.  The prefix `dc` indicates the vocabulary (called a namespace in XML) to which 
the word `title` belongs.  The prefix is defined elsewhere in the NeXML file, as we can see with:



```coffee
prefixes <- get_namespaces(birds)
prefixes["dc"]
```

```
                                dc 
"http://purl.org/dc/elements/1.1/" 
```


This URL links us to a webpage providing both a human and machine-readable version of precisely
what the term `title` means, showing us that it refers to `title` in the sense of a creative work, 
rather than the title of a person, a legal title, the verb title, etc.  This precision can allow
us to perform precise computer reasoning on metadata, without relying on heuristics or assumptions,
as we illustrate later.  For the moment though, the need to refer to an explicit controlled vocabulary (or
ontology) with a url, saying "dc:title" rather than just saying the property is "title", is something
of a burden in creating metadata.  The previous examples involve functions that already know what 
kind of metadata is being provided (a title, a data creator, etc), and so can provide the appropriate
vocabulary automatically.  The main advantage of linking to an external vocabulary, however, is the
ability to express concepts that are not already "built in" to the data model.  `RNeXML` provides 
tools to generate arbitrary metadata annotations given an external vocabulary, such as we illustrate
here.  


From the list of namespaces in `prefixes`, we can follow links a handful of established vocabularies
and find terms that are not included in `add_basic_meta`.  For instance, in the `prism` prefix, 


```coffee
prefixes["prism"]
```

```
                                           prism 
"http://prismstandard.org/namespaces/1.2/basic/" 
```


we see that there is a term for `modificationDate` for a file.  We create a `meta` element 
containing this annotation using the `meta` function:


```coffee
modified <- meta(property = "prism:modificationDate", content = "2013-10-04")
```


We can add this annotation to our existing `birds` nexml file using the `add_meta` function.
Because we do not specify a level, it is added to the root node, refering to the nexml file as a whole.  


```coffee
birds <- add_meta(modified, birds) 
```


The built-in vocabularies are just the tip of the iceberg of established vocabularies. Here 
we add an annotation from the `skos` namespace which describes the history of where the
data comes from:


```coffee
history <- meta(property = "skos:historyNote",
  content = "Mapped from the bird.orders data in the ape package using RNeXML")
```


Because `skos` is not in the current namespace list, we add it with a url when
adding this meta element.  We also specify that this annotation be placed 
at the level of the `trees` subnode in the NeXML file.  


```coffee
birds <- add_meta(history, 
                birds, 
                level = "trees",
                namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#"))
```


If we forget to add the namespace with add_meta, we can always add it later
using the `add_namespaces` function,



```coffee
birds <- add_namespaces(c(skos = "http://www.w3.org/2004/02/skos/core#"), birds)
```


(The `add_namespaces` function will avoid creating any duplicate definitions)
For finer control of the level at which a `meta` element is added, we will manipulate 
the nexml R object directly using S4 subsetting, as shown later.  


<!-- Ugh. y'all have much more practice explaining this well and without tangents, 
so please feel free to edit or re-write.  -->

Much richer metadata annotation is possible. Later we illustrate how metadata annotation
can be used to extend the base NeXML format to represent new forms of data while maintaining
compatability with any NeXML parser. The `RNeXML` package can be easily extended to support
helper functions such as `taxize_nexml` to add additional metadata without imposing a large
burden on the user.  

<!--
- Utilities to add common metadata

  - Add a citation to any papers associated with the phylogeny data.  
  - Writing in methodological information on how tree has been generated or analyzed (including R code)
  - Add annotations to any taxon.  

Temporal, spatial, taxonomic coverage for dryad/knb?
--> 



### Reading NeXML metadata

A call to the nexml object prints some metadata summarizing the data structure: <!-- need to make this method faster!--> 



```coffee
birds
```

```
A nexml object representing:
 	 1 phylogenetic tree blocks, where: 
 	 block 1 contains 1 phylogenetic trees 
 	 43 meta elements 
 	 0 character matrices 
 	 23 taxonomic units 
 Taxa: 	 Struthioniformes, Tinamiformes, Craciformes, Galliformes, Anseriformes, Turniciformes ... 

 NeXML generated by RNeXML using schema version: 0.9 
 size: 370.7 Kb 
```


We can extract all metadata pertaining to the NeXML document as a whole
(annotations of the XML root node, `<nexml>`) with the command


```coffee
meta <- get_metadata(birds) 
```


This returns a named list of available metadata. We can see the kinds
of metadata recorded from the names (showing the first 4):


```coffee
names(meta)[1:4]
```

```
[1] "dc:title"                      "dc:creator"                   
[3] "dc:description"                "dcterms:bibliographicCitation"
```


and can ask for a particular element using the standard list subsetting
mechanism (i.e. either the name of an element or it's numeric position),


```coffee
meta[["dc:title"]]
```

```
[1] "Phylogeny of the Orders of Birds From Sibley and Ahlquist"
```


All metadata terms must belong to an explicit *namespace* or vocabulary
that allows a computer to interpret the term precisely. The prefix (before
the `:`) indicates to which vocabulary the term belongs, e.g. `dc` in
this case.  The `get_namespaces` function tells us the definition of
the vocabulary using a link:


```coffee
prefixes <- get_namespaces(birds)
prefixes["dc"]
```

```
                                dc 
"http://purl.org/dc/elements/1.1/" 
```


Following the link we can see that `dc` refers to the "Dublin Core"
vocabulary, and the term `title` refers to the title of a creative work
(as opposed to the title of a person, a legal title, the verb title, etc).
Such explicit definitions allow a computer to reason about these objects,
as we shall see later.  


Common metadata can be accessed with a few dedicated functions:


```coffee
get_citation(birds)
```

```
[1] "Sibley, C. G. and Ahlquist, J. E. (1990) Phylogeny and\n     classification of birds: a study in molecular evolution. New\n     Haven: Yale University Press."
[2] "Paradis E, Claude J and Strimmer K (2004). \"APE: analyses of\nphylogenetics and evolution in R language.\" _Bioinformatics_,\n*20*, pp. 289-290."            
```




```coffee
get_taxa(birds)
```

```
 [1] "Struthioniformes" "Tinamiformes"     "Craciformes"     
 [4] "Galliformes"      "Anseriformes"     "Turniciformes"   
 [7] "Piciformes"       "Galbuliformes"    "Bucerotiformes"  
[10] "Upupiformes"      "Trogoniformes"    "Coraciiformes"   
[13] "Coliiformes"      "Cuculiformes"     "Psittaciformes"  
[16] "Apodiformes"      "Trochiliformes"   "Musophagiformes" 
[19] "Strigiformes"     "Columbiformes"    "Gruiformes"      
[22] "Ciconiiformes"    "Passeriformes"   
```


Which returns text from the otu element labels, typically used to define
taxonomic names, rather than text from explicit meta elements.



We can also access metadata at a specific level (or use `level=all`
to extract all meta elements in a list).  Here we show only the first
few results:


```coffee
otu_meta <- get_metadata(birds, level="otu")
otu_meta[1:4]
```

```
$`tc:toTaxon`
[1] "http://ncbi.nlm.nih.gov/taxonomy/8798"

$`tc:toTaxon`
[1] "http://ncbi.nlm.nih.gov/taxonomy/8802"

$`tc:toTaxon`
[1] "http://ncbi.nlm.nih.gov/taxonomy/8976"

$`tc:toTaxon`
[1] "http://ncbi.nlm.nih.gov/taxonomy/8976"
```


This approach does not preserve the relationship between the metadata element and the 
`otu` it was annotating, and as such is most useful for quick visual exploration. 
Metadata values for specific elements or sets of elemetns can always be accessed using
the S4 subsetting mechanisms (see section below).  



## RDF manipulation and SPARQL queries 

Consider a better example than this one: 



```coffee
rdf <- get_rdf(birds)
```


Query the rdf using XPath expressions:


```coffee
library(XML)
xpathSApply(rdf, "//dc:title", xmlValue) 
```

```
[1] "Phylogeny of the Orders of Birds From Sibley and Ahlquist"     
[2] "A{PE}: analyses of phylogenetics and evolution in {R} language"
```


XPath is an expressive syntax for querying XML-structured data such
as RDF.  Queries using the XPath syntax are possible in R using the
XML library [Temple Lang 2013, XML].  (Note that XPath is far more
expressive for querying the metadata once the RDFa has been extracted,
since attribute values are now interpreted as namespaced XML nodes and
not just arbitrary text.)

The most powerful way to take advantage of the machine-readable nature
of the metadata involves the construction of SPARQL queries. This
approach can exploit the reasoning and logical deduction capacity of
formal vocabularies (ontologies). For a proper introduction to SPARQL
queries, see CITE SOMETHING.  Meanwhile, we just introduce the sytnax
necessary to apply such queries to NeXML files from R.  First we load
the necessary R library and import the RDF-extracted metadata:


```coffee
library(rrdf)
saveXML(rdf, "rdf_meta.xml") # rrdf requires a file name, so we must write the XML out first
```

```
[1] "rdf_meta.xml"
```

```coffee
lib <- load.rdf("rdf_meta.xml")
```


We can then make arbitrary SPARQL queries directly from R.  For instance,
here is a simple query to extract the value of any `dc:title` element, analogous
to the XPath expression above: 


```coffee
sparql.rdf(lib, "SELECT ?title WHERE { ?x <http://purl.org/dc/elements/1.1/title> ?title}")
```

```
     title                                                           
[1,] "A{PE}: analyses of phylogenetics and evolution in {R} language"
[2,] "Phylogeny of the Orders of Birds From Sibley and Ahlquist"     
```





### Extending the NeXML standard through metadata annotation.

No data standard can anticipate all the uses and extensions researchers
may one day need. Researchers have repeatedly adapted the existing NEXUS
file format to accomidate encoding new kinds of information, but in the
process break compatibility between different flavors of NEXUS files, with
few reliable methods to distinguish between these flavors.  NeXML provides
a solution to this problem through the use of metadata annotation.

Here we illustrate this process using the example of stochastic character
mapping [Huelsenbeck et al 2003]. A stochastic character map is simply
an annotation of the branches on a phylogeny, assigning each section
of each branch to a particular "state" (typically of a morphological
characteristic).

[Bollback 2006] provides a widely used stand-alone software implementation
of this method in the software `simmap`, which modified the standard
Newick tree format to express this additional information. This can
break compatibility with other software [^1], and creates a format that
cannot be interpreted without additional information describing this
convention.  By contrast, the NeXML extension is not only backwards
compatible but contains a precise and machine-readable description of
what it is encoding.

In this example, we illustrate how the additional information required
to define a stochastic character mapping (a `simmap` mapping) in NeXML.  



[Revell 2011] describes the `phytools` package for R, which includes
utilities for reading, manipulating, and writing `simmap` files in R.
In this example, we also show how to define `RNeXML` functions that 
map the R representation used by Revell (an extension of the `ape` class)
into the NeXML extension we have defined by using `RNeXML` functions..

Since a stochastic character map simply assigns different states to
parts of a branch (or edge) on the phylogenetic tree, we can create
a NeXML representation by annotating the `edge` elements with appropriate
`meta` elements.  These elements need to describe the character state
being assigned and the duration (in terms of branch-length) that the edge 
spends in that state (Stochastic character maps are specific to time-calibrated
or ultrametric trees).  

NeXML already defines the `characters` element to handle descrete character traits (`nex:char`)
and the states they can assume (`nex:state`).  We will thus reuse the `characters` element for
this purpose, referring to both the character trait and the states by the ids assigned to them
in that element.  (NeXML's convention of referring to everything by id permits a single canonical
definition of each term, making it clear where additional annotation belongs).  For each edge, we 
need to indicate:

<!-- Ugh, do we need to explain that everything in NeXML uses an id so that it can be reused in this way?
     -->

- That our annotation contains a stochastic character mapping reconstruction
- Since many reconstructions are possible for a single edge, we give each reconstruction an id
- We indicate for which character trait we are defining the reconstruction 
- We then indicate which states the character assumes on that edge. 
  For each state realized on the edge, that involves stating: 
    + the state assignment
    + the duration (length of time) for which the edge spends in the given state
    + the order in which the state changes happen (Though we could just assume 
      state transitions are listed chronologically, NeXML suggests making all 
      data explicit, rather than relying on the structure of the data file to
      convey information).  

Thus the the annotation for an edge that switches from state `s2` to state 
`s1` of character `cr1` would be constructed like this:


```coffee
 m <- meta("simmap:reconstructions", children = c(
        meta("simmap:reconstruction", children = c(

          meta("simmap:char", "cr1"),
          meta("simmap:stateChange", children = c(
            meta("simmap:order", 1),
            meta("simmap:length", "0.2030"),
            meta("simmap:state", "s2"))),
          
          meta("simmap:char", "cr1"),
          meta("simmap:stateChange", children = c(
            meta("simmap:order", 2),
            meta("simmap:length", "0.0022"),
            meta("simmap:state", "s1")))
          ))))
```


Of course writing out such a definition manually becomes tedious quickly. Because
these are just R commands, we can easily define a function that can loop over an
assignment like this for each edge, extracting the appropriate order, length and
state from an existing R object such as that provided in the `phytools` package.  
Likewise, it is straight forward to define a function that reads this data using
the `RNeXML` utilities and converts it back to the `phytools` package. The full
implementation of this mapping can be seen in the `simmap_to_nexml()` and the
`nexml_to_simmap()` functions provided in the `RNeXML` package.  

As the code indicates, the key step is simply to define the data in meta elements. In 
so doing, we have defined a custom namespace, `simmap`, to hold our variables.  This
allows us to provide a URL with more detailed descriptions of what each of these 
elements mean:


```coffee
nex <- add_namespaces(c(simmap = "https://github.com/ropensci/RNeXML/tree/master/inst/simmap.md"))
```


At that URL we have posted a simple description of each term. 

<!-- How could this be improved? Should we use something more formal? 
We want this example to be something other users could reasonably
do themselves without expertise in RDFa or XML, but also to be a good
model case that doesn't cut corners. @cboettig -->

<!-- I don't see how this could be improved much without forcing people
to fire op protege (let's not!). As it happens, the Dublin Core URI, 
for example, points to an HTML page so even in that case automatic 
reasoners would not be able to consume the vocabulary and draw inferences
about, e.g. identity of predicates (owl:sameAs and such). @rvosa -->


Using this convention we can generate NeXML files containing simmap
data, read those files into R, and convert them back into the `phytools`
package format. These simple functions serve as further illustration of
how `RNeXML` can be used to extend the NeXML standard.  We illustrate
their use briefly here, starting with  loading a nexml object containing
a simmap reconstruction into R:



```coffee
data(simmap_ex)
```


The `get_trees` function can be used to return an `ape::phylo` tree as
usual.  `RNeXML` automatically detects the simmap reconstruction data
and returns includes this in a `maps` element of the `phylo` object,
for use with other `phytools` functions.


```coffee
phy <- nexml_to_simmap(simmap_ex)
```


We can then use various functions from `phytools` designed for simmap
objects, such as the plotting function:


```coffee
library(phytools)
plotSimmap(phy)
```

```
no colors provided. using the following legend:
       A        B        C 
 "black"    "red" "green3" 
```

![plot of chunk unnamed-chunk-38](http://i.imgur.com/z2CzWbB.png) 


Likewise, we can convert the object back in the nexml format and write
it out to file to be read by other users. 


```coffee
nex <- simmap_to_nexml(phy) 
nexml_write(nex, "simmap.xml")
```

```
[1] "simmap.xml"
```


Though other NeXML parsers (for instance, for Perl or Python) have
not been written explicitly to express simmap data, those parsers will
nonetheless be able to successfully parse this file and expose the simmap
data to the user.






[Revell 2011]: http://doi.org/10.1111/j.2041-210X.2011.00169.x
[Huelsenbeck et al 2003]: http://doi.org/10.1080/10635150390192780 "Stochastic Mapping of Morphological Characters"
[Bollback 2006]: http://doi.org/10.1186/1471-2105-7-88 "SIMMAP: stochastic character mapping of discrete traits on phylogenies."



[^1]: By using the commenting mechanism of the Newick format, it is
possible that other software that doesn't also use the comment mechanism
for some other such purpose would be able to successfully parse the tree.
However there is no way to guarentee that this is the case or for the
data format to describe its use.


### Subsetting nexml objects directly

The `RNeXML` package provides many convenient functions to add and extract
information from `nexml` objects in the R environment without requiring
the reader to understand the details of the NeXML data structure and
making it less likely that a user will generate invalid NeXML syntax
that could not be read by other parsers. The `nexml` object we have been using
in all of the examples is built on R's S4 mechanism. Advanced users may
sometimes prefer to interact with the data structure more directly using 
R's S4 class mechanism and subsetting methods. Many R users are more familiar
with the S3 class mechanism (such as in the `ape` package phylo objects)
rather than the S4 class mechanism used in phylogenetics packages such as 
`ouch` and `phylobase`. The `phylobase` vignette provides an excellent introduction
to these data structures.  Users already familiar with subsetting lists and other
S3 objects in R are likely familar with the use of the `$` operator, such as
`phy$edge`. S4 objects simply use an `@` operator instead (but cannot be subset
using numeric arguments such as `phy[[1]]` or named arguments such as phy[["edge"]]).  


The `nexml` object is an S4 object, as are all of its components (slots).  It's 
heirachical structure corresponds exactly with the XML tree of a NeXML file, with 
the single exception that both XML attributes and children are represented as slots.  
S4 objects have constructor functions to initialize them.  We create a new `nexml` 
object with the command:


```coffee
nex <- new("nexml")
```


We can see a list of slots contained in this object with


```coffee
slotNames(nex)
```

```
 [1] "version"            "generator"          "xsi:schemaLocation"
 [4] "namespaces"         "otus"               "trees"             
 [7] "characters"         "meta"               "about"             
[10] "xsi:type"          
```


Some of these slots have already been populated for us, for instance, the schema version and default namespaces:


```coffee
nex@version
```

```
[1] "0.9"
```

```coffee
nex@namespaces
```

```
                                                     nex 
                             "http://www.nexml.org/2009" 
                                                     xsi 
             "http://www.w3.org/2001/XMLSchema-instance" 
                                                     xml 
                  "http://www.w3.org/XML/1998/namespace" 
                                                    cdao 
"http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#" 
                                                     xsd 
                     "http://www.w3.org/2001/XMLSchema#" 
                                                      dc 
                      "http://purl.org/dc/elements/1.1/" 
                                                 dcterms 
                             "http://purl.org/dc/terms/" 
                                                   prism 
        "http://prismstandard.org/namespaces/1.2/basic/" 
                                                      cc 
                        "http://creativecommons.org/ns#" 
                                                    ncbi 
                 "http://www.ncbi.nlm.nih.gov/taxonomy#" 
                                                      tc 
         "http://rs.tdwg.org/ontology/voc/TaxonConcept#" 
                                                         
                             "http://www.nexml.org/2009" 
```


Recognize that `nex@namespaces` serves the same role as `get_namespaces` function, but provides direct access to the slot data.  For instance, with this syntax we could also overwrite the existing namespaces with `nex@namespaces <- NULL`.  Changing the namespace in this way is not advised.

Some slots can contain multiple elements of the same type, such as `trees`, `characters`, and `otus`.  For instance, we see that 


```coffee
class(nex@characters)
```

```
[1] "ListOfcharacters"
attr(,"package")
[1] "RNeXML"
```


is an object of class `ListOfcharacters`, and is currently empty,


```coffee
length(nex@characters)
```

```
[1] 0
```


In order to assign an object to a slot, it must match the class definition
of the slot.  We can create a new element of any given class with the
`new` function,


```coffee
nex@characters <- new("ListOfcharacters", list(new("characters")))
```


and now we have a length-1 list of character matrices,


```coffee
length(nex@characters)
```

```
[1] 1
```


and we access the first character matrix using the list notation, `[[1]]`. Here we check the class is a `characters` object.  


```coffee
class(nex@characters[[1]])
```

```
[1] "characters"
attr(,"package")
[1] "RNeXML"
```


Direct subsetting has two primary use cases: (a) useful in looking up (and possibly editing) a specific value of an element, or (b) when adding metadata annotations to specific elements. Consider the example file



```coffee
f <- system.file("examples", "trees.xml", package="RNeXML")
nex <- nexml_read(f)
```


We can look up the species label of the first `otu` in the first `otus` block:


```coffee
nex@otus[[1]]@otu[[1]]@label
```

```
      label 
"species 1" 
```


We can add metadata to this particular OTU using this subsetting format


```coffee
nex@otus[[1]]@otu[[1]]@meta <- 
  c(meta("skos:note", 
          "This species was incorrectly identified"),
         nex@otus[[1]]@otu[[1]]@meta)
```


Here we use the `c` operator to append this element to any existing meta annotations to this otu.  




### Publishing NeXML files from R

Data archiving is increasingly required by journals in evolutionary biology and 
biodiversity (e.g. [Whitlock2011]).  The burden of preparing and submitting properly
annotated data to archives continues to be a significant barrier ([Tenopir et al. 2011], [Stodden 2010]),
and many phylogenetic trees upon which studies are based are inaccessible or lost 
to the research community [Drew et al 2013].  `RNeXML` seeks to lower these barriers
by providing functions to immediately archive NeXML files in scientific repositories.  





```coffee
doi <- nexml_publish(birds, visibility = "public", repository="figshare")
```



_Show downloading the nexml file directly from figshare, explain value
of global paths instead of personal directory paths appearing in scripts, etc_



Robust data archiving need not involve immediate release of the data. The `figshare` 
repository supports the option of `visibility = "private"`, allowing secure 
publication that provides a way to backup and share data with collaborators 
prior to public release.  

<!-- Wouldn't it be nice to publish to Dryad or TreeBASE in this way?  -->




### Implementation and architecture

The `RNeXML` package is built for the NeXML Schema, [version 2009],
using the R software environment and the XML package for parsing and
generating XML [Temple Lang 2013, XML], and provides conversions into
the popular `phylo` format provided by the `ape` package [Paradis 2004].
`RNeXML` can automatically check taxonomic names and add taxonomic
identifiers when generating NeXML using methods from the `taxize` package
([Chamberlain 2013]).

<!-- Possibly cite all dependencies and suggests from the DESCRIPTION --> 
<!-- Reference any installation and configuration instructions necessary (e.g. API credentials for figshare) -->


### Quality control

The `RNeXML` package performs severak levels of validation on NeXML files
written from or read into R: validating the XML, and validating against
the current NeXML schema, and performing additional checks through the 
NeXML online validation tool for elements that cannot be enforced by
schema validation alone.  Validation serves to improve consistency in
data encoding which facilitates the use of NeXML across a wide variety
of platforms and software.  A lack of a consistent standard and validation
mechanism has continually plagued the current NEXUS standard and made 
interoperability difficult [Vos et al. 2011].  

Several mechanisms are in place to evaluate the quality of the `RNeXML`
package functions themselves.  In addition to the examples from the
documentation and the automated package checks provided for the R system,
the `RNeXML` package includes a complete test suite using the `testthat`
package [Wickham 2011]. Package development on Github includes continuous 
integration through the `travis` platform, in which the full test suite
is run every time the code is changed.  This not only gives users confidence
that the development copy can be successfully installed, but should assist
other developers seeking to contribute to the package to test that their proposed 
changes have not negatively impacted any of the existing code base.  


### Software reuse and support

The `RNeXML` package is available through the CRAN repository under
a CC0 license. At the time of writing, the current version is 1.0-1. 
The current development
branch can be found on Github at [https://github.com/ropensci/RNeXML].
See the NEWS file for changes in recent versions. Users are encouraged
to submit bug reports or feature requests in the issues log.  Contact
ropensci-discuss@googlegroups.com or the package maintainer for help.

There are many promising directions for further development of the
`RNeXML` package.  The project's [Milestones page][milestones] page provides a list of
steps currently planned or under consideration.  In particular, these
milestones include increased support for interactive mode of entering data
(wizards), and support for integration of semantics defined through the
Web Ontology language (OWL), as discussed in the Challenges section.



### Acknowledgements

This project was supported in part by a grant from the Alfred P Sloan
Foundation (CB and SC), NSF Grant DBI-1306697 (CB), ... `RNeXML` is part
of the rOpenSci project, [http://ropensci.org].



[Temple Lang 2013, XML]: http://cran.r-project.org/web/packages/XML/
[Wickham 2011]: http://vita.had.co.nz/papers/testthat.html
[Chamberlain 2013]: http://dx.doi.org/10.12688/f1000research.2-191.v2
[@ouch]: http://cran.r-project.org/web/packages/ouch/
[@phylobase]: http://cran.r-project.org/web/packages/phylobase
[W3C 2013]: http://www.w3.org/TR/xhtml-rdfa-primer/ "RDFa 1.1 Primer - Second Edition: Rich Structured Data Markup for Web Documents"
[Tenopir et al. 2011]: http://doi.org/10.1371/journal.pone.0021101 "Data Sharing by Scientists: Practices and Perceptions"
[Drew et al 2013]: http://doi.org/10.1371/journal.pbio.1001636 
[milestones]: https://github.com/ropensci/RNeXML/issues/milestones


phylobase::phylo4 was an attempt to solve the problems associated with
the popular ape::phylo format (unpredictable errors, loose and multiple
conflicting specifications).


