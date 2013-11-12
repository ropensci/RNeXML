
## Should offer a faster way of extracting data without loading the entire tree into memory.  Not working though...
## Even XML documentation examples don't seem to return what I expected (get a function closure back instead of a subsetted tree)


require(XML)
require(RNeXML)

f <- system.file("examples", "trees.xml", package="RNeXML")

handlers <- list(meta = function(node){ xmlAttrs(node)["length"] })
handlers <- list(edge = function(node){xmlAttrs(node)["length"] })


handlers <-  (function() { 
                 vars <- character(0) ;
                 list(meta=function(x, attrs) { 
                                 vars <<- c(vars, xmlValue(x[[1]])); 
                                 NULL}, 
                      startElement=function(x,attr){
                                    NULL
                                   }, 
                      names = function() {
                                  vars
                              }
                     )
                })()


a <- xmlParse(f, handlers = handlers,asTree=TRUE)
