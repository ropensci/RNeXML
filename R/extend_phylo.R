setClass("phyloS4", 
         representation(edge = "matrix",
                        Nnode = "integer",
                        tip.label = "character",
                        edge.length = "numeric"))
setOldClass("phylo", S4Class="phyloS4")
# FIXME repeat selectMethod for all ape, geiger, etc methods(??)
selectMethod("show", "phylo")
removeClass("phyloS4")

setClass("nexmlTree", representation(nexml = "nexml"), contains="phylo")
setMethod("show", "nexmlTree", function(object) print.phylo(object))
# callNextMethod(object) ## callNextMethod might have been an option, but it looks for 'show' method, not print method?? 


### Testing 
# a <- new("phylo", bird.orders)
# expect_is(a, "phylo")
# a 
# plot(a)
# 
# b <- new("nexmlTree", bird.orders, nexml = as(bird.orders, "nexml"))
# expect_is(b, "phylo")
# b
# plot(b) 
# 
# Some ape functions don't check class properly.  i.e. class(b) == "phylo" is FALSE, but is(b, "phylo") is TRUE.  

# Don't really need these, but here they are mapping between S3 and S4 
setAs("phyloS4", "phylo", function(from){
      out <- list(edge = from@edge,
                  Nnode = from@Nnode,
                  tip.label = from@tip.label,
                  edge.length = from@edge.length)
      class(out) <- "phylo"
      out
})
setAs("phylo", "phyloS4", function(from)
      new("phyloS4", 
          edge = from$edge,
          Nnode = from$Nnode,
          tip.label = from$tip.label,
          edge.length = from$edge.length))



