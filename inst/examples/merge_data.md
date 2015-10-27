``` r
library("RNeXML")
```

    ## Loading required package: ape

``` r
library("dplyr")
```


``` r
library("geiger")
```

Let's generate a `NeXML` file using the tree and trait data from the `geiger` package's "primates" data:

``` r
data("primates")
add_trees(primates$phy) %>% 
  add_characters(primates$dat, ., append=TRUE) %>% 
  taxize_nexml() -> nex 
```


(Note that we've used `dplyr`'s cute pipe syntax, but unfortunately our `add_` methods take the `nexml` object as the *second* argument instead of the first, so this isn't as elegant since we need the stupid `.` to show where the piped output should go...)

We now read in the three tables of interest. Note that we tell `get_characters` to give us species labels as there own column, rather than as rownames. The latter is the default only because this plays more nicely with the default format for character matrices that is expected by `geiger` and other phylogenetics packages, but is in general a silly choice for data manipulation.

``` r
otu_meta <- get_metadata(nex, "otu")
taxa <- get_taxa(nex)
char <- get_characters(nex, rownames_as_col = TRUE)
```

Now that we have nice `data.frame` objects for all our data, it's easy to join them into the desired table with a few obvious `dplyr` commands:

``` r
taxa %>% 
  left_join(char, by = c("label" = "taxa")) %>% 
  left_join(otu_meta, by = c("id" = "parent_id")) %>%
  select(id, label, x, href)
```

    ## Warning in left_join_impl(x, y, by$x, by$y): joining factor and character
    ## vector, coercing into character vector

    ## Source: local data frame [233 x 4]
    ## 
    ##       id                       label        x
    ##    (chr)                       (chr)    (dbl)
    ## 1    ou1 Allenopithecus_nigroviridis 8.465900
    ## 2    ou2         Allocebus_trichotis 4.368181
    ## 3    ou3           Alouatta_belzebul 8.729074
    ## 4    ou4             Alouatta_caraya 8.628735
    ## 5    ou5          Alouatta_coibensis 8.764053
    ## 6    ou6              Alouatta_fusca 8.554489
    ## 7    ou7           Alouatta_palliata 8.791790
    ## 8    ou8              Alouatta_pigra 8.881836
    ## 9    ou9               Alouatta_sara 8.796339
    ## 10  ou10          Alouatta_seniculus 8.767173
    ## ..   ...                         ...      ...
    ## Variables not shown: href (chr)
