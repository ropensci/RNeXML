context("Set global (uuid) identifiers")

test_that("We can generate valid EML with uuid ids on all elements", {

  skip_if_not_installed("geiger")
  skip_if_not_installed("uuid")
  
  if(require("uuid")){
    options(uuid = TRUE)
    require(geiger)
    data(geospiza)
    add_trees(geospiza$phy)
    nexml <- add_characters(geospiza$dat)
    write.nexml(nexml, file = "geospiza.xml")

    expect_true_or_null(nexml_validate("geospiza.xml"))
    unlink("geospiza.xml")
  }
})
