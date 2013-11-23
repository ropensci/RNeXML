
nexml_env = new.env(hash=TRUE)

# If no prefix is given, will use a UUID
# Generates an id number by appending a counter to the prefix
# Will keep track of the counter for each prefix for that session.  
nexml_id <- function(prefix = "", use_uuid = getOption("uuid", FALSE)){
  if(use_uuid){
    success <- require(uuid)
    if(success)
      id <- uuid::UUIDgenerate()
    else {
      install.packages("uuid")
      success <- require(uuid)
      if(success)
        id <- uuid::UUIDgenerate()
      else
        stop("Cannot generate uuid, please provide a prefix")
    }
  } else {
    counter <- try(get(prefix, envir=nexml_env), silent=TRUE)
    if(is(counter, "try-error")){
      assign(prefix, 1, envir=nexml_env)
      counter <- 1
    }
    id <- paste0(prefix, counter)
    counter <- counter + 1
    assign(prefix, counter, envir=nexml_env)
  }
  id
}


# use an environment to store counter 
