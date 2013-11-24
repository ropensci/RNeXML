
nexml_env = new.env(hash=TRUE)

# If no prefix is given, will use a UUID
# Generates an id number by appending a counter to the prefix
# Will keep track of the counter for each prefix for that session.  
nexml_id <- function(prefix = "", use_uuid = getOption("uuid", FALSE)){
  if(use_uuid){
    success <- require(uuid)
    if(success)
      uid <- uuid::UUIDgenerate()
    else {
      install.packages("uuid")
      success <- require(uuid)
      if(success)
        uid <- uuid::UUIDgenerate()
      else
        stop("Cannot generate uuid, please provide a prefix")
    }
  } else {
    id_counter <- try(get(prefix, envir=nexml_env), silent=TRUE)
    if(is(id_counter, "try-error")){
      assign(prefix, 1, envir=nexml_env)
      id_counter <- 1
    } else if(is(id_counter, "character")){
      warning(paste("id_counter is character", id_counter))
      id_counter <- 1000
    } 

    uid <- paste0(prefix, id_counter)
    id_counter <- id_counter + 1
    assign(prefix, id_counter, envir=nexml_env)
  }
  uid
}


nexml_clean_env <- function(){
  rm(list=ls(envir=nexml_env), envir=nexml_env)
}

# use an environment to store counter 
