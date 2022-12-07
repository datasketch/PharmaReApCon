#' @export
get_data_api <- function(name,selector=NULL){

  result=NULL
  if(name=="request"){

    load("data/request.rda")
    result=request
    rm(request)
  }

  if(name=="appeals"){

    load("data/appeals.rda")
    result=appeals
    rm(appeals)
  }


  if(name=="contracts"){

    load("data/contracts.rda")
    result=contracts
    rm(contracts)
  }

 result
}
