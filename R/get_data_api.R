#' @export
get_data_api <- function(name,selector=NULL){

  result=NULL
  if(name=="request"){

    load("data/request.rda")
    result=request
    rm(request)
  }

 result
}
