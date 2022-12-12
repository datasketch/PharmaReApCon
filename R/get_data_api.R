#' @export
get_data_api <- function(name,selector=NULL){
  # readRenviron(".Renviron")
  options(timeout=100000)
  result=NULL

  countries <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
                                   tables = c("tbl6WB3ZHH9d7ZicJ")#eg: ("tabla1", "tabla2")
  )



  if(name=="request"){
    request <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
                                   tables = c("tbl0RZB8txg6xaDVZ")#eg: ("tabla1", "tabla2")
    )
    request= as.data.frame(request$tbl0RZB8txg6xaDVZ$select())
    request$Request..doc.=""
    list_temp2=""

      for(i in 1:nrow(request)){
       temp=""
      # print(nrow(request$`Request (doc)`[[i]][3]))
      if(!is.null(nrow(request$`Request (doc)`[[i]][3]))){
        for(j in 1:nrow(request$`Request (doc)`[[i]][3])){
          temp=paste(temp, a$`Request (doc)`[[i]][j,3] , paste("(",a$`Request (doc)`[[i]][j,2],")",sep=""),sep=" ")
        }
        request$Request..doc.[i]=temp
        # print(temp)
      }
    }

    b <- as.data.frame(countries$tbl6WB3ZHH9d7ZicJ$select())
    request$Country = unlist(request$Country)
    request=request %>% left_join(b,by=c("Country"="id"))
    request=request %>% rename(country_id=Country, Country= Country.y)
    print(result)
    request = request |> select(!c(country_id,`Request (doc)`))

    # load("data/request.rda") #csv version
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

#TODO create
#' #' @export
#' get_data_api_by_key <- function(name_key,selector=NULL){
#'
#' }
