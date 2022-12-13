#' @export
get_data_api <- function(name,selector=NULL){
  readRenviron("environ.R")
  options(timeout=100000)
  result=NULL

  countries <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
                                   tables = c("tbl6WB3ZHH9d7ZicJ")#eg: ("tabla1", "tabla2")
  )



  if(name=="request"){
    request <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
                                   tables = c("tbl0RZB8txg6xaDVZ")#eg: ("tabla1", "tabla2")
    )
    request <- as.data.frame(request$tbl0RZB8txg6xaDVZ$select())
    request$Request..doc. <- ""
    list_temp2 <- ""

      for(i in 1:nrow(request)){
       temp <- ""
      # #print(nrow(request$`Request (doc)`[[i]][3]))
      if(!is.null(nrow(request$`Request (doc)`[[i]][3]))){
        for(j in 1:nrow(request$`Request (doc)`[[i]][3])){
          temp <- paste(temp, request$`Request (doc)`[[i]][j,3] , paste("(",request$`Request (doc)`[[i]][j,2],")",sep=""),sep=" ")
        }
        request$Request..doc.[i] <-  temp
        # #print(temp)
      }
    }

    b <- as.data.frame(countries$tbl6WB3ZHH9d7ZicJ$select())
    request$Country <-  unlist(request$Country)
    request <- request %>% left_join(b,by=c("Country"="id"))
    request <- request %>% rename(country_id=Country, Country= Country.y)
    #print(result)
    request <- request |> select(!c(country_id,`Request (doc)`))

    # load("data/request.rda") #csv version
    result <- request
    rm(request)
  }

  if(name=="appeals"){
    a=NULL
    appeals <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
                                   tables = c("tblAEExG4kBQao3uC")#eg: ("tabla1", "tabla2")
    )


    b <- as.data.frame(countries$tbl6WB3ZHH9d7ZicJ$select())
    a <- as.data.frame(appeals$tblAEExG4kBQao3uC$select())


    a$Attachments_text <- ""
    list_temp2 <- ""
    for(i in 1:nrow(a)){
      temp <- ""
      #print(nrow(a$Attachments[[i]][3]))
      if(!is.null(nrow(a$Attachments[[i]][3]))){
        for(j in 1:nrow(a$Attachments[[i]][3])){
          temp <- paste(temp, a$Attachments[[i]][j,3] , paste("(",a$Attachments[[i]][j,2],")",sep=""),sep=" ")
        }
        a$Attachments_text[i] <- temp
      }
    }

    a$Country <-  unlist(a$Country)
    a <- a %>% left_join(b,by=c("Country"="id"))
    a <- a %>% rename(country_id=Country, Country= Country.y)
    a <- a %>% rename(Attachments_list=Attachments, Attachments= Attachments_text)
    a <- a |> select(!c(country_id,Attachments_list))
    # load("data/appeals.rda")
    result <- a
    rm(a)
  }


  if(name=="contracts"){
    a=NULL
    contracts  <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
                                      tables = c("tbl7EnV6QCTrvIAB7")#eg: ("tabla1", "tabla2")
    )

    b <- as.data.frame(countries$tbl6WB3ZHH9d7ZicJ$select())
    a <-  as.data.frame(contracts$tbl7EnV6QCTrvIAB7$select())


    a$Contracts_text <- ""
    list_temp2 <- ""
    for(i in 1:nrow(a)){
      temp <- ""
      #print(nrow(a$Contracts[[i]][3]))
      if(!is.null(nrow(a$Contracts[[i]][3]))){
        for(j in 1:nrow(a$Contracts[[i]][3])){
          temp <- paste(temp, a$Contracts[[i]][j,3] , paste("(",a$Contracts[[i]][j,2],")",sep=""),sep=" ")
        }
        a$Attachments_text[i] <- temp
      }
    }

    a$Country  <-  unlist(a$Country)

    a$Vaccine_text  <- ""

    for(i in 1:nrow(a)){

      if(!is.null(a$Vaccine[[i]]))  a$Vaccine_text[i]=a$Vaccine[[i]]
      else  a$Vaccine_text[i]= "NA"


    }

    a <- a %>% left_join(b,by=c("Country"="id"))
    a <- a %>% rename(country_id=Country, Country= Country.y)
    a <- a %>% rename(Contracts_list=Contracts, Contracts= Contracts_text)
    a <- a %>% rename(Vaccine_list=Vaccine, Vaccine = Vaccine_text)
    a <- a |> select(!c(country_id, Contracts_list, Vaccine_list))
    # load("data/contracts.rda")
    result <- a
    rm(a)
  }

 result
}

#TODO create
#' #' @export
#' get_data_api_by_key <- function(name_key,selector=NULL){
#'
#' }
