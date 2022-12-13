# readRenviron(".Renviron")
# request <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
#                                tables = c("tbl0RZB8txg6xaDVZ")#eg: ("tabla1", "tabla2")
# )
#
# options(timeout=100000)
#
#
# countries <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
#                                tables = c("tbl6WB3ZHH9d7ZicJ")#eg: ("tabla1", "tabla2")
# )
#
#
# contracts  <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
#                                  tables = c("tbl7EnV6QCTrvIAB7")#eg: ("tabla1", "tabla2")
# )
#
#
# request <- airtabler::airtable(base = "app2THgdU5NHqq1hh" , #appxxxxxxxx
#                                tables = c("tbl0RZB8txg6xaDVZ")#eg: ("tabla1", "tabla2")
# )
# request <- as.data.frame(request$tbl0RZB8txg6xaDVZ$select())
# class(request$Status)
#
#
# b <- as.data.frame(countries$tbl6WB3ZHH9d7ZicJ$select())
# a= as.data.frame(request$tbl0RZB8txg6xaDVZ$select())
#
# c =  as.data.frame(contracts$tbl7EnV6QCTrvIAB7$select())
# c$Vaccine = unlist(c$Vaccine)
# c[sapply(c$Vaccine, is.null)] <- NA
# class(c$Supplier)
#
#
# c$Vaccine[[29]][2]
#
# c$Contracts[[1]][2]
# class(c$)
# a$Country = unlist(a$Country)
#
#
# a$Country = unlist(a$Country)
# a=a %>% left_join(b |> select(id,Country),by=c("Country"="id"))
# a=a %>% rename(country_id=Country, Country= Country.y)
# str(a)
#
# [[i]][3])
# # paste(a$`Request (doc)`[[21]][1,3],a$`Request (doc)`[[21]][1,2],sep=" ")
# #
# # nrow(a$`Request (doc)`[[21]][3])
# # a$Request..doc.=""
# # a$id[1]
# # a$Request..doc.[1]=a$id[[1]][1]
#
# a$Request..doc.=""
#   list_temp2=""
# for(i in 1:nrow(a)){
#   temp=""
#   print(nrow(a$`Request (doc)`[[i]][3]))
#   if(!is.null(nrow(a$`Request (doc)`[[i]][3]))){
#       for(j in 1:nrow(a$`Request (doc)`[[i]][3])){
#         temp=paste(temp, a$`Request (doc)`[[i]][j,3] , paste("(",a$`Request (doc)`[[i]][j,2],")",sep=""),sep=" ")
#       }
#   a$Request..doc.[i]=temp
#   print(temp)
#   }
# }
#
#   c$Vaccine_list=""
#   for(i in 1:nrow(c)){
#     temp=""
#     print(nrow(c$Vaccine[[i]]))
#        if(!is.null(c$Vaccine[[i]]))      c$Vaccine_list[i]=c$Vaccine[[i]]
#       else  c$Vaccine_list[i]= "NA"
#
#
#   }
#
#
# a$Request..doc.
# #
# # a$Country = unlist(a$Country)
