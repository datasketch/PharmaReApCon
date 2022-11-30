# readRenviron(".Renviron")
devtools::install_github("bergant/airtabler")

airtable <- airtabler::airtable(base ="https://airtable.com/shrGnAZYmTNhIIBQu", tables =c("A. Requests","B. Appeals"))

airtable$`A. Requests`$get()
airtable$`A. Requests`$select_all()



