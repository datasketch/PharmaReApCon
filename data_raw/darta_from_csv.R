#file for to proccesing csvs
request= read.csv("data_raw/request.csv")
contracts= read.csv("data_raw/contracts.csv")
countries= read.csv("data_raw/countries.csv")
movement= read.csv("data_raw/movement.csv")
appeals= read.csv("data_raw/appeals.csv")



#saving
usethis::use_data(request,contracts,countries,movement,appeals, overwrite = TRUE)
