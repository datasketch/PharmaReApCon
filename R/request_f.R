# a=get_data_api("request")
#' @import dplyr
#' @export
get_year <- function(df, colname_to_year){

  # colname_to_year= "Submission.date..DD.MM.YYYY."
  # df=PharmaReApCon::get_data_api("request")
  # df$year=NULL
  df$year = substr(as.Date(df[[colname_to_year]],format="%d/%m/%Y"), 1, 4)


  df
}

# library(dplyr)
# library(PharmaReApCon)
# test_get_year = function(){
#   dft=PharmaReApCon::get_data_api("request")
#   dft=get_year(dft,"Submission.date..DD.MM.YYYY.")
#    #print(dft)
# }
#
#
#       test_get_year()
#get total count by col

#' @import dplyr
#' @export
counter_r <- function(df,colname_group1, colname_group2=NULL){
  #print(colname_group1)
  #print(names(df))
  if(is.null(colname_group2)){
    df = df %>% group_by(across(all_of(colname_group1))) %>% summarize(count =n())
  }else{

    df = df %>% group_by(across(all_of(colname_group1)),across(all_of(colname_group2))) %>% summarize(count =n())
  }
  df
}


#' @import dplyr
#' @export
filter_r <- function(df,colname_filter_col, colname_filter_val){
  # colname_filter_col="Country"
  df$temp =  df[[colname_filter_col]]
  # class(df$temp)
  # colname_filter_val=c("Chile")
  #  df=get_data_api("request")

  # df =filter_r(df,"Country",c("Mexico"))
   df = df %>% filter( temp %in% colname_filter_val) %>%  dplyr::select(!temp)
df
}

#' @import dplyr
#' @export
filter_make <-function(df_name,filter_var, orderasc=FALSE, orderdesc=FALSE){

   df=get_data_api(df_name)
   #print("entro")
   l = df %>% dplyr::distinct(across(all_of(filter_var)))
   #print(l)
   if(orderasc==TRUE) l = l %>% dplyr::arrange(filter_var)
   if(orderdesc==TRUE) l = l %>% dplyr::arrange(desc(filter_var))
   l
}

# test_filter_maker <- function(){
#   df=get_data_api("request")
#    l = filter_make(df,"Country")
#     l
# }
#

# test_filter_maker()
#

# test_filter_r = function(){
#   df=get_data_api("request")
#   df =filter_r(df,"Country",c("Mexico"))
#   df
# }
#
# test_filter_r()

#' @import dplyr
#' @export
request_country_get_data_graph <- function(name,  country_fil=NULL, status_fil=NULL, type="line"){
  #print(country_fil)
  df=get_data_api(name)
  total =NULL
  if(!is.null(country_fil)){
    df =filter_r(df,"Country",country_fil)
  }

  if(!is.null(status_fil)){
    df =filter_r(df,"Status",status_fil)
  }


  if(type=="bar"){
    total =  counter_r(df,"Status")
    # #print("Intolinedata")
    # #print(df)
    # df = get_year(df,"Submission.date..DD.MM.YYYY.")
    # #print(df)
    # total =  counter_r(df,"Status","year")

   }
  else{
    total =  counter_r(df,"Country")

  }

  total
}


#' @import dplyr
#' @export
request_country_get_data_map <- function(name,  country_fil=NULL, status_fil=NULL){
  df=get_data_api(name)
  total =NULL
  if(!is.null(country_fil)){
    df =filter_r(df,"Country",country_fil)
  }

  if(!is.null(status_fil)){
    df =filter_r(df,"Status",status_fil)
  }


    total =  counter_r(df,"Country")


  total
}


#' @import dplyr
#' @export
request_country_get_data_table <- function(name,  country_fil=NULL, status_fil=NULL){

  df=get_data_api(name)

  if(!is.null(country_fil)){
    df =filter_r(df,"Country",country_fil)
  }

  if(!is.null(status_fil)){
    df =filter_r(df,"Status",status_fil)
  }

  df

}


#' @export
df_color_tree <- function(df, col_to_color,pallete="Pubu"){
  coloresFuente = df %>% dplyr::distinct(across(all_of(col_to_color)))
  b=nrow(coloresFuente)
  color_tree = hcl.colors(b,palette = "Pubu")
  vart= vector()
  vart=append(vart,color_tree)
  coloresFuente = as.data.frame(cbind(coloresFuente,vart))
  colnames(coloresFuente) = c(col_to_color,"...colors")
  df =df %>% dplyr::left_join(coloresFuente, copy=TRUE)
  df
}



# test_df_color_tree <- function(){
#   df=get_data_api("request")
#     df =df_color_tree(df,"Country")
#
# }
# test_df_color_tree <- function(){
#   df=get_data_api("request")
#     df =df_color_tree(df,"Country")
#
# }



#' @export
show_map<- function(df){

  l = list(df,
           # map_name = "col_larg",
           map_tiles = "CartoDB",
           title = "",
           title_size = 11,
           text_family = "Fira Sans",
           title_family = "Fira Sans",
           # tooltip = paste0("<b>{depto}</b><br/>",
           #                  "Total: {Total2}<br/>
           #                                                   PORCENTAJE: {Porcentaje}"),
           legend_show = F,
           # map_zoom = F,
           map_min_zoom = -5,
           map_max_zoom = 20,
           caption = "",

           background_color = "#ffffff",
           palette_colors = c("#d7d1ff", "#bcb5f6", "#a19ae5", "#8880ce", "#7268b1", "#5d518f", "#4b3c69"))

   l
}





#' @export
show_bar = function(df, color_by_input=NULL){

  l <-
    list(
      data = df,
      # title = titleviz(),
      title_size = 15,
      text_family = "Fira Sans",
      title_family = "Fira Sans",
      caption = "",
      label_wrap_legend = 100,
      legend_align = "center",
      legend_verticalAlign = "top",
      graph_type = "stacked",
      format_sample_num = "10T",
      format_numericSymbols = T,
      color_by= color_by_input,
      #prefix="$",
      # tooltip = "{labelToShow}",
      legend_maxHeight = 100,
      background_color = "#ffffff"
    )
 l
}

# total_data =request_country_get_data("request")

