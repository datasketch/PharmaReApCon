# a=get_data_api("request")

#get total count by col
#' @export
counter_r <- function(df,colname_group1, colname_group2=NULL){
  print(colname_group1)
  print(names(df))
  if(is.null(colname_group2)){
    df = df %>% group_by(across(all_of(colname_group1))) %>% summarize(count =n())
  }else{

    df = df %>% group_by(across(all_of(colname_group1,colname_group2))) %>% summarize(count =n())
  }
  df
}


#' @export
filter_r <- function(df,colname_filter_col, colname_filter_val){
  df = df %% dply::filter(across(all_of(country_fil))  %in%  across(all_of(country_fil)))
  df
}

#' @export
filter_make <-function(df_name,filter_var, orderasc=FALSE, orderdesc=FALSE){

   df=get_data_api(df_name)
   print("entro")
   l = df %>% dplyr::distinct(across(all_of(filter_var)))
   print(l)
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
#' @export
request_country_get_data_graph <- function(name,  country_fil=NULL, status_fil=NULL){

    df=get_data_api(name)

     if(!is.null(country_fil)){
       df =filter_r(df,"Country",country_fil)
     }

    if(!is.null(status_fil)){
      df =filter_r(df,"Status",status_fil)
    }

     total =  counter_r(df,"Country")

}

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
show_map<- function(df){

  l = list(df,
           map_name = "col_larg",
           map_tiles = "CartoDB",
           title = "",
           title_size = 11,
           text_family = "Fira Sans",
           title_family = "Fira Sans",
           # tooltip = paste0("<b>{depto}</b><br/>",
           #                  "Total: {Total2}<br/>
           #                                                   PORCENTAJE: {Porcentaje}"),
           legend_show = F,
           map_zoom = F,
           map_min_zoom = 5,
           map_max_zoom = 8,
           caption = "",

           background_color = "#ffffff",
           palette_colors = c("#d7d1ff", "#bcb5f6", "#a19ae5", "#8880ce", "#7268b1", "#5d518f", "#4b3c69"))

   l
}

#' @export
show_bar = function(df){

  l <-
    list(
      data = df(),
      title = titleviz(),
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
      #prefix="$",
      tooltip = "{labelToShow}",
      legend_maxHeight = 100,
      background_color = "#ffffff"
    )
 l
}

# total_data =request_country_get_data("request")

