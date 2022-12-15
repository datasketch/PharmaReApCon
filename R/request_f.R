#TODO: translate to utils the general functions

# a=get_data_api("request")
#' @import dplyr
#' @export
get_year <- function(df, colname_to_year){

  df$year <- substr(as.Date(df[[colname_to_year]],format="%d/%m/%Y"), 1, 4)

  df
}

# library(dplyr)
# library(PharmaReApCon)
# test_get_year = function(){
#   dft=PharmaReApCon::get_data_api("request")
#   dft=get_year(dft,"Submission.date..DD.MM.YYYY.")
#    #####print(dft)
# }
#
#
#       test_get_year()
#get total count by col


#' @import dplyr
#' @export
counter_r <- function(df,colname_group1, colname_group2=NULL){
  #####print(colname_group1)
  #####print(names(df))
  if(is.null(colname_group2)){
    df <- df %>% group_by(across(all_of(colname_group1))) %>% summarize(count =n())
  }else{

    df <- df %>% group_by(across(all_of(colname_group1)),across(all_of(colname_group2))) %>% summarize(count =n())
  }
  df
}


#' @import dplyr
#' @export
filter_r <- function(df,colname_filter_col, colname_filter_val){
   df$temp <-  df[[colname_filter_col]]
   df  <- df %>% filter( temp %in% colname_filter_val) %>%  dplyr::select(!temp)
   df
}


#' @import dplyr
#' @export
filter_make <-function(df_name,filter_var, orderasc=FALSE, orderdesc=FALSE){

   df <- get_data_api(df_name) #chance to  get filter data as parameter
   l  <-  df %>% dplyr::distinct(across(all_of(filter_var)))
   #print("l")
   #print(l)
   if(orderasc==TRUE) l  <-  l %>% dplyr::arrange(across(all_of(filter_var)))
   if(orderdesc==TRUE) l  <-  l %>% dplyr::arrange(across(all_of(filter_var)))
   l
}

# test_filter_maker <- function(){
#   df=get_data_api("request")
#    l = filter_make("request","Country",orderdesc=TRUE)
#     l
# }


# test_filter_maker()
#

# test_filter_r = function(){
#   df=get_data_api("request")
#   df =filter_r(df,"Country",c("Mexico"))
#   df
# }
#
# test_filter_r()
#TODO  improve recieving filtered data
#' @import dplyr
#' @export
request_country_get_data_graph <- function(name,  country_fil=NULL, status_fil=NULL,supplier_fil=NULL,vaccine_fil=NULL, type="line"){
  # print("into data graph")

  df <- get_data_api(name)

  total  <- NULL

  if(!is.null(country_fil)){
    df <- filter_r(df,"Country",country_fil)
  }

  if(!is.null(status_fil)){
    df <- filter_r(df,"Status",status_fil)
  }

  if(!is.null(supplier_fil)){

    df <- filter_r(df,"Supplier",supplier_fil)
  }

if(!is.null(vaccine_fil)){
  df <- filter_r(df,"Vaccine",vaccine_fil)
}
  # print("df-get")
  # print(df)

  if(type=="bar"){
     #print("into bar")
    if(name=="request" | name=="appeals"){
      #print("intorequest")
      #print(df)
      total <- counter_r(df,"Status")
      #print(total)
      total <- df_color_tree(total,"Status")

      #print(total)
    }
    else{
      #print("contract")
      total <- counter_r(df,"Vaccine")
      #print("total")
      #print(total)
      #########
      total <- total %>% mutate(Vaccine = case_when(is.na(Vaccine) | Vaccine=="" ~ "(NA)", TRUE ~ Vaccine))
      #print("total2")
      #print(total)
      ############
      total <-  df_color_tree(total,"Vaccine")
      #print("total3")
      #print(total)

    }

   }
  else{
    # print("total")
    total  <-   counter_r(df,"Country")
    print(total)
    # print("total color")
    total  <-  df_color_tree(total,"Country")
    print(total)

  }

  total
}


#TODO  improve recieving filtered data
#' @import dplyr
#' @export
request_country_get_data_map <- function(name,  country_fil=NULL, status_fil=NULL, supplier_fil=NULL,vaccine_fil=NULL){
  df <- get_data_api(name)
  total =NULL

  if(!is.null(country_fil)){
    df  <- filter_r(df,"Country",country_fil)
  }

  if(!is.null(status_fil)){
    df  <- filter_r(df,"Status",status_fil)
  }


  if(!is.null(supplier_fil)){
    df <- filter_r(df,"Supplier",supplier_fil)
  }

  if(!is.null(vaccine_fil)){
    df <- filter_r(df,"Vaccine",vaccine_fil)
  }

    total <- counter_r(df,"Country")


  total
}



#TODO  improve recieving filtered data
#' @import dplyr
#' @export
request_country_get_data_table <- function(name,  country_fil=NULL, status_fil=NULL, supplier_fil=NULL,vaccine_fil=NULL){

  df <- get_data_api(name)

  if(!is.null(country_fil)){
    df <- filter_r(df,"Country",country_fil)
  }

  if(!is.null(status_fil)){
    df <- filter_r(df,"Status",status_fil)
  }
  if(!is.null(supplier_fil)){
    df <- filter_r(df,"Supplier",supplier_fil)
  }

  if(!is.null(vaccine_fil)){
    df <- filter_r(df,"Vaccine",vaccine_fil)
  }

   df

}

#TODO group by count distinct
#' @export
df_color_tree <- function(df, col_to_color,pallete="Pubu"){
  #print("into color tree")
  coloresFuente  <-  df %>%  dplyr::distinct(across(all_of(col_to_color)))
  # print("colores")
  b <- nrow(coloresFuente)
  # print(b)
  if(b>1)   color_tree  <- hcl.colors(b,palette = "green-orange")
  else color_tree = c("#11C638")
  # print(color_tree)
  vart <- vector()
  vart <- append(vart,color_tree)

  coloresFuente <- as.data.frame(cbind(coloresFuente,vart))
  # print(coloresFuente)
  colnames(coloresFuente)  <-  c(col_to_color,"...colors")
  df <- df %>% dplyr::left_join(coloresFuente, copy=TRUE)
  # print("df")
  # print(df)
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

  l  <- list(df,
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
           palette_colors = rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29", "#ffb446", "#ffca6b", "#ffdf98")))

   l
}



#' @export
show_bar = function(df, color_by_input=NULL,tooltip_t=NULL){
  l <-list(
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
      # format_sample_num = "10T",
      format_numericSymbols = T,
      color_by= color_by_input,
      #prefix="$",
      # tooltip = tooltip_t,
      legend_maxHeight = 100,
      background_color = "#ffffff"
    )
 l
}

# total_data =request_country_get_data("request")



#' @export
show_table<- function(df){
  DT::datatable(as.data.frame(df),
                rownames = F,
                options = list(
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  lengthChange = F,
                  pageLength = 5,
                  scrollX = T,
                  scrollY = T#,

                )
  )
}

# html_table_block <- function(da,param){
#   v <- vector()
#   list_temp2 <- ""
#   for(j in 1:nrow(da)){
#     v <- NULL
#     for(i in 1:ncol(da)){
#       if(colnames(da[i])==param) {
#         v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", da[j,i]))
#       }
#       else {
#         v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", da[j,i]))
#
#       }
#     }
#     list_temp <- paste(v, "</BR>",collapse = " ")
#     list_temp2 <- paste(list_temp2,list_temp, "</BR> </BR>")
#   }
#
#   list_temp2 <- HTML(list_temp2)
# }


#' @export
html_table_block <- function(da,param){
  # da=get_data_api("request")
  # param="Request..doc."
  v <- vector()
  list_temp2 <- ""

  for(j in 1:nrow(da)){
    v <- NULL
    for(i in 1:ncol(da)){
     if(colnames(da[i])==param) {
       v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", to_url_link(da[j,i])))

     }
      else {
        ##print("entri3")
        v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", da[j,i]))

      }
    }
    list_temp <- paste(v, "</BR>",collapse = " ")
    list_temp2 <- paste(list_temp2,list_temp, "</BR> </BR>")
  }

  list_temp2 <- HTML(list_temp2)
}


paste_url_tag <- function(head, url) {
  #head not implemented
  # a(paste("href",url,sep="="), "document.pdf")
  paste0("<a href='",url,"'>Document</a>")
}

#' @export
to_url_link <- function(a){
    # a= "victima(1).pdf( http:1) ( http:2"
  # ##print("##############################################################")
  a <- str_replace_all(a,"\\([1-9]\\)","")
  a <- str_replace_all(a,"[' ']","")
  b <- str_replace_all(a,"[(]"," ")
  b <- str_replace_all(b,"[)]","")
  a <- stringr::str_split(b," ")[[1]]
  c <- data.frame()
  for(i in 2:length(a)) {

    c <- append(c,(paste_url_tag(a[1],a[i])))
  }

  c

}
