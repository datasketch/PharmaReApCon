library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(shinyinvoer)
library(dsmodules)
library(webshot2)
library(DT)
library(PharmaReApCon)
webshot::install_phantomjs()


ui <- function(request) {


  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="custom.css")
    ),
    # Leave this function for adding external resources


    fluidPage({
      div(class = "layout-container",
          div(class = "layout-panels",
              div(class = "app-container",
                  div(class = "panel top-malibu",
                      div(style = "max-height: 1000px; !important; margin-bottom: 3%;",
                          tags$head(tags$script(src="handlers.js")),
                           uiOutput("generalFilters"),
                           uiOutput("sel_country"),
                           uiOutput("sel_status"),
                           uiOutput("generalFilters2")),
                      div (class = "panel-body",style="flex-grow: 1; min-height: 600px;",

                           uiOutput("sel_depto"),
                           uiOutput("sel_fecha_inspeccion"),
                           uiOutput("sel_establecimiento"),
                           uiOutput("sel_fecha_aprenhension"),
                           uiOutput("sel_cierre_establecimiento"),
                           uiOutput("sel_marca"),
                           uiOutput("sel_producto"),
                          # uiOutput("sel_operativo"),
                           uiOutput("sel_valor"),
                           uiOutput("sel_periodo")
                      ), div(class="footer",
                              div(style = "display:flex;gap:50px; horizontal-align:middle; text-align:center",
                                  tags$a(
                                    img(src= 'viz_icons/logos-app-orca.png', align = "center", width = 240)))

                      )),
                  div(class = "panel",
                      div (class = "panel-body",
                           div(style="flex-grow: 1; min-width: 600px;",
                               div(class = "head-viz",
                                   div(style = "display:flex;gap:30px;margin-bottom: 20px;align-items: flex-end;",
                                       "VISUALIZACIÓN",
                                       uiOutput("viz_icons")
                                   ),
                                   uiOutput("descargas")
                               ),
                               div(class = "viz-nucleo", style="min-height: 600px;",
                                   uiOutput("viz")
                               )
                           )
                      )),
                  div(class = "panel",
                      div (class = "panel-body",
                           div(style="flex-grow: 1; min-width: 320px;",
                               div(style = "display:block;",
                                   div(class = "viz-center",
                                       div(
                                         uiOutput("downMap"),
                                         leaflet::leafletOutput("map_viz", width = 300, height = 300),
                                         uiOutput("infoClickMap")
                                       )
                                   ),
                                   div(class = "viz-center",
                                       div(

                                         uiOutput("downTree"),
                                         uiOutput("vizbar_tree")),
                                         uiOutput("infoClickTree")
                                   ),

                                   div(class = "viz-center",
                                       div(
                                         uiOutput("downBar"),
                                         uiOutput("vizbar_vizl")),
                                       uiOutput("infoClickBar")
                                   )

                               )
                           )
                      )
                  )
              )
          )
      )
    })
  )
}

server <- function(input, output) {



  question_buttons1 <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
    if (is.null(ids)) stop("Please enter identifiers for each question")
    if (is.null(labels)) stop("Please enter labels for each question")
    # if (is.null(tooltips)) stop("Please enter tooltips for each question")

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
      # #######print(df[z,]$id)
      # #######print(session)
      # htmltools::withTags(
      #   div(class = "BB",
      shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed")# %>%
      # shiny::numericInput(inputId = df[z,]$id, label = df[z,]$questions, value=0) %>%
      #    bsplus::shinyInput_label_embed(
      #   shiny::icon("info") %>%
      # bsplus::bs_embed_popover(title=df[z,]$id,content=df[z,]$tools,placement = "left")
      # bsplus::bs_embed_tooltip(id=df[z,]$id,title =df[z,]$tools)

      # ))
      # bsplus::bs_embed_tooltip(id=df[z,]$id,title =df[z,]$tools)
      # shinyBS::addTooltip(session=session,id=df[z,]$id,title="Hello! This is a hover pop-up. You'll have to click to see the next one.")

    })
    l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))

    l
  }
  question_buttons2 <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
    if (is.null(ids)) stop("Please enter identifiers for each question")
    if (is.null(labels)) stop("Please enter labels for each question")
    # if (is.null(tooltips)) stop("Please enter tooltips for each question")

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
      # #######print(df[z,]$id)
      # #######print(session)
      # htmltools::withTags(
      #   div(class = "BB",
      shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed")# %>%
      # shiny::numericInput(inputId = df[z,]$id, label = df[z,]$questions, value=0) %>%
      #    bsplus::shinyInput_label_embed(
      #   shiny::icon("info") %>%
      # bsplus::bs_embed_popover(title=df[z,]$id,content=df[z,]$tools,placement = "left")
      # bsplus::bs_embed_tooltip(id=df[z,]$id,title =df[z,]$tools)

      # ))
      # bsplus::bs_embed_tooltip(id=df[z,]$id,title =df[z,]$tools)
      # shinyBS::addTooltip(session=session,id=df[z,]$id,title="Hello! This is a hover pop-up. You'll have to click to see the next one.")

    })
    l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))

    l
  }

  output$generalFilters <- renderUI({
    question_buttons1(c("Request"),
                     c( "Request"))

    })

    output$generalFilters2 <- renderUI({
      question_buttons1(c( "Appeals"),
                        c( "Appeals"))

      })


    # bsTooltip(id, title, placement = "bottom", trigger = "hover",
    #           options = NULL)



  quest_choose <- reactive({
    last_btn <- input$last_click
    #######print("lassst")
    #######print(last_btn)

    # if(input$sel_valor) input$sel_valor="cantidad"
    # if (is.null(last_btn)) last_btn <- "Inspecciones"
    # last_btn




    #######print(last_btn)
  })
  # viz_f=NULL

  #TODO  transform reactive filters  to functions package  app


  #########################

  #
  #   output$sel_depto <- renderUI({
  #     req(opts_depto)
  #     req(opts_establecimiento)
  #     default_select <- NULL
  #     # if (!is.null(url_par()$region)) default_select <- tolower(url_par()$region)
  #     opts_depto$id = opts_depto$depto
  #     colnames(opts_depto) = c("id","depto")
  #     opts_establecimiento$id= opts_establecimiento$cierre_establecimiento
  #     colnames(opts_establecimiento) = c("id","cierre_establecimiento")
  #
  #     #######print("######################################")
  #     #######print(opts_establecimiento)
  #     # #######print(class(opts_depto$depto))
  #     #######print(pickerOpts)
  #     default_select <- NULL
  #     req( pickerOpts )
  #     shinyWidgets::pickerInput("sel_depto","Seleccione Región",
  #                               opts_depto, options= pickerOpts(),
  #                               multiple = T
  #
  #     )


  #
#
#   df <- reactive({
#
#   })


titleviz <-reactive({

})


viz_opts <- reactive({
    req(actual_but$active)
  print("5")
  print(actual_but$active)

    if(actual_but$active=="line" | actual_but$active=="bar"){
      print("vizpot")
       vart_country=NULL
       vart_status=NULL
      if (!is.null(input$sel_country)) {
        vart_country= vector()
        vart_country=append(vart_country,input$sel_country)

      }
       if (!is.null(input$sel_status)) {
         vart_status= vector()
         vart_status=append(vart_country,input$sel_status)

       }

      df=request_country_get_data_graph("request",vart_country,vart_status,type="bar")
      l=show_bar(df)

    }else{
      print("inmap")
      vart_country=NULL
      vart_status=NULL
      if (!is.null(input$sel_country)) {
        vart_country= vector()
        vart_country=append(vart_country,input$sel_country)

      }
      if (!is.null(input$sel_status)) {
        vart_status= vector()
        vart_status=append(vart_country,input$sel_status)

      }

      df=request_country_get_data_map("request",vart_country,vart_status)
      # print(df)
      l=show_map(df)

    }
    l
  })

  ##### outputs section
  # output$sel_operativo<- renderUI({
  #   req(opts_operativo())
  #   default_select <- NULL
  #   if( quest_choose() =="Aprehensiones"){
  #     req( pickerOpts )
  #     shinyWidgets::pickerInput("sel_operativo","Clase de operativo",
  #                               opts_operativo(), options= pickerOpts(),
  #                               multiple = T
  #
  #     )
  #
  #   }
  #
  # })
  output$sel_valor <- renderUI({

    # filter_make()
    # req(opts_depto)
    #
    # default_select <- NULL
    # req( pickerOpts )
    # if( quest_choose() =="Aprehensiones"){
    #   shiny::radioButtons("sel_valor","Variable numérica",
    #                       c("Cantidad de actas de aprehensión"="cantidad","Cantidad de productos aprehendidos"="cantidad_productos","Avalúo comercial"="valor_comercial")
    #
    #   )
    # }
    # # else{
    # #   shiny::radioButtons("sel_valor","Variable numérica",
    # #                       c(Cantidad="cantidad")
    # #   )
    # # }
    #

  })

  ###################viz
  # possible_viz <- reactive({
#
#     #  if (is.null(r$d_viz)) return()
#     # req(r$active_viz)
#     # #######print(r$d_viz)
#     v <- c("line", "bar")
#     #######print("perio")
#     # #######print(stringr::str_detect(r$d_viz,"PERIODO"))
#     # if (!stringr::str_detect(r$d_viz,"PERIODO") |length(unique(r$periodoId)) == 1 ) {
#     # if (!"PERIODO" %in% names(r$d_viz) & r$active_viz!="map") {
#     #   v <- "bar"
#     # }
#
#     v <- c(v, "table","map")
     # v <- c("bar","map","table")
#
#     #######print(v)
#     # if (nrow(r$d_viz) <= 1) v <- "table"
#     v
  # })


  output$sel_country <- renderUI({
    default_select <- NULL
    # df=get_data_api("request")
    # print(df)
    df <- filter_make("request","Country")
    print("df")


    selectizeInput("sel_country","Country",
                   df,
                   default_select, multiple=TRUE, width='200px')
  })


  output$sel_status <- renderUI({
    default_select <- NULL
    df <- filter_make("request","Status")
    print("df")


    selectizeInput("sel_status","Status",
                   df,
                   default_select, multiple=TRUE, width='200px')
  })


  possible_viz <- reactive({

    #  if (is.null(r$d_viz)) return()
    # req(r$active_viz)
    # #######print(r$d_viz)
    # v <- c("line", "bar")
    # #######print("perio")
    # # #######print(stringr::str_detect(r$d_viz,"PERIODO"))
    # # if (!stringr::str_detect(r$d_viz,"PERIODO") |length(unique(r$periodoId)) == 1 ) {
    # # if (!"PERIODO" %in% names(r$d_viz) & r$active_viz!="map") {
    # #   v <- "bar"
    # # }
    #
    # v <- c(v, "table","map")
    v <- c("bar","map","table")

    #######print(v)
    # if (nrow(r$d_viz) <= 1) v <- "table"
    v
  })


  actual_but <- reactiveValues(active = NULL)

  observe({
    print("3")
    print( actual_but$active)
    if (is.null(input$viz_selection)) return()
    req(possible_viz())
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
    # #######print(actual_but$active)
    # actual_but$active
    # viz_f <- vizFrtype()

  })

  output$viz_icons <- renderUI({
    print("2")
    print( actual_but$active)



    # #######print("icons")
     req(possible_viz())
    # possible_viz <- possibsle_viz()
    #
    #

    possible_viz <- possible_viz()


    shinyinvoer::buttonImageInput('viz_selection',
                                  " ",#div(class="title-data-select", "Selecciona tipo de visualización"),
                                  images = possible_viz,
                                  path = "www/viz_icons/",
                                  active = actual_but$active,
                                  imageStyle = list(shadow = TRUE,
                                                    borderColor = "#ffffff",
                                                    padding = "3px"))


  })



  vizFrtype <- reactive({

    tp <- "CatNum"
    # if (!"PERIODO" %in% names(d_viz)) {
    #   tp <- "CatCatNum"
    # }

   tp
  })



  hgch_viz <- reactive({
    print("1")
    print( actual_but$active)
  # tryCatch({
    req(viz_opts())
  if (is.null(vizFrtype())) return()
    if (is.null(actual_but$active))
    if (is.null(actual_but$active)) actual_but$active="bar"
    if (actual_but$active == "table") return()
    if (actual_but$active == "map") return()
    #
    #     viz <- paste0("lfltmagic::", "lflt_choropleth_GnmNum") # TODO update with active_viz and vi type
    #
    # }
    # else{

            viz <- paste0("hgchmagic::", paste0("hgch_",actual_but$active, "_", vizFrtype()))
            library(hgchmagic)
    # }

    try({
      do.call(eval(parse(text=viz)),
              viz_opts()
      )
    }


    )

    # },
    # error = function(cond) {
    #   return()
    # })
  })



  output$hgch_viz <- highcharter::renderHighchart({
    # tryCatch({
       print("-1")
      print(actual_but$active)
      req( hgch_viz())
      hgch_viz()
     # },
    # error = function(cond) {
    #   return()
    # })
  })


  r_viz <- reactive({
    # tryCatch({
      req(viz_opts())
    #
    #   print("llego")
    print("-2")
    print( actual_but$active)
      if (is.null(vizFrtype())) return()
       if (actual_but$active == "bar") return()
        if (actual_but$active == "table") return()
      print("LF")
      viz <- paste0("lfltmagic::", "lflt_choropleth_GnmNum")
      library(lfltmagic)
      suppressWarnings(
        do.call(eval(parse(text=viz)),
                viz_opts()
        ))
    # # },
    # error = function(cond) {
    #   return()
    # })
  })

  output$r_viz <- leaflet::renderLeaflet({
#
     req(r_viz())
#     print("inviz")
    #req(r$InsId_band)
    # actual_but$active = "map"
    # print((r_viz))
    # print(colnames(r$d_viz))
    # a = r$d_viz
    # print(a)
    r_viz()

  })




  output$table_dt <- DT::renderDataTable({
    # #if (r$active_viz != "table") return()
    # req(df())
    # # df <- r$d_fil
    # df = con %>% tbl("temporal") %>% collect()
    # if(is.null(df)) return()
    # DT::datatable(df,
    #               rownames = F,
    #               options = list(
    #                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    #                 lengthChange = F,
    #                 pageLength = 5,
    #                 scrollX = T,
    #                 scrollY = T#,
    #                 #   initComplete = htmlwidgets::JS(
    #                 #     "function(settings, json) {",
    #                 #     "$(this.api().table().header()).css({'background-color': '#012a4a', 'color': '#fff'});",
    #                 #     "}")
    #               )
    # ) #%>%
    # #DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')

  })

  output$descargas <- renderUI({
    # if (is.null(actual_but$active)) return()
    # if (actual_but$active != "table") {
    #   dsmodules::downloadImageUI("download_viz", dropdownLabel ="Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown", text = "Descargar")
    # } else {
    #   dsmodules::downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown", text = "Descargar")
    # }
  })

  # dsmodules::downloadTableServer("dropdown_table", element = reactive( df_temp() ), formats = c("csv", "xlsx", "json"))
  # dsmodules::downloadImageServer("download_viz", element = reactive(hgch_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")


  output$viz <- renderUI({
    print("-3")
    print( actual_but$active)
     if (is.null(actual_but$active))
       actual_but$active="bar"

    if (actual_but$active == "table") {
      dataTableOutput("table_dt",  width = 800)
    } else if (actual_but$active == "map") {
      leaflet::leafletOutput("r_viz", height = 600)
    } else {print("drrrraw")


      highchartOutput("hgch_viz", height = 600)
    }
  })


}

shinyApp(ui, server)

