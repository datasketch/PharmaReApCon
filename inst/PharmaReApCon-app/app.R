#TODO  change UI to shinypanels, button implementation,  create reactive with common filterd data
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
                           div(style="flex-grow: 1;",
                               div(style = "display:block;",
                                   div(class = "viz-center",
                                       div(style="flex-grow: 1; max-width: 320px;overflow: scroll;max-height: 620px",
                                         htmlOutput("side_table")


                                       )
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

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
      shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed")# %>%

    })
    l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))

    l
  }

  question_buttons2 <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
    if (is.null(ids)) stop("Please enter identifiers for each question")
    if (is.null(labels)) stop("Please enter labels for each question")

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
     shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed")# %>%

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


   uest_choose <- reactive({

  })



titleviz <-reactive({

})


viz_opts <- reactive({
    req(actual_but$active)

    if(actual_but$active=="treemap" | actual_but$active=="bar"){
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


      if(actual_but$active=="treemap"){
        df=request_country_get_data_graph("request",vart_country,vart_status,type="treemap")
          l=show_bar(df, "Country",paste0("Country: ","{Country} Total {count}"))

      }
      else{
        df=request_country_get_data_graph("request",vart_country,vart_status,type="bar")
          l=show_bar(df, "Status",paste0("Status: ","{Status} Total {count}"))
      }


    }
    else{
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
      # #print(df)
      l=show_map(df)

    }
    l
  })

    output$sel_valor <- renderUI({

  })



  output$sel_country <- renderUI({
    default_select <- NULL
    df <- filter_make("request","Country")
    selectizeInput("sel_country","Country",
                   df,
                   default_select, multiple=TRUE, width='200px')
  })


  output$sel_status <- renderUI({
    default_select <- NULL
    df <- filter_make("request","Status")
    selectizeInput("sel_status","Status",
                   df,
                   default_select, multiple=TRUE, width='200px')
  })


  possible_viz <- reactive({
    v <- c("map","bar","treemap","table")
    v
  })


  actual_but <- reactiveValues(active = NULL)

  observe({
    if (is.null(input$viz_selection)) return()
    req(possible_viz())
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })

  output$viz_icons <- renderUI({
     req(possible_viz())
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

    tp
  })



  hgch_viz <- reactive({
  tryCatch({
    req(viz_opts())
    if (is.null(vizFrtype())) return()
    if (is.null(actual_but$active)) actual_but$active="map"
    if (actual_but$active == "table") return()
    if (actual_but$active == "map") return()

     viz <- paste0("hgchmagic::", paste0("hgch_",actual_but$active, "_", vizFrtype()))
            library(hgchmagic)

    try({
      do.call(eval(parse(text=viz)),
              viz_opts()
      )
    })

    },
    error = function(cond) {
      return()
    })
  })



  output$hgch_viz <- highcharter::renderHighchart({
    tryCatch({
      req( hgch_viz())
      hgch_viz()
    },
    error = function(cond) {
      return()
    })
  })


  r_viz <- reactive({
    tryCatch({
      req(viz_opts())
      if (is.null(vizFrtype())) return()
      if (actual_but$active == "bar") return()
      if (actual_but$active == "table") return()
      viz <- paste0("lfltmagic::", "lflt_choropleth_GnmNum")
      library(lfltmagic)
      suppressWarnings(
        do.call(eval(parse(text=viz)),
                viz_opts()
        ))
    },
    error = function(cond) {
      return()
    })
  })

  output$r_viz <- leaflet::renderLeaflet({

     req(r_viz())
    r_viz()

  })


  df_temp <- reactive({

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

    df=request_country_get_data_table("request",vart_country,vart_status)
    df
  })

  output$table_dt <- DT::renderDataTable({


    df=request_country_get_data_table("request")
    l=show_table(df)
    l

  })

  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz", dropdownLabel ="Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown", text = "Descargar")
    } else {
      dsmodules::downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown", text = "Descargar")
    }
  })

   dsmodules::downloadTableServer("dropdown_table", element = reactive( df_temp() ), formats = c("csv", "xlsx", "json"))
   dsmodules::downloadImageServer("download_viz", element = reactive(hgch_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")


  output$viz <- renderUI({
     if (is.null(actual_but$active))
       actual_but$active="bar"

    if (actual_but$active == "table") {
      dataTableOutput("table_dt",  width = 800)
    } else if (actual_but$active == "map") {
      leaflet::leafletOutput("r_viz", height = 600)
    } else {
      highchartOutput("hgch_viz", height = 600)
    }
  })


  output$side_table <- renderUI({
    req(df_temp())
    (html_table_block(df_temp()))

    })

}

shinyApp(ui, server)

