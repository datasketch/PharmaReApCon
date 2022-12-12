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
    shinypanels::panelsPage(
      shinypanels::panel(title = "Filters",
                         id = "azul",
                         color="#6fcbff",
                         width = 250,
                           body = div(
                             tags$head(tags$script(src="handlers.js")),
                           div(style = "!important;margin-bottom: 1%;",
                               uiOutput("generalFilters"),
                               uiOutput("sel_country"),
                               uiOutput("sel_status"),
                               uiOutput("generalFilters2"),
                               uiOutput("sel_country_ap"),
                               uiOutput("sel_status_ap"),
                               uiOutput("generalFilters3"),
                               uiOutput("sel_country_ct"),
                               uiOutput("sel_supplier_ct"),
                               uiOutput("sel_vaccine_ct")

                               # , shiny::actionButton("todas2", "Tooltip on the left"),
                               #spsComps:: bsTip("primary", status = "primary")


                           )
                         )
      ),
      shinypanels::panel(title = "Visualization",
                         id = "naranja",
                         header_right = div(class="flex-container",style="display:flex;",
                                            div(class="flex-item-left",uiOutput("viz_icons")),
                                            div(class="flex-item-right",uiOutput("descargas"))
                         ),
                         can_collapse = FALSE,
                         color = "chardonnay",
                         body = div(shinybusy::add_busy_spinner(spin = "fading-circle"),
                                    uiOutput("viz")
                         )
      ),
      shinypanels::panel(title = "Details",
                         id="verde",
                         width = 300,
                         body=
                         div(style="flex-grow: 1; max-width: 300px;overflow: scroll;max-height: 620px",
                             htmlOutput("side_table")
                         )

      )
    )


  )
}

server <- function(input, output, session) {



  question_buttons1 <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
    if (is.null(ids)) stop("Please enter identifiers for each question")
    if (is.null(labels)) stop("Please enter labels for each question")

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
      shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed", icon=icon("arrow-right", class="fa",  lib ="font-awesome"))# %>%

    })
    # l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    # l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))

    l
  }

  question_buttons2 <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
    if (is.null(ids)) stop("Please enter identifiers for each question")
    if (is.null(labels)) stop("Please enter labels for each question")

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
     shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed")# %>%

    })
    # l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    # l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))

    l
  }


  question_buttons3 <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
    if (is.null(ids)) stop("Please enter identifiers for each question")
    if (is.null(labels)) stop("Please enter labels for each question")

    df <- data.frame(id = ids, questions = labels)
    l <- purrr::map(1:nrow(df), function(z){
      shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions,class = "needed")# %>%

    })
    # l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    # l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))

    l
  }
  output$generalFilters <- renderUI({
    question_buttons1(c("request"),
                     c( "Request"))

    })

   output$generalFilters2 <- renderUI({
      question_buttons1(c( "appeals"),
                        c( "Appeals"))

      })


   output$generalFilters3 <- renderUI({
     question_buttons1(c( "contracts"),
                       c( "Contracts details"))

   })

  quest_choose <- reactive({
    last_btn <- input$last_click
    # print("lassst")
    # print(last_btn)
    # # input$sel_country=NULL
    # # input$sel_status=NULL
    # # if(input$sel_valor) input$sel_valor="cantidad"
    # # if (is.null(last_btn)) last_btn <- "Request"
    # print(last_btn)
    last_btn
  })

  observeEvent(input$last_click, {
    shinyjs::toggle(id = "request")
    if(input$last_click == "request"){
      updateActionButton(session,"request", icon=icon("arrow-down", class="fa",  lib ="font-awesome"))
    }else{
      updateActionButton(session,"request",icon=icon("arrow-right", class="fa",  lib ="font-awesome"))
    }


    if(input$last_click == "appeals"){
      updateActionButton(session,"appeals", icon=icon("arrow-down", class="fa",  lib ="font-awesome"))
    }else{
      updateActionButton(session,"appeals",icon=icon("arrow-right", class="fa",  lib ="font-awesome"))
    }

    if(input$last_click == "contracts"){
      updateActionButton(session,"contracts", icon=icon("arrow-down", class="fa",  lib ="font-awesome"))
    }else{
      updateActionButton(session,"contracts",icon=icon("arrow-right", class="fa",  lib ="font-awesome"))
    }
  }, ignoreInit = TRUE)


titleviz <-reactive({

})


viz_opts <- reactive({
    req(quest_choose())
    req(actual_but$active)

    vart_country <- NULL
    vart_status <- NULL
    vart_supplier <- NULL
    vart_vaccine <- NULL

    if (!is.null(filter_viz$country)) {
      vart_country <-  vector()
      vart_country <- append(vart_country,filter_viz$country)

    }
    if (!is.null(filter_viz$status)) {
      vart_status <- vector()
      vart_status <-  append(vart_status,filter_viz$status)

    }
    if (!is.null(filter_viz$supplier)) {
      vart_supplier <-  vector()
      vart_supplier <-  append(vart_supplier,filter_viz$supplier)

    }

    if (!is.null(filter_viz$vaccine)) {
      vart_vaccine <- vector()
      vart_vaccine <- append(vart_vaccine,filter_viz$vaccine)

    }
    if(actual_but$active=="treemap" | actual_but$active=="bar"){


      if(actual_but$active=="treemap"){

        df <- request_country_get_data_graph(quest_choose(),vart_country,vart_status,vart_supplier, vart_vaccine, type="treemap")

          l <- show_bar(df, "Country",paste0("Country: ","{Country} Total {count}"))


      }
      else{
        df <- request_country_get_data_graph(quest_choose(),vart_country,vart_status,vart_supplier, vart_vaccine,type="bar")
          if(quest_choose()!="contract") {
            l <- show_bar(df, "Status",paste0("Status: ","{Status} Total {count}"))
          }
        else{
          l <- show_bar(df, "Vaccine")
          }
      }


    }
    else{


      df <- request_country_get_data_map(quest_choose(),vart_country,vart_status,vart_supplier, vart_vaccine)

      l <- show_map(df)

    }

    l
  })

    output$sel_valor <- renderUI({

  })



  output$sel_country <- renderUI({
    default_select <- NULL
    req(quest_choose())


    if(quest_choose()=="request"){
      df <- filter_make("request","Country")



     selectizeInput("sel_country","Country",
                     df,
                     default_select, multiple=TRUE, width='200px')
    }

  })


  output$sel_status <- renderUI({
    req(quest_choose())
    default_select <- NULL
    if(quest_choose()=="request"){
      df <- filter_make("request","Status")
      selectizeInput("sel_status","Status",
                     df,
                     default_select, multiple=TRUE, width='200px')
   }

  })



  output$sel_country_ap <- renderUI({
    default_select <- NULL
    req(quest_choose())

    if(quest_choose()=="appeals"){

      df <- filter_make("appeals","Country")


    selectizeInput("sel_country_ap","Country",
                   df,
                   default_select, multiple=TRUE, width='200px')
    }

  })


  output$sel_status_ap <- renderUI({
    req(quest_choose())
    default_select <- NULL
    if(quest_choose()=="appeals"){

      df <- filter_make("appeals","Status")

     selectizeInput("sel_status_ap","Status",
                   df,
                   default_select, multiple=TRUE, width='200px')
    }

  })



  output$sel_country_ct <- renderUI({
    default_select <- NULL
    req(quest_choose())

    if(quest_choose()=="contracts"){

      df <- filter_make("contracts","Country")


      selectizeInput("sel_country_ct","Country",
                     df,
                     default_select, multiple=TRUE, width='200px')
    }

  })

  output$sel_supplier_ct <- renderUI({
    default_select <- NULL
    req(quest_choose())

    if(quest_choose()=="contracts"){

      df <- filter_make("contracts","Supplier")


      selectizeInput("sel_supplier_ct","Supplier",
                     df,
                     default_select, multiple=TRUE, width='200px')
    }

  })


  output$sel_vaccine_ct <- renderUI({
    default_select <- NULL
    req(quest_choose())

    if(quest_choose()=="contracts"){

      df <- filter_make("contracts","Vaccine")


      selectizeInput("sel_vaccine_ct","Vaccine",
                     df,
                     default_select, multiple=TRUE, width='200px')
    }

  })


  possible_viz <- reactive({
    req(quest_choose())
    v <- c("map","bar","treemap","table")
    v
  })


  actual_but <- reactiveValues(active = NULL)
  filter_viz <- reactiveValues(country = NULL, status=NULL)


  observe({
    if (is.null(input$viz_selection)) return()
    req(possible_viz())
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }

    req(quest_choose())
    if(quest_choose()=="request"){
      filter_viz$country <- input$sel_country
      filter_viz$status <- input$sel_status
      filter_viz$supplier <- NULL
      filter_viz$vaccine <- NULL
    }
    if(quest_choose()=="appeals"){
      filter_viz$country <- input$sel_country_ap
      filter_viz$status <- input$sel_status_ap
      filter_viz$supplier <- NULL
      filter_viz$vaccine <- NULL
    }
    if(quest_choose()=="contracts"){
      filter_viz$status <- NULL
      filter_viz$country <- input$sel_country_ct
      filter_viz$supplier <- input$sel_supplier_ct
      filter_viz$vaccine <- input$sel_vaccine_ct
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
    req(quest_choose())
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
    req(quest_choose())
    vart_country <- NULL
    vart_status <- NULL
    vart_supplier <- NULL
    vart_vaccine <- NULL
    if (!is.null(filter_viz$country)) {
      vart_country <-  vector()
      vart_country <- append(vart_country,filter_viz$country)

    }
    if (!is.null(filter_viz$status)) {
      vart_status <- vector()
      vart_status <- append(vart_country,filter_viz$status)

    }

    if (!is.null(filter_viz$supplier)) {
      vart_supplier <- vector()
      vart_supplier <- append(vart_supplier,filter_viz$supplier)

    }

    if (!is.null(filter_viz$vaccine)) {
      vart_vaccine <- vector()
      vart_vaccine <- append(vart_vaccine,filter_viz$vaccine)

    }


    df <- request_country_get_data_table(quest_choose(),vart_country,vart_status,vart_supplier, vart_vaccine)
    df
  })

  output$table_dt <- DT::renderDataTable({
    req(quest_choose())
    req(df_temp())
    # df=request_country_get_data_table(quest_choose(),vart_country,vart_status,vart_supplier, vart_vaccine)
    l <- show_table(df_temp())
    l

  })

  output$descargas <- renderUI({
    req(quest_choose())
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz", dropdownLabel ="Download", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown", text = "Descargar")
    } else {
      dsmodules::downloadTableUI("dropdown_table", dropdownLabel = "Download", formats = c("csv", "xlsx", "json"), display = "dropdown", text = "Descargar")
    }
  })

   dsmodules::downloadTableServer("dropdown_table", element = reactive( df_temp() ), formats = c("csv", "xlsx", "json"))
   dsmodules::downloadImageServer("download_viz", element = reactive(hgch_viz()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")


  output$viz <- renderUI({
     if (is.null(actual_but$active))
       actual_but$active <- "map"

    if (actual_but$active == "table") {
      dataTableOutput("table_dt",  width = 800)
    } else if (actual_but$active == "map") {
      leaflet::leafletOutput("r_viz", height = 600)
    } else {
      highchartOutput("hgch_viz", height = 600)
    }
  })


  output$side_table <- renderUI({
    req(quest_choose())
    req(df_temp())
    print(quest_choose())
    if(quest_choose()=="request") {
      html_table_block(df_temp(),"Request..doc.")
     }
     else{
       if(quest_choose()=="contracts") { html_table_block(df_temp(),"Contracts") }
       else {
          if(quest_choose()=="appeals") { html_table_block(df_temp(),"Attachments") }
       }
      }
    })

}

shinyApp(ui, server)

