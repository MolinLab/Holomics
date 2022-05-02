#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    bs4Dash::dashboardPage(
      dark = NULL,
      
      ## Header 
      bs4Dash::dashboardHeader(
        title = tags$img(src='www/logo_hell.png', width="80%", style="display: block; margin: auto;")
      ),
      
      
      ## Sidebar content  
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          
          bs4Dash::menuItem("Home", tabName = "home", icon = icon("home")),
          # bs4Dash::menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
          # bs4Dash::menuItem("Preprocessing", tabName = "preprocessing", icon = icon("wrench"), startExpanded = FALSE,
          #                   bs4Dash::menuSubItem("Impute values", tabName = "impute_vals"),
          #                   bs4Dash::menuSubItem("Normalization", tabName = "normalization"),
          #                   bs4Dash::menuSubItem("Outlier Detection", tabName = "outliers"),
          #                   bs4Dash::menuSubItem("Filtration", tabName = "filtration")
          # ),
          bs4Dash::menuItem("Single 'omics", tabName = "singleOmics", icon = icon("window-minimize")),
          bs4Dash::menuItem("Multi 'omics", tabName = "multiOmics", icon = icon("layer-group"), startExpanded = FALSE,
                            bs4Dash::menuSubItem("sPLS", tabName = "sPLS"),
                            bs4Dash::menuSubItem("DIABLO", tabName = "DIABLO"))
        )
      ),
      
      ## Body content
      bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "home",
                           fluidRow(
                             column(width = 10,
                                    includeMarkdown(app_sys("app/www/01-home.md"))) # create and change to app/www/home.md
                           )),
          bs4Dash::tabItem(tabName = "upload", h2("Upload Data Module")),
          bs4Dash::tabItem(tabName = "singleOmics", mod_SingleOmics_ui("singleOmics")),
          # bs4Dash::tabItem(tabName = "multiOmics", mod_MultiOmics_ui("multiOmics")),
          bs4Dash::tabItem(tabName = "sPLS", mod_sPLS_ui("sPLS")),
          bs4Dash::tabItem(tabName = "DIABLO", mod_DIABLO_ui("DIABLO"))
        )
        
      )
    ),
    
    bs4Dash::bs4DashFooter(
      fluidRow(
        column(width = 12, align = "center", 
               a(href = "https://www.ait.ac.at/en/", HTML("<b> AIT - Austrian Institute of Technology"), 
                 br()),
               "Copyright (C) 2022" #add license
               #add github link
               
        )
      )
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Holomics'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    
    tags$link(rel = "stylesheet", type ="text/css", href="www/custom.css"),
    tags$link(rel = "stylesheet", type ="text/css", href="www/theme.css")
  )
}

