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
        title = tags$img(src='www/img/logo.png', width="80%", style="display: block; margin: auto;"),
        controlbarIcon = icon("table-cells")
      ),
      
      
      ## Sidebar content  
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          
          bs4Dash::menuItem("Home", tabName = "home", icon = icon("house")),
          bs4Dash::menuItem("Upload data", tabName = "upload", icon = icon("upload")),
          bs4Dash::menuItem("Single-omics", tabName = "singleOmics", icon = icon("window-minimize"),
                            bs4Dash::menuSubItem("PCA", tabName = "PCA"),
                            bs4Dash::menuSubItem("PLS-DA", tabName = "PLSDA")
          ),
          bs4Dash::menuItem("Pairwise", tabName = "sPLS", icon = icon("layer-group")),
          bs4Dash::menuItem("Multi-omics", tabName = "DIABLO", icon = icon("layer-group")),
          bs4Dash::menuItem("Help", tabName = "help", icon = icon("info"), startExpanded = F,
                            bs4Dash::menuSubItem("Plots", tabName = "help-plots"),
                            bs4Dash::menuSubItem(HTML("Feature selection <br> and tuning"), tabName = "help-tuning"),
                            bs4Dash::menuSubItem("DIABLO design matrix", tabName = "help-designmatrix")
          ),
          bs4Dash::menuItem("About", tabName = "about", icon = icon("info"))
        )
      ),
      
      ## Body content
      bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "home",
                           fluidRow(
                             column(width = 10,
                                    includeMarkdown(app_sys("app/www/01-home.md")))
                           )),
          bs4Dash::tabItem(tabName = "upload", mod_Upload_ui("upload")),
          bs4Dash::tabItem(tabName = "PCA", mod_PCA_ui("PCA")),
          bs4Dash::tabItem(tabName = "PLSDA", mod_PLSDA_ui("PLSDA")),
          bs4Dash::tabItem(tabName = "sPLS", mod_sPLS_ui("sPLS")),
          bs4Dash::tabItem(tabName = "DIABLO", mod_DIABLO_ui("DIABLO")),
          bs4Dash::tabItem(tabName = "help-plots", 
                           fluidRow(
                             column(width = 10,
                                    includeMarkdown(app_sys("app/www/help-plots.md")))
                           )),
          bs4Dash::tabItem(tabName = "help-tuning", 
                           fluidRow(
                             column(width = 10,
                                    includeMarkdown(app_sys("app/www/help-tuning.md")))
                           )),
          bs4Dash::tabItem(tabName = "help-designmatrix", 
                           fluidRow(
                             column(width = 10,
                                    includeMarkdown(app_sys("app/www/help-designmatrix.md")))
                           )),
          bs4Dash::tabItem(tabName = "about",
                           fluidRow(
                             column(width = 10,
                                    includeMarkdown(app_sys("app/www/about.md")))
                           ))
        )
        
      )
    ),
    
    bs4Dash::bs4DashFooter(
      fluidRow(
        column(width = 12, align = "center", 
               a(href = "https://www.ait.ac.at/en/", HTML("<b> AIT Austrian Institute of Technology"),
                 target = "_blank", rel="noreferrer noopener"), 

                br(),
               "Copyright (C) 2023" 
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
#' @import visNetwork
#' @import shinyvalidate
#' @import shinyWidgets
#' @import dplyr
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom grDevices col2rgb dev.off png
#' @importFrom stats mad
#' @importFrom utils read.csv write.csv2
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
    
    tags$link(rel = "stylesheet", type ="text/css", href="www/css/custom.css"),
    tags$link(rel = "stylesheet", type ="text/css", href="www/css/theme.css")
      )
}