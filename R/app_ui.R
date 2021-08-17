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
      
      
    ## Header 
      bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Holomics",
          color = "secondary",
          #href = "https://adminlte.io/themes/v3",
          #image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
          
        )
      ),
    
      
    ## Sidebar content  
    bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem("Home", tabName = "home"),
          bs4Dash::menuItem("Upload Data", tabName = "upload"),
          bs4Dash::menuItem("Preprocessing", tabName = "preprocessing", icon = icon("wrench"), startExpanded = FALSE,
            bs4Dash::menuSubItem("Impute values", tabName = "impute_vals"),
            bs4Dash::menuSubItem("Normalization", tabName = "normalization"),
            bs4Dash::menuSubItem("Outlier Detection", tabName = "outliers"),
            bs4Dash::menuSubItem("Filtration", tabName = "filtration")
          ),
          bs4Dash::menuItem("EDA", tabName = "visualisation", startExpanded = FALSE),
          bs4Dash::menuItem("Integration", tabName = "integration", startExpanded = FALSE,
            bs4Dash::menuSubItem("DIABLO", tabName = "diablo"),
            bs4Dash::menuSubItem("WGCNA", tabName = "wgcna")
          )

        )
      ),
      
    ## Body content
    bs4Dash::dashboardBody(
      #use_theme(mytheme), # <-- use the theme contained in in theme.R
      bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = "home",
                         fluidRow(
                           column(width = 10,
                                  includeMarkdown(paste0(resourcePaths(),"/01-home.md"))) # create and change to app/www/home.md
                         )),
        bs4Dash::tabItem(tabName = "upload", h2("Upload Data Module")),
        bs4Dash::tabItem(tabName = "visualisation", mod_PCA_ui("PCA_ui_1"))
      )
      
      )
    ),
    
    bs4Dash::bs4DashFooter(
      fluidRow(
        column(width = 12, align = "center", 
               a(href = "https://www.ait.ac.at/en/", HTML("<b> AIT - Austrian Institute of Technology"), 
               br()),
              "Copyright (C) 2021" #add license
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

