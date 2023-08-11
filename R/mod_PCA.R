#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PCA_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      bs4Dash::column(width = 12,
                      uiOutput(ns("dataSelComp")), 
                      uiOutput(ns("classSelComp")),
                      textOutput(ns("errorMsg")),
                      style = "display: flex; column-gap: 1rem"
      )
    ),
    fluidRow(
      bs4Dash::column(width = 5,
                       fluidRow(style = "padding-left: 7.5px;",
                                h1("PCA")
                       ),
                       fluidRow(
                         bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                      htmlOutput(ns("PCAinfotext"))
                         )
                       ),
                       fluidRow(width = 12,
                         getAnalysisParametersComponent(ns)
                       ),
                       fluidRow(
                         bs4Dash::tabBox(width = 12, collapsible = FALSE,
                           getScreePlot(ns),
                           getSamplePlot(ns),
                           getVariablePlot(ns),
                           getLoadingsPlot(ns),
                           getSelectedVarsPlot(ns)
                         )
                       )

      ),
      bs4Dash::column(width = 2,
                      getFilterBox(ns, "Perform feature selection", 
                                   "Automatically reduce the dataset by the optimal number of components 
                                   and the number of features per component."),
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                       fluidRow(style = "padding-left: 7.5px;",
                                h1("PCA reduced"),
                       ),
                       fluidRow(
                         bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                      htmlOutput(ns("PCAreducedinfotext"))
                         )
                       ),
                      fluidRow(width = 12,
                               bs4Dash::box(title = "Reduced dataset parameters", width = 12, collapsed = TRUE,
                                            fluidRow(style = "column-gap: 1rem",
                                                     textOutput(ns("ncomp.tuned")),
                                                     textOutput(ns("keepX.tuned")),
                                                     textOutput(ns("scale.tuned"))
                                            )
                               )
                      ),
                      fluidRow(width = 12,
                               bs4Dash::tabBox(width = 12, collapsible = FALSE,
                                               tabPanel("Correlations",
                                                        fluidRow(
                                                          bs4Dash::column(width = 12,
                                                                          plotOutput(ns("Correlations")),
                                                                          downloadButton(ns("Correlations.download"), "Save plot"))
                                                        )
                                               ),
                                               getScreePlot(ns, ".tuned"),
                                               getSamplePlot(ns, ".tuned"),
                                               getVariablePlot(ns, ".tuned"),
                                               getLoadingsPlot(ns, ".tuned"),
                                               getSelectedVarsPlot(ns, ".tuned")
                               )
                      )
      )
    )
  )
}

#' PCA Server Functions
#'
#' @noRd 
mod_PCA_server <- function(id, dataset, classes, multiDataset, tables){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::hide("Filter.download")
    shinyjs::hide("tunedCol")
  
    dataSelection <- reactiveValues()
    classSelection <- reactiveValues()
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL,  scale = T)
    
    results <- run_pca_analysis(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals)
    
    render_pca_ui_components(ns, input, output, tunedVals)
    
    observe_pca_ui_components(ns, input, output, dataset, dataSelection, classes, classSelection, results, 
                              useTunedVals, tunedVals, multiDataset, tables)
    
    generate_pca_plots(ns, input, output, dataSelection, classSelection, results$result, results$resultTuned, 
                       tunedVals, multiDataset, tables)
    
    generate_pca_error_messages(output, dataset, classes, dataSelection, classSelection)
      
    render_pca_infotexts(output)
  })
}

#' Render Ui functions
#' @noRd
render_pca_ui_components <- function(ns, input, output, tunedVals){
  
  renderIndivComps(ns, input, output, TRUE, tunedVals)
  
  renderVarComps(ns, input, output, TRUE, tunedVals)
  
  renderLoadComp(ns, input, output, TRUE, tunedVals)
  
  renderSelVarComp(ns, input, output, TRUE, tunedVals)
}

#' Observe different ui components
#' @noRd
observe_pca_ui_components <- function(ns, input, output, dataset, dataSelection, classes, classSelection, result, useTunedVals, tunedVals, multiDataset, tables){
  observeEvent(dataset$data, {
    output$dataSelComp <- renderUI({
      choice <- ""
      choices <- generateChoices(dataset$data)
      if (length(choices) != 0L) 
        choice <- choices[[1]]
      
      selection <- isolate(input$dataSelection)
      
      #same choice name as before but the data is not necessarily the same!
      #observeEvent of input$dataSelection will not be triggered
      if(!is.null(selection) && selection == choice){
        dataSelection$data <- dataset$data[[choice]]
      }
      
      getSelectionComponent(ns("dataSelection"), "Select dataset:", choices, width = "fit-content")
    })
  })
  
  observeEvent(input$dataSelection, {
    dataSelection$data <- dataset$data[[input$dataSelection]]
    dataSelection$name <- input$dataSelection
  })
  
  observeEvent(classes$data, {
    output$classSelComp <- renderUI({
      choices <- generateChoices(classes$data)
      choice = ""
      if (length(choices) != 0L) 
        choice <- choices[[1]]
      
      selection <- isolate(input$classSelection)
      
      #same choice name as before but the data is not necessarily the same!
      #observeEvent of input$dataSelection will not be triggered
      if(!is.null(selection) && selection == choice){
        classSelection$data <- classes$data[[choice]]
      }
      
      getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices, width = "fit-content")
    })
  })
  
  observeEvent(input$classSelection, {
    classSelection$data <- classes$data[[input$classSelection]]
  })
  
  observeEvent(dataSelection$data, {
    useTunedVals(FALSE)
    shinyjs::hide("tunedCol")
    shinyjs::hide("Filter.download")
  })
  
  # Observe tune button
  observeEvent(input$tune, {
    tryCatch({
      pca_filterByLoadings(input, output, dataSelection, result, tunedVals, multiDataset, tables)
    
      if (!is.null(tunedVals$ncomp)){
        shinyjs::show("tunedCol")
        shinyjs::show("Filter.download")
        useTunedVals(T)
      }
    }, error = function(cond){
      getErrorMessage(cond, trim = F)
    })
  })
}

#' Filter function
#' @noRd
pca_filterByLoadings <- function(input, output, dataSelection, result, tunedVals, multiDataset, tables){
  req(dataSelection$data$omicsData)

  withProgress(message = 'Reducing the dataset ... Please wait!', value = 1/3, {
    
    error <- F
    #get optimal number of components
    error <- tryCatch({
      tune <- mixOmics::tune.pca(dataSelection$data$omicsData, scale = input$scale)
      ncomp <- min(which(tune$cum.var > 0.8))
      error <- F
    }, error = function(cond){
      getErrorMessage(cond)
      return(T)
    })
    

    if (error){
      tunedVals$ncomp <- NULL
    } else{
      if (ncomp <= 15){
        #get optimal number of features per component
        grid.keepX <- getTestKeepX(ncol(dataSelection$data$omicsData))
        folds <- floor(nrow(dataSelection$data$omicsData)/3)
        
        BPPARAM <- BiocParallel::SnowParam(workers = max(parallel::detectCores()-1, 2))
        suppressWarnings({
          tune.spca.result <- mixOmics::tune.spca(dataSelection$data$omicsData, ncomp = ncomp,
                                                  test.keepX = grid.keepX, scale = input$scale,
                                                  nrepeat = 10, folds = folds, BPPARAM = BPPARAM)
          
          keepX <- tune.spca.result$choice.keepX[1:ncomp]
          
          incProgress(1/3)
          
          #filter dataset
          spca.result <- mixOmics::spca(dataSelection$data$omicsData, ncomp = ncomp, keepX = tune.spca.result$choice.keepX[1:ncomp],
                                        input$scale, max.iter = 10000)
        })
        
        sel_feature <- c()
        for (comp in 1:ncomp){
          loadings <- mixOmics::selectVar(spca.result, comp = comp)$name
          sel_feature <- c(sel_feature, loadings)
        }
        
        feature_cols <- (names(dataSelection$data$omicsData) %in% sel_feature)
        result <- dataSelection$data$omicsData[, feature_cols]
        multiDataset$data[[paste0(dataSelection$name, "_pca_reduced")]] <- list(omicsData = result, name = dataSelection$data$name)
        
        # extend table in upload with available datasets and if there is already an entry with the same name remove it
        tables$data <- as.data.frame(tables$data)
        tables$data <- tables$data[tables$data$Name != paste0(dataSelection$name, "_pca_reduced"), ]
        tables$data <- as.matrix(tables$data)
        tables$data <- extendDataTable(tables$data, paste0(dataSelection$name, "_pca_reduced"), "-", nrow(result), ncol(result),
                                       FALSE, "multi", dataSelection$data$name)
        
        #Correlations plot
        output$Correlations <- renderPlot(plot(tune.spca.result))
  
        output$Correlations.download <- getDownloadHandler("PCA_correlations.png", function(){plot(tune.spca.result)})
        
        tunedVals$ncomp <- ncomp
        tunedVals$keepX <- keepX
        tunedVals$scale <- input$scale
        
        incProgress(1/3)
      } else {
        
        tunedVals$ncomp <- NULL
        
        getShinyWarningAlert("The number of components for reducing this dataset would be above 15, 
                               which would result in a huge runtime to reduce the dataset. 
                               This is why the feature selection was aborted and we recommend either using the whole dataset or using 
                               PLS-DA for the feature selection step.")
        incProgress(2/3)
      }
    }
  })
}

#' Run analysis
#' @noRd
run_pca_analysis <- function(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals){
  result <- reactive({
    req(dataSelection$data$omicsData)
    req(nrow(classSelection$data) == nrow(dataSelection$data$omicsData))
    req(identical(classSelection$data[,1], rownames(dataSelection$data$omicsData)))
    
    msg <- checkDataNcompCompatibility(dataSelection$data$omicsData, input$ncomp) 
    output$parameters.error <- renderText(msg)
    
    if(msg == ""){
      X <- dataSelection$data$omicsData
      finished = F
      while(!finished){
        X <- tryCatch({
          result <- mixOmics::pca(X, ncomp = input$ncomp,
                                  scale = input$scale)
          finished = T
        }, error = function(cond){
          if (grepl("columns with zero variance", cond$message, fixed = T)){
            indices <- stringr::str_match(cond$message, "columns with zero variance in 'X': ([\\d,]*).")[2]
            X <- X[, -lapply(strsplit(indices, ","), as.numeric)[[1]]]
            return (X)
          } else {
            getErrorMessage(cond)
            result <- NULL
            return(NULL)
          }
        })
        
        finished <- ifelse(finished, finished, is.null(X))
      }
    } else {
      result <- NULL
    }
    result
  })
  
  result.tuned <- reactive({
    if (useTunedVals()){
      req(dataSelection$data$omicsData)
      req(nrow(classSelection$data) == nrow(dataSelection$data$omicsData))
      req(identical(classSelection$data[,1], rownames(dataSelection$data$omicsData)))
      
      X <- dataSelection$data$omicsData
      finished = F
      while(!finished){
        tryCatch({
          suppressWarnings({
            result.tuned <- mixOmics::spca(X, ncomp = tunedVals$ncomp, keepX = tunedVals$keepX,
                                          scale = tunedVals$scale, max.iter = 1000)
          })
          finished = T
        }, error = function(cond){
            getErrorMessage(cond)
            result.tuned <- NULL
        })
      }
      result.tuned
    }
  })
  
  return(list(result = result, resultTuned = result.tuned))
}

#' Generate the error messages
#' @noRd
generate_pca_error_messages <- function(output, dataset, classes, dataSelection, classSelection){
  # Error message when selection is incompatible or  data or classes are missing
  inputSelChange <- reactive({  #change of input values or selected values
    list(dataSelection$data, classSelection$data)
  })
  
  observeEvent(inputSelChange(), {
    output$errorMsg <- renderText({
      if(length(dataset$data) == 0){
        "Please upload some data to be able to use the analysis!"
      } else if(length(classes$data) == 0){
        "Please upload some classes/label information to be able to use the analysis!"
      } else {
        class <- classSelection$data
        data <- dataSelection$data$omicsData
        
        req(data)
        req(class)
        if (length(data) != 0 && nrow(class) != nrow(data)){
          "The selected data and classes are incompatible due to their different amount of samples! 
            Please change your selection!"
        } else if(!identical(class[,1], rownames(data))){
          "The selected data and classes are incompatible as they do not contain the same sample(name)s! 
            Please change your selection!"
        } else {
          ""
        }
      }
    })
  })
}

#' Business logic functions
#' @noRd
generate_pca_plots <- function(ns, input, output, dataSelection, classSelection, result, resultTuned, tunedVals, multiDataset, tables){
  # Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  comp.indiv.tuned <- getCompIndivReactive(input, tuned = TRUE)
  comp.var.tuned <- getCompVarReactive(input, tuned = TRUE)
  
  # plot functions
  plot.scree <- function() {
    if(!is.null(result())){
      plot(result())
    }
  }
  
  plot.indiv <- function(){
    if(!is.null(result())){
      req(classSelection$data)
      legend.title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result(), classes = classSelection$data[,2], title = paste("PCA on", dataSelection$data$name ,"data"), legend.title = legend.title, 
                  comp = comp.indiv(), indNames = input$indiv.names, col.per.group = colors)
      } else {
        plotIndiv(result(), classes = classSelection$data[,2], title = paste("PCA on", dataSelection$data$name ,"data"), legend.title = legend.title, 
                  comp = comp.indiv(), indNames = input$indiv.names)
      }
    }
  }
  
  plot.var <- function(){
    if(!is.null(result())){
      plotVar(result(), comp.var(), input$var.names)
    }
  }
  
  plot.load <- function(){
    if(!is.null(result())){
      req(input$load.comp)
      plotLoadings(result(), as.numeric(input$load.comp))
    }
  }
  
  table.selVar <- function(){
    if(!is.null(result())){
      req(input$sel.var.comp)
      selectVar(result(), as.numeric(input$sel.var.comp))
    }
  }
  
  plot.scree.tuned <- function() {
    if(!is.null(resultTuned())){
      plot(resultTuned())
    }
  }
  
  plot.indiv.tuned <- function(){
    if(!is.null(resultTuned())){
      legend.title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(resultTuned(), classes = classSelection$data[,2], title = paste("PCA on", dataSelection$data$name ,"data"), 
                  legend.title = legend.title, comp = comp.indiv.tuned(), 
                  indNames = input$indiv.names.tuned, col.per.group = colors)
      } else {
        plotIndiv(resultTuned(), classes = classSelection$data[,2], title = paste("PCA on", dataSelection$data$name ,"data"), 
                  legend.title = legend.title, comp = comp.indiv.tuned(), 
                  indNames = input$indiv.names.tuned)
      }
    }
  }
  
  plot.var.tuned <- function(){
    if(!is.null(resultTuned())){
      plotVar(resultTuned(), comp.var.tuned(), input$var.names.tuned)
    }
  }
  
  plot.load.tuned <- function(){
    if(!is.null(resultTuned())){
      req(input$load.comp.tuned)
      plotLoadings(resultTuned(), as.numeric(input$load.comp.tuned))
    }
  }
  
  table.selVar.tuned <- function(){
    if(!is.null(resultTuned())){
      req(input$sel.var.comp.tuned)
      selectVar(resultTuned(), as.numeric(input$sel.var.comp.tuned))
    }
  }
  
  #output plots
  # Scree plot
  output$Scree <- renderPlot(
    plot.scree()
  )
  
  # Sample plot
  output$Indiv <- renderPlot(
    plot.indiv()
  )
  
  # Correlation Circle plot
  output$Var <- renderPlot(
    plot.var() 
  )
  
  # Loading plot
  output$Load <- renderPlot(
    plot.load()
  )
  
  #Selected Variables Tables
  output$Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  #filtered/tuned
  # Scree plot
  output$Scree.tuned <- renderPlot(
    plot.scree.tuned()
  )
  
  # Sample plot
  output$Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  # Correlation Circle plot
  output$Var.tuned <- renderPlot(
    plot.var.tuned() 
  )
  
  # Loading plot
  output$Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  #Selected Variables Tables
  output$Sel.Var.tuned <- DT::renderDataTable(
    table.selVar.tuned()
  )
  
  # Tuned parameters
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Features of dataset: ",  paste(tunedVals$keepX, collapse = ", "))
  )
  
  output$scale.tuned <- renderText(
    paste("scaled: ",  tunedVals$scale)
  )
  
  # Download handler
  dataName <- reactive({
    dataSelection$name 
  })

  output$Indiv.download <- getDownloadHandler("PCA_Sampleplot.png", plot.indiv)
  output$Scree.download <- getDownloadHandler("PCA_Screeplot.png", plot.scree)
  output$Var.download <- getDownloadHandler("PCA_CorrelationCircleplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PCA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PCA_SelectedFeatures.csv", table.selVar, type = "csv")
  
  output$Indiv.download.tuned <- getDownloadHandler("PCA_reduced_Sampleplot.png", plot.indiv.tuned)
  output$Scree.download.tuned <- getDownloadHandler("PCA_reduced_Screeplot.png", plot.scree.tuned)
  output$Var.download.tuned <- getDownloadHandler("PCA_reduced_CorrelationCircleplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("PCA_reduced_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$SelVar.download.tuned <- getDownloadHandler("PCA_reduced_SelectedFeatures.csv", table.selVar.tuned, type = "csv")
  
  output$Filter.download <- downloadHandler(
    filename = function() {
      paste0(dataName(), "_pca_reduced.xlsx")
    },
    content = function(file){
      openxlsx::write.xlsx(multiDataset$data[[paste0(dataName(), "_pca_reduced")]]$omicsData, 
                           file, rowNames = TRUE, colNames = TRUE)
    }
  )

}

#' Information texts
#' @noRd
render_pca_infotexts <- function(output){
  output$PCAinfotext <- renderText({
    HTML("The <b>P</b>rincipal <b>C</b>omponent <b>A</b>nalysis decreases the size of the high-dimensional dataset, 
      removes noise from the dataset and presents the similarities between the samples.
      It works unsupervised and determines uncorrelated and orthogonal principal components (PC) in the data.
      Therefore, the PCA only works with the data matrix containing the samples and features information without knowing the classification of the samples.
      It helps to identify characteristics of the data and eventual biases and artefacts by visualising the PCs with the respective features and samples. <br/>
      Additional information can be found on the <a class='mixOmics-link' href='http://mixomics.org/methods/spca/' target='_blank'>mixOmics website</a> and
      in several scientific papers (e.g. <a class='ref-link' href='https://link.springer.com/article/10.1186/1471-2105-13-24' target='_blank'>Yao et.al. (2012)</a>).
      More information about the plots and the feature selection and tuning methods can be found on our <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-plots').click();\">'Plots-Helppage'</a> and
      <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.</br>
      <b>Please adjust the number of components in the 'Analysis parameters' tab according to your selected dataset.</b> 
      We recommend to use a number of components that explains at least 80% of the dataset variance.")
  })
  
  output$PCAreducedinfotext <- renderText({
    HTML("The dataset feature selection algorithm calculates the optimal number of components (number of components which explain at least 80% variance) 
         and the optimal number of features per component.<br/>
         According to this information the dataset was reduced and the reduced dataset is used for the plots. <br/>
         More detailed information can be found on our 
         <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.
         ")
  })
}