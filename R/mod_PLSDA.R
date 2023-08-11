#' PLSDA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PLSDA_ui <- function(id){
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
                               h1("PLS-DA")
                      ),
                      fluidRow(
                        bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                     htmlOutput(ns("PLSDAinfotext"))
                        )
                      ),
                      fluidRow(width = 12,
                               getAnalysisParametersComponent(ns)
                      ),
                      fluidRow(
                        bs4Dash::tabBox(width = 12, collapsible = FALSE,
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
                               h1("PLS-DA reduced"),
                      ),
                      fluidRow(
                        bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                     htmlOutput(ns("PLSDAreducedinfotext"))
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
                                               getErrorRatePlot(ns),
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

#' PLSDA Server Functions
#'
#' @noRd 
mod_PLSDA_server <- function(id, dataset, classes, multiDataset, tables){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shinyjs::hide("Filter.download")
    shinyjs::hide("tunedCol")
    
    dataSelection <- reactiveValues()
    classSelection <- reactiveValues()
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL,  scale = T)
    
    results <- run_plsda_analysis(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals)
    
    render_plsda_ui_components(ns, input, output, tunedVals)
    
    observe_plsda_ui_components(ns, input, output, dataset, dataSelection, classes, classSelection, results, 
                              useTunedVals, tunedVals, multiDataset, tables)
    
    generate_plsda_plots(ns, input, output, dataSelection, classSelection, results$result, results$resultTuned, 
                       tunedVals, multiDataset, tables)
    
    generate_plsda_error_messages(output, dataset, classes, dataSelection, classSelection)
    
    render_plsda_infotexts(output)
  })
}

#' Render Ui functions
#' @noRd
render_plsda_ui_components <- function(ns, input, output, tunedVals){
  renderIndivComps(ns, input, output, TRUE, tunedVals)
  
  renderVarComps(ns, input, output, TRUE, tunedVals)
  
  renderLoadComp(ns, input, output, TRUE, tunedVals)
  
  renderSelVarComp(ns, input, output, TRUE, tunedVals)
}

#' Observe different ui components
#' @noRd
observe_plsda_ui_components <- function(ns, input, output, dataset, dataSelection, classes, classSelection, 
                                        result, useTunedVals, tunedVals, multiDataset, tables){
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
      plsda_filterByLoadings(input, output, dataSelection, classSelection, result, tunedVals, multiDataset, tables)
      
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
plsda_filterByLoadings <- function(input, output, dataSelection, classSelection, result, tunedVals, multiDataset, tables){
  req(dataSelection$data$omicsData)
  req(classSelection$data)
  
  withProgress(message = 'Reducing the dataset ... Please wait!', value = 1/3, {
    
    error <- F
    #get optimal number of components
    error <- tryCatch({
      Y <- classSelection$data[,2]
      result <- mixOmics::plsda(dataSelection$data$omicsData, Y = Y,
                                ncomp = input$ncomp , scale = input$scale)
      
      #get optimal number of components and number of features per component
      grid.keepX <- getTestKeepX(ncol(dataSelection$data$omicsData))
      tune.splsda.result <- mixOmics::tune.splsda(dataSelection$data$omicsData,Y = Y, ncomp = input$ncomp,
                                                  test.keepX = grid.keepX, scale = input$scale,
                                                  validation = c('Mfold'),
                                                  folds = getFolds(Y),
                                                  dist = 'max.dist',
                                                  nrepeat = 50,
                                                  progressBar = TRUE)
      
      # max. possible number is input$ncomp
      ncomp <- tune.splsda.result$choice.ncomp$ncomp
      keepX <- tune.splsda.result$choice.keepX[1:ncomp]
      
      incProgress(1/3)
      
      #filter dataset
      splsda.result <- mixOmics::splsda(dataSelection$data$omicsData, Y = Y, ncomp = ncomp, keepX = keepX, scale = input$scale)
      
      sel_feature <- c()
      for (comp in 1:ncomp){
        loadings <- mixOmics::plotLoadings(splsda.result, comp = comp, method = 'mean', contrib = 'max')
        sel_feature <- c(sel_feature, rownames(loadings))
      }
      
      feature_cols <- (names(dataSelection$data$omicsData) %in% sel_feature)
      result <- dataSelection$data$omicsData[, feature_cols]
      multiDataset$data[[paste0(dataSelection$name, "_plsda_reduced")]] <- list(omicsData = result, name = dataSelection$data$name)
      
      #if there is already an entry with the same name remove it
      tables$data <- as.data.frame(tables$data)
      tables$data <- tables$data[tables$data$Name != paste0(dataSelection$name, "_plsda_reduced"), ]
      tables$data <- as.matrix(tables$data)
      tables$data <- extendDataTable(tables$data, paste0(dataSelection$name, "_plsda_reduced"), "-", nrow(result), ncol(result),
                                     FALSE, "multi", dataSelection$data$name)
      
      #set error rate plot    
      errorPlot <- plot(tune.splsda.result)
      output$ErrorRate <- renderPlot(errorPlot)
      output$ErrorRate.download <- getDownloadHandler("PLSDA_error_rates.png", type = "ggplot", plot = errorPlot)
        
      incProgress(1/3)
      error <- F
    }, error = function(cond){
      if (grepl("system is computationally singular", cond$message, fixed = T)){
        getShinyErrorAlert("An error appeared while trying to reduce the dataset. <br> 
                        Please reduce your number of components and try again!", html = T)
      } else {
        getErrorMessage(cond)
      }
      return(T)
    })
    
    if (error){
      tunedVals$ncomp <- NULL
    } else{
      tunedVals$ncomp <- ncomp
      tunedVals$keepX <- keepX
      tunedVals$scale <- input$scale
    }
  })
}

#' Run analysis
#' @noRd
run_plsda_analysis <- function(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals){
  result <- reactive({
    req(dataSelection$data$omicsData)
    req(classSelection$data)
    req(nrow(classSelection$data) == nrow(dataSelection$data$omicsData))
    req(identical(classSelection$data[,1], rownames(dataSelection$data$omicsData)))
    
    msg <- checkDataNcompCompatibility(dataSelection$data$omicsData, input$ncomp) 
    output$parameters.error <- renderText(msg)
    
    if(msg == ""){
      tryCatch({
        result <- mixOmics::plsda(dataSelection$data$omicsData, Y = classSelection$data[,2],
                                  ncomp = input$ncomp , scale = input$scale)
      }, error = function(cond){
        getErrorMessage(cond)
        result <- NULL
      })
    } else {
      result <- NULL
    }
    result
  })
  
  result.tuned <- reactive({
    if (useTunedVals()){
      req(dataSelection$data$omicsData)
      req(classSelection$data)
      req(nrow(classSelection$data) == nrow(dataSelection$data$omicsData))
      req(identical(classSelection$data[,1], rownames(dataSelection$data$omicsData)))
      
      msg <- checkDataNcompCompatibility(dataSelection$data$omicsData, input$ncomp) 
      output$parameters.error <- renderText(msg)
      
      if(msg == ""){
        tryCatch({
          result.tuned <- mixOmics::splsda(dataSelection$data$omicsData, Y = classSelection$data[,2],
                                          ncomp = tunedVals$ncomp, keepX = tunedVals$keepX,
                                          scale = tunedVals$scale)
        }, error = function(cond){
          getErrorMessage(cond)
          result.tuned <- NULL
        })
      } else {
        result.tuned <- NULL
      }
      result.tuned
    }
  })
  
  return(list(result = result, resultTuned = result.tuned))
}

#' Generate the error messages
#' @noRd
generate_plsda_error_messages <- function(output, dataset, classes, dataSelection, classSelection){
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
generate_plsda_plots <- function(ns, input, output, dataSelection, classSelection, result, resultTuned, tunedVals, multiDataset, tables){
  # Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  comp.indiv.tuned <- getCompIndivReactive(input, tuned = TRUE)
  comp.var.tuned <- getCompVarReactive(input, tuned = TRUE)

  # plot functions
  plot.indiv <- function(){
    req(classSelection$data)
    if (!is.null(result())){
      legend.title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result(), classes = classSelection$data[,2], title = paste("PLS-DA on", dataSelection$data$name ,"data"), 
                  legend.title = legend.title, comp = comp.indiv(), indNames = input$indiv.names, col.per.group = colors)
      } else {
        plotIndiv(result(), classes = classSelection$data[,2], title = paste("PLS-DA on", dataSelection$data$name ,"data"), 
                  legend.title = legend.title, comp = comp.indiv(), indNames = input$indiv.names)
      }
    }
  }
  
  plot.var <- function(){
    if (!is.null(result())){
      plotVar(result(), comp.var(), input$var.names)
    }
  }
  
  plot.load <- function(){
    req(classSelection$data)
    req(input$load.comp)
    if (!is.null(result())){
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotLoadings(result(), as.numeric(input$load.comp),
                     contrib = 'max', method = 'mean', legend.color = colors)
      } else {
        plotLoadings(result(), as.numeric(input$load.comp),
                     contrib = 'max', method = 'mean')    
      }
    }
  }
  
  table.selVar <- function(){
    req(input$sel.var.comp)
    if (!is.null(result())){
      selectVar(result(), as.numeric(input$sel.var.comp))
    }
  }
  
  plot.indiv.tuned <- function(){
    req(classSelection$data)
    if (!is.null(resultTuned())){
      legend.title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(resultTuned(), classes = classSelection$data[,2], title = paste("PLS-DA on", dataSelection$data$name ,"data"), 
                  legend.title = legend.title, comp = comp.indiv.tuned(), indNames = input$indiv.names.tuned, col.per.group = colors)
      } else {
        plotIndiv(resultTuned(), classes = classSelection$data[,2], title = paste("PLS-DA on", dataSelection$data$name ,"data"), 
                  legend.title = legend.title, comp = comp.indiv.tuned(), indNames = input$indiv.names.tuned)
      }
    }
  }
  
  plot.var.tuned <- function(){
    if (!is.null(resultTuned())){
      #with only one component the plot does not work
      if(max(comp.var.tuned()) == 1){
        output$var.error.tuned <- renderText({
          "There need to be at least two components to render this plot!"
        })
      } else {
        plotVar(resultTuned(), comp.var.tuned(), input$var.names.tuned)
      }
    }
  }
  
  plot.load.tuned <- function(){
    req(classSelection$data)
    req(input$load.comp.tuned)
    if (!is.null(resultTuned())){
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotLoadings(resultTuned(), as.numeric(input$load.comp.tuned),
                     contrib = 'max', method = 'mean', legend.color = colors)
      } else {
        plotLoadings(resultTuned(), as.numeric(input$load.comp.tuned),
                     contrib = 'max', method = 'mean')    
      }
    }
  }
  
  table.selVar.tuned <- function(){
    req(input$sel.var.comp.tuned)
    if (!is.null(resultTuned())){
      selectVar(resultTuned(), as.numeric(input$sel.var.comp.tuned))
    }
  }
  
  #Sample plot
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
  
  # Selected Variables Tables
  output$Sel.Var <- DT::renderDataTable(
    table.selVar()
  )
  
  #tuned
  #Sample plot
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
  
  # Selected Variables Tables
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
  
  output$Indiv.download <- getDownloadHandler("PLS-DA_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS-DA_CorrelationCircleplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS-DA_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$SelVar.download <- getDownloadHandler("PLS-DA_SelectedFeatures.csv", table.selVar, type = "csv")
  
  output$Indiv.download.tuned <- getDownloadHandler("PLS-DA_reduced_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("PLS-DA_reduced_CorrelationCircleplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("PLS-DA_reduced_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$SelVar.download.tuned <- getDownloadHandler("PLS-DA_reduced_SelectedFeatures.csv", table.selVar.tuned, type = "csv")
  
  output$Filter.download <- downloadHandler(
    filename = function() {
      paste0(dataName(), "_plsda_reduced.xlsx")
    },
    content = function(file){
      openxlsx::write.xlsx(multiDataset$data[[paste0(dataName(), "_plsda_reduced")]]$omicsData, 
                           file, rowNames = TRUE, colNames = TRUE)
    }
  )
}

#' Information texts
#' @noRd
render_plsda_infotexts <- function(output){
  output$PLSDAinfotext <- renderText({
    HTML("The <b>P</b>artial <b>L</b>east-<b>S</b>quares <b>D</b>iscriminant <b>A</b>nalysis is a tool used for multivariate dimension reduction of large datasets. 
      It is similar to the PCA, but with a supervised approach for reducing the input data dimension. This means that the PLS-DA knows the label of each sample when reducing the dimension. 
      The PLS-DA works with the matrix containing the sample and features information and the dataset with the classes of each sample. 
      In the context of multi-omics analyses, it is used to get a first impression of the input data and find the key features of the datasets.<br/>
      Additional information can be found on the <a class='mixOmics-link' href='http://mixomics.org/methods/spls-da/' target='_blank'>mixOmics website</a> and
      in several scientific papers (e.g. <a class='ref-link' href='https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-3310-7' target='_blank'>Ruiz-Perez et.al. (2020)</a>).
      More information about the plots and the reducing and tuning methods can be found on our <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-plots').click();\">'Plots-Helppage'</a> and
      <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.</br>
      <b>Please adjust the number of components in the 'Analysis parameters' tab according to your selected dataset.</b>")
  })
  
  output$PLSDAreducedinfotext <- renderText({
    HTML("The dataset feature selection algorithm calculates the optimal number of components and the optimal number of features per component.<br/>
         According to this information the dataset was reduced and the reduced dataset is used for the plots. <br/>
         More detailed information can be found on our 
         <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.
         ")
  })
}