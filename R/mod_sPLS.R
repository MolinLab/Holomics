#' PLS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sPLS_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(spin = "circle", position = "bottom-right", height = "60px", width = "60px"),
    fluidRow(
      bs4Dash::column(width = 12,
                      uiOutput(ns("dataSelComp1")),
                      uiOutput(ns("dataSelComp2")),
                      uiOutput(ns("classSelComp")),
                      textOutput(ns("errorMsg")),
                      style = "display: flex; column-gap: 1rem"
      )
    ),
    fluidRow(
      bs4Dash::column(width = 5, 
                      fluidRow(style = "padding-left: 7.5px;",
                               h1("sPLS"),
                      ),
                      fluidRow(
                               bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                            htmlOutput(ns("sPLSinfotext"))
                               )  
                      ),
                      fluidRow(width = 12,
                               getAnalysisParametersComponent(ns,
                                                              bs4Dash::column(width = 5,
                                                                              fluidRow(
                                                                                tags$label("PLS mode"),
                                                                                getTooltip(ns("mode-info"), 
                                                                                           "Mode to be used in the PLS algorithmn.
                                                                                           More information in the vignette.")
                                                                              ),
                                                                              fluidRow(
                                                                                selectizeInput(ns("mode"), label = "", 
                                                                                               choices = c("canonical"= "canonical", 
                                                                                                           "regression"= "regression"),
                                                                                               width = "60%")
                                                                              )
                                                              )
                               )
                      ),
                      fluidRow(width = 12,
                               bs4Dash::tabBox(width = 12, collapsible = FALSE,
                                               getsPLSSamplePlot(ns),
                                               getVariablePlot(ns),
                                               getLoadingsPlot(ns),
                                               getsPLSSelectedVarsPlot(ns),
                                               getCimPlot(ns),
                                               getArrowPlot(ns)
                               )
                      )
      ),
      bs4Dash::column(width = 2,
                      getTuneBox(ns, "Tune parameters", "Automatically calculate the optimal number of components 
                                     and the number of features per component.")
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(style = "padding-left: 7.5px;",
                               h1("sPLS tuned"),
                      ),
                      fluidRow(
                               bs4Dash::box(title = "General information", width = 12, collapsed = TRUE,
                                            htmlOutput(ns("sPLStunedinfotext"))
                               )
                      ),
                      fluidRow(width = 12,
                               getTunedParametersComponent(ns, TRUE)
                      ),
                      fluidRow(width = 12,
                               bs4Dash::tabBox(width = 12, collapsible = FALSE,
                                 getsPLSTuningPlots(ns),
                                 getsPLSSamplePlot(ns, ".tuned"),
                                 getVariablePlot(ns, ".tuned"),
                                 getLoadingsPlot(ns, ".tuned"),
                                 getsPLSSelectedVarsPlot(ns, ".tuned"),
                                 getCimPlot(ns, ".tuned"),
                                 getArrowPlot(ns, ".tuned")
                               )
                      )
      )
    )
  )
}

#' PLS Server Functions
#' @noRd
mod_sPLS_server <- function(id, data, classes){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::hide("tunedCol")

    dataSelection <- reactiveValues()
    classSelection <- reactiveValues()
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL, keepY = NULL, scale = F)
    
    results <- run_spls_analysis(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals)
    
    render_spls_ui_components(ns, input, output, tunedVals)
    
    observe_spls_ui_components(ns, input, output, data, dataSelection, classes, classSelection, results$result, useTunedVals, tunedVals)
    
    generate_spls_plots(ns, input, output, dataSelection, classSelection, results$result, results$resultTuned, tunedVals)
    
    generate_spls_error_messages(input, output, data, classes, dataSelection, classSelection, tunedVals)
    
    render_spls_infotexts(output)
  })
}

#' Render Ui functions
#' @noRd
render_spls_ui_components <- function(ns, input, output, tunedVals){
  renderIndivComps(ns, input, output, TRUE, tunedVals)
  
  renderVarComps(ns, input, output, TRUE, tunedVals)
  
  renderLoadComp(ns, input, output, TRUE, tunedVals)
  
  renderSelVarComp(ns, input, output, TRUE, tunedVals)
  
  renderImgComp(ns, input, output, TRUE, tunedVals)
}

#' Observe different ui components
#' @noRd
observe_spls_ui_components <- function(ns, input, output, data, dataSelection, classes, classSelection, result, useTunedVals, tunedVals){
  
  # Observe data  
  observeEvent(data$data, {
    output$dataSelComp1 <- renderUI({
      choices <- generateChoices(data$data)
      choice = ""
      if (length(choices) != 0L) 
        choice <- choices[[1]]
      
      selection <- isolate(input$dataSelection1)
      
      #same choice name as before but the data is not necessarily the same!
      #observeEvent of input$dataSelection will not be triggered
      if(!is.null(selection) && selection == choice){
        dataSelection$dataX <- data$data[[choice]]$omicsData
        dataSelection$dataXName <- data$data[[choice]]$name
      }
      
      getSelectionComponent(ns("dataSelection1"), "Select dataset X:", choices = choices, width = "fit-content")
    })
    
    output$dataSelComp2 <- renderUI({
      choices <- generateChoices(data$data)
      choice = ""
      if (length(choices) != 0L) 
        choice <- choices[[1]]
      
      selection <- isolate(input$dataSelection2)
      
      #same choice name as before but the data is not necessarily the same!
      #observeEvent of input$dataSelection will not be triggered
      if(!is.null(selection) && selection == choice){
        dataSelection$dataY <- data$data[[choice]]$omicsData
        dataSelection$dataYName <- data$data[[choice]]$name
      }
      
      getSelectionComponent(ns("dataSelection2"), "Select dataset Y:", choices = choices, width = "fit-content")
    })
  })
  
  # Observe classes
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
      
      getSelectionComponent(ns("classSelection"), "Select classes/labels:", choices = choices, width = "fit-content")
    })
  })

  # Observe change of data selection
  observeEvent(input$dataSelection1, {
    dataSelection$dataX <- data$data[[input$dataSelection1]]$omicsData
    dataSelection$dataXName <- data$data[[input$dataSelection1]]$name
  })
  
  observeEvent(input$dataSelection2, {
    dataSelection$dataY <- data$data[[input$dataSelection2]]$omicsData
    dataSelection$dataYName <- data$data[[input$dataSelection2]]$name
  })
  
  # Observe change of class selection
  observeEvent(input$classSelection, {
    classSelection$data <- classes$data[[input$classSelection]]
  })
  
  # Observe change of data
  observeDataset <- reactive({
    list(dataSelection$dataX, dataSelection$dataY)
  })
  
  observeEvent(observeDataset(), {
    useTunedVals(FALSE)
    shinyjs::hide("tunedCol")
  })
  
  # Observe tune button
  observeEvent(input$tune, {
    tryCatch({
      tune_values(dataSelection, result, tunedVals, input, output)

      if (!is.null(tunedVals$ncomp)){
        shinyjs::show("tunedCol")
        useTunedVals(T)
      }
    }, error = function(cond){
      getErrorMessage(cond, trim = F)
    })
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
#' @noRd
tune_values <- function(dataSelection, result, tunedVals, input, output){
  error <- F
  withProgress(message = 'Tuning parameters .... Please wait!', value = 1/4, {
    
    X <- dataSelection$dataX
    Y <- dataSelection$dataY
    result <- result()
    
    #tune ncomp
    finished <- F
    while (!finished && !error){
      values <- tryCatch({
        perf.spls <- mixOmics::perf(result, validation = "Mfold", folds = getsPLSFolds(X), progressBar = TRUE, nrepeat = 50)
        finished = T
      }, error = function(cond){
        if (grepl("Error in Ypred", cond, fixed = T) || 
            grepl("X.test %*% a.cv: non-conformable arguments", cond, fixed = T)){
          #get all possible value combinations
          nearZeroX <- mixOmics::nearZeroVar(X, freqCut = 1, uniqueCut = 100)
          nearZeroY <- mixOmics::nearZeroVar(Y, freqCut = 1, uniqueCut = 100)
          
          return (list(nearZeroX = nearZeroX, nearZeroY = nearZeroY))
        } else {
          if (grepl("system is computationally singular", cond$message, fixed = T)){
            getShinyErrorAlert("An error appeared while trying to tune the dataset. <br> 
                        Please reduce your number of components and try again!", html = T)
          } else {
            getErrorMessage(cond)
          }
          return(NULL)
        }
      })
      
      error <- is.null(values)
      if (!finished && !error){
        #get all possible value combinations
        nearZeroX <- values$nearZeroX
        nearZeroY <- values$nearZeroY
        
        if (length(nearZeroX$Position) == 0 && length(nearZeroY$Position) == 0){
          getErrorMessage("Unfortunately it is not possible to tune the parameters")
          error <- T
        } else {          
          minValX <- ifelse(length(nearZeroX$Position) != 0, min(nearZeroX$Metrics$percentUnique), 100)
          minValY <- ifelse(length(nearZeroY$Position) != 0, min(nearZeroY$Metrics$percentUnique), 100)
          
          if (minValX < minValY){
            X <- X[, -mixOmics::nearZeroVar(X, freqCut = 1, uniqueCut = minValX)$Position]
          } else {
            Y <- Y[, -mixOmics::nearZeroVar(Y, freqCut = 1, uniqueCut = minValY)$Position]
          }
          
          tryCatch({
            result <- mixOmics::spls(X, Y, mode = input$mode,
                                     ncomp = input$ncomp, scale = input$scale)
          }, error = function(cond){
            getErrorMessage(cond)
            error <- T
          }) 
        }
      }
    }
    
    if (!error){
      if (max(perf.spls$measures$Q2.total$summary$mean) < 0.0975){
        ncomp <- perf.spls$measures$Q2.total$summary[which.max(perf.spls$measures$Q2.total$summary$mean), 2]
      } else {
        indices <- which(perf.spls$measures$Q2.total$summary$mean > 0.0975)
        if (length(indices) == 1){
          ncomp <- perf.spls$measures$Q2.total$summary[indices, 2]
        } else {
          ncomp <- perf.spls$measures$Q2.total$summary[indices[-1], 2]
        }
      }
      
      incProgress(1/4)
      
      #tune keepX
      list_keepX <- getTestKeepX(ncol(X))
      BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
      
      finished <- F
      tryCatch({
        tune.X <- mixOmics::tune.spls(X, Y, mode = input$mode,
                                      ncomp = ncomp, scale = input$scale,
                                      validation = "Mfold",
                                      test.keepX = list_keepX, 
                                      measure = "cor", BPPARAM = BPPARAM,
                                      folds = getsPLSFolds(X), nrepeat = 50, progressBar = TRUE)
        finished <- T
        
      }, error = function(cond){
        getErrorMessage(cond)
        error <- T
      })
        
      if (!error){
        keepX <- tune.X$choice.keepX
        incProgress(1/4)
        
        #tune keepY
        list_keepY <- getTestKeepX(ncol(Y))
        finished <- F
        tryCatch({
          tune.Y <- mixOmics::tune.spls(X, Y, mode = input$mode,
                                        ncomp = ncomp, scale = input$scale,
                                        validation = "Mfold",
                                        test.keepY = list_keepY, 
                                        measure = "cor", BPPARAM = BPPARAM,
                                        folds = getsPLSFolds(X), nrepeat = 50, progressBar = TRUE)
          finished <- T
          
        }, error = function(cond){
          getErrorMessage(cond)
          error <- T
        })
        
        if (!error){
          keepY <- tune.Y$choice.keepY
          incProgress(1/4)
        }
      }
    }
  })
  
  if(error){
    tunedVals$ncomp <- NULL
  } else {
    # Tuning plots
    
    q2plot <- plot(perf.spls, criterion = 'Q2.total')
    output$Tuned.ncomp <- renderPlot(q2plot)
    output$Tuned.ncomp.download <- getDownloadHandler("PLS_Q2_Components_Plot.png", type = "ggplot", plot = q2plot)
    
    if (length(list_keepX) > 1){
      tuneXplot <- plot(tune.X)
      output$Tuned.keepX <- renderPlot(tuneXplot)
      output$Tuned.keepX.download <- getDownloadHandler("PLS_keepX_Plot.png", type = "ggplot", plot = tuneXplot)
      output$Tuned.keepX.msg <- renderText("")
      
    } else {
      output$Tuned.keepX.msg <- renderText("No plot available, as the first dataset is already very small due to that no feature selection was performed!")
    }
    
    if (length(list_keepY) > 1){
      tuneYplot <- plot(tune.Y)
      output$Tuned.keepY <- renderPlot(tuneYplot)
      output$Tuned.keepY.download <- getDownloadHandler("PLS_keepY_Plot.png", type = "ggplot", plot = tuneYplot)
      output$Tuned.keepY.msg <- renderText("")
      
    } else{
      output$Tuned.keepY.msg <- renderText("No plot available, as the second dataset is already very small due to that no feature selection was performed!")
    }
    
    tunedVals$ncomp <- ncomp
    tunedVals$keepX <- keepX
    tunedVals$keepY <- keepY
    tunedVals$scale <- input$scale
    
    if(all(perf.spls$measures$Q2.total$summary$mean < 0)){
      getShinyWarningAlert(HTML("Unfortunately, the Q2 score for all the components is below 0, 
                                 which could be the result of a too low number of samples or that X does not explain Y 
                                (for more information see: 
                                <a class='mixOmics-link' href='https://mixomics-users.discourse.group/t/q2-total-negative-in-perf-pls/138/6' 
                                target='_blank'>Q2.total negative in perf.pls (L\u00EA Cao, 2020)</a>) </br>.
                                In general, please consider using the untuned version."), html = T
      )
    }
  }
}

#' Run analysis
#' @noRd
run_spls_analysis <- function(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals){
  spls.result <- reactive({
    req(dataSelection$dataX)
    req(dataSelection$dataY)
    req(nrow(classSelection$data) == nrow(dataSelection$dataX) && nrow(classSelection$data) == nrow(dataSelection$dataY))
    req(identical(classSelection$data[,1], rownames(dataSelection$dataX)) && 
          identical(classSelection$data[,1], rownames(dataSelection$dataY)))
    X <- dataSelection$dataX
    Y <- dataSelection$dataY
    
    msg <- checkDataNcompCompatibility(X, input$ncomp)
    if (msg == ""){
      msg <- checkDataNcompCompatibility(Y, input$ncomp)
    }
    output$parameters.error <- renderText(msg)
    
    if(msg == ""){
      tryCatch({
        spls.result <- mixOmics::spls(X, Y, mode = input$mode,
                                      ncomp = input$ncomp, scale = input$scale)
      }, error = function(cond){
        getErrorMessage(cond)
        spls.result <- NULL
      }) 
    } else {
      spls.result <- NULL
    }
  })
  
  spls.result.tuned <- reactive({
    if (useTunedVals()){
      req(dataSelection$dataX)
      req(dataSelection$dataY)
      req(nrow(classSelection$data) == nrow(dataSelection$dataX) && nrow(classSelection$data) == nrow(dataSelection$dataY))
      req(identical(classSelection$data[,1], rownames(dataSelection$dataX)) && 
            identical(classSelection$data[,1], rownames(dataSelection$dataY)))
      X <- dataSelection$dataX
      Y <- dataSelection$dataY
    
        tryCatch({
        spls.result.tuned <- mixOmics::spls(X, Y, mode = input$mode,
                                            ncomp = tunedVals$ncomp, scale = tunedVals$scale,
                                            keepX = tunedVals$keepX, keepY = tunedVals$keepY)
      }, error = function(cond){
        getErrorMessage(cond)
        spls.result.tuned <- NULL
      })
    }
  })
  
  return(list(result = spls.result, resultTuned = spls.result.tuned))
}

#' Generate the error messages
#' @noRd
generate_spls_error_messages <- function(input, output, data, classes, dataSelection, classSelection, tunedVals){
  
  # Error message when selection is incompatible or  data or classes are missing
  inputSelChange <- reactive({
    list(data$data, classes$data, dataSelection$dataX, dataSelection$dataY, classSelection$data)
  })
  
  observeEvent(inputSelChange(), {
    output$errorMsg <- renderText({
      if(length(data$data) == 0){
        "Please upload some data to be able to use the analysis!"
      } else if(length(classes$data) == 0){
        "Please upload some classes/label information to be able to use the analysis!"
      } else {
        class <- classSelection$data
        dataX <- dataSelection$dataX
        dataY <- dataSelection$dataY
        
        req(dataX)
        req(dataY)
        req(class)
        if ((length(dataX) != 0 && nrow(class) != nrow(dataX)) ||
                   (length(dataY) != 0 && nrow(class) != nrow(dataY))){
          "The selected data and classes are incompatible due to their different amount of samples! 
              Please change your selection!"
        } else if(!identical(class[,1], rownames(dataX)) && 
                          !identical(class[,1], rownames(dataY))){
          "The selected data and classes are incompatible as they do not contain the same sample(name)s! 
            Please change your selection!"
        } else {
          ""
        }
      }
    })
  })
  
  output$arrow.error <- renderText({
    return (splsCheckNcomp(input))
  })
  
  output$arrow.error.tuned <- renderText({
    return (splsCheckNcomp(input, tuned = TRUE, tunedVals))
  })
}

#' Business logic functions
#' @noRd
generate_spls_plots <- function(ns, input, output, dataSelection, classSelection, result, resultTuned, tunedVals){
  # Create reactive values
  comp.indiv <- getCompIndivReactive(input)
  comp.var <- getCompVarReactive(input)
  comp.img <- getCompImgReactive(input)
  
  rep.space <- reactive({
    #necessary, because it's not possible to set NULL as choice value
    if (identical(input$rep.space, "NULL")){
      NULL
    } else{
      input$rep.space
    } 
  })
  
  comp.indiv.tuned <- getCompIndivReactive(input, tuned = TRUE)
  comp.var.tuned <- getCompVarReactive(input, tuned = TRUE)
  comp.img.tuned <- getCompImgReactive(input, tuned = TRUE)
  
  rep.space.tuned <- reactive({
    #necessary, because it's not possible to set NULL as choice value
    if(identical(input$rep.space.tuned, "NULL")){
      NULL
    } else {
      input$rep.space.tuned
    }
  })
  
  
  # generate output plots
  plot.indiv <- function(){
    req(classSelection$data)
    if(!is.null(result())){
      legend.title = colnames(classSelection$data)[2]
      
      titles = getTitleAccordingToRepSpace(rep.space(), dataSelection$dataXName, dataSelection$dataYName)
      
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result(), classes = classSelection$data[,2], title = titles$title, legend.title = legend.title, 
                  subtitle = titles$subtitle, comp = comp.indiv(), indNames = input$indiv.names, 
                  repSpace = rep.space(), legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(result(), classes = classSelection$data[,2], title = titles$title, legend.title = legend.title, 
                  subtitle = titles$subtitle, comp = comp.indiv(), indNames = input$indiv.names, 
                  repSpace = rep.space(), legendPosition = "bottom")    
      }
    }
  }
  
  plot.var <- function(){
    if(!is.null(result())){
      names <- getDatasetNames(dataSelection$dataXName, dataSelection$dataYName)
      plotVar(result(), comp.var(), input$var.names,
            pch = c(1,2), legend = c(names$name1, names$name2))
    }
  }
  
  plot.load <- function(){
    req(input$load.comp)
    if(!is.null(result())){
      names <- getDatasetNames(dataSelection$dataXName, dataSelection$dataYName)
      plotLoadings(result(), as.numeric(input$load.comp), 
                   subtitle = lapply(c(names$name1, names$name2),
                                     function(x) paste('Loadings on comp', input$load.comp, "\nBlock", x,"'"))
      )
    }
  }
  
  plot.img <- function(){
    if(!is.null(result())){
      comp.img <- checkCompNcompCombination(result()$ncomp, comp.img())
      mixOmics::cim(result(), comp = comp.img, margin=c(input$xmargin, input$ymargin), 
                    xlab = dataSelection$dataYName, ylab = dataSelection$dataXName)
    }
  }
  
  plot.arrow <- function(){
    if(!is.null(result())){
      if(splsGetNcomp(input) >= 2){
        req(classSelection$data)
        legend.title = colnames(classSelection$data)[2]
        if (ncol(classSelection$data) == 3){
          colors = getGroupColors(classSelection$data)
          plotArrow(result(), classSelection$data[,2], legend.title, input$namesArrow, col.per.group = colors)
        } else {
          plotArrow(result(), classSelection$data[,2], legend.title, input$namesArrow)
        }
      }
    }
  }
  
  selVarTable <- reactive({
    req(input$sel.var.comp)
    if(!is.null(result())){
      selectVar(result(), as.numeric(input$sel.var.comp), XY = TRUE)
    }
  })
  
  table.selVarX <- function(){
    selVarTable()$X
  }
  
  table.selVarY <- function(){
    selVarTable()$Y
  }
  
  #tuned
  plot.indiv.tuned <- function(){
    if (!is.null(resultTuned())){
      legend.title = colnames(classSelection$data)[2]
      titles = getTitleAccordingToRepSpace(rep.space.tuned(), dataSelection$dataXName, dataSelection$dataYName)
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(resultTuned(), classes = classSelection$data[,2], title = titles$title, legend.title = legend.title, 
                  subtitle = titles$subtitle, comp = comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
                  repSpace = rep.space.tuned(), legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(resultTuned(), classes = classSelection$data[,2], title = titles$title, legend.title = legend.title, 
                  subtitle = titles$subtitle, comp = comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
                  repSpace = rep.space.tuned(), legendPosition = "bottom")    
      }
    }
  }
  
  plot.var.tuned <- function(){
    if (!is.null(resultTuned())){
      plotVar(resultTuned(), comp.var.tuned(), input$var.names.tuned,
              pch = c(1,2), legend = TRUE)
    }
  }
  
  plot.load.tuned <- function(){
    if (!is.null(resultTuned())){
      req(input$load.comp.tuned)
      names <- getDatasetNames(dataSelection$dataXName, dataSelection$dataYName)
      plotLoadings(resultTuned(), as.numeric(input$load.comp.tuned), 
                   subtitle = lapply(c(names$name1, names$name2),
                                     function(x) paste('Loadings on comp', input$load.comp, "\nBlock", x,"'"))
      )
    }
  }
  
  plot.img.tuned <- function(){
    if (!is.null(resultTuned())){
      mixOmics::cim(resultTuned(), comp = comp.img.tuned(), margin=c(input$xmargin.tuned, input$ymargin.tuned),
                    xlab = dataSelection$dataYName, ylab = dataSelection$dataXName)
    }
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(resultTuned()) & splsGetNcomp(input, tuned = TRUE, tunedVals) >= 2){
      req(classSelection$data)
      legend.title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotArrow(resultTuned(), classSelection$data[,2], legend.title, input$namesArrow.tuned, col.per.group = colors)
      } else {
        plotArrow(resultTuned(), classSelection$data[,2], legend.title, input$namesArrow.tuned)
      }
    }
  }
  
  selVarTable.tuned <- reactive({
    if (!is.null(resultTuned())){
      req(input$sel.var.comp.tuned)
      req(classSelection$data)
      legend.title = colnames(classSelection$data)[2]
      selectVar(resultTuned(), as.numeric(input$sel.var.comp.tuned), XY = TRUE)
    }
  })
  
  table.selVarX.tuned <- function(){
    selVarTable.tuned()$X
  }
  
  table.selVarY.tuned <- function(){
    selVarTable.tuned()$Y
  }
  
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
  
  # Selected Variables Table
  output$X.Sel.Var <- DT::renderDataTable(
    table.selVarX()
  )
  output$Y.Sel.Var <- DT::renderDataTable(
    table.selVarY()
  )
  
  # CIM plot
  output$Img <- renderPlot(
    plot.img()
  )
  
  # Arrow plot
  output$Arrow <- renderPlot(
    plot.arrow()
  )
  
  #tuned
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
  
  # Selected variables table
  output$X.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarX.tuned()
  )
  output$Y.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarY.tuned()
  )
  
  # CIM plot
  output$Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  # Arrow plot
  output$Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Features of dataset X: ",  paste(tunedVals$keepX, collapse = ", "))
  )
  
  output$keepY.tuned <- renderText(
    paste("Features of dataset Y: ",  paste(tunedVals$keepY, collapse = ", "))
  )
  
  output$scale.tuned <- renderText(
    paste("Scaled: ",  tunedVals$scale)
  )
  
  output$mode.tuned <- renderText(
    paste("Mode: ",  input$mode)
  )
  
  # Download handler
  output$Indiv.download <- getDownloadHandler("PLS_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS_CorrelationCircleplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$Img.download <- getDownloadHandler("PLS_Heatmap.png", plot.img, width = 2592, height = 1944)
  output$SelVarX.download <- getDownloadHandler("PLS_SelectedFeatures1.csv", table.selVarX, type = "csv")
  output$SelVarY.download <- getDownloadHandler("PLS_SelectedFeatures2.csv", table.selVarY, type = "csv")
  output$Arrow.download <- getDownloadHandler("PLS_Arrowplot.png", plot.arrow, type = "ggplot")
  output$Img.table.download <- getDownloadHandler("PLS_Heatmap.csv", contentfct = plot.img, type = "csv", tablefct = cimToTable)
  
  
  output$Indiv.download.tuned <- getDownloadHandler("PLS_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("PLS_tuned_CorrelationCircleplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("PLS_tuned_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$Img.download.tuned <- getDownloadHandler("PLS_tuned_Heatmap.png", plot.img.tuned, width = 2592, height = 1944)
  output$SelVarX.download.tuned <- getDownloadHandler("PLS_tuned_SelectedFeatures1.csv", table.selVarX.tuned, type = "csv")
  output$SelVarY.download.tuned <- getDownloadHandler("PLS_tuned_SelectedFeatures2.csv", table.selVarY.tuned, type = "csv")
  output$Arrow.download.tuned <- getDownloadHandler("PLS_tuned_Arrowplot.png", plot.arrow.tuned, type = "ggplot")
  output$Img.table.download.tuned <- getDownloadHandler("PLS_Heatmap_tuned.csv", contentfct = plot.img.tuned, type = "csv", tablefct = cimToTable)
}

#' Information texts
#' @noRd
render_spls_infotexts <- function(output){
  output$sPLSinfotext <- renderText({
   HTML("The standard PLS is a multivariate analysis used to analyse two datasets by maximising the covariance between the datasets components.
   The <b>s</b>parse <b>P</b>rojection to <b>L</b>atent <b>S</b>tructure is a variant of the PLS, 
   which combines the correlation calculation and final features selection into one step instead of two like the standard PLS has. 
   It is an unsupervised analysis method like the PCA, so it only needs the two omics datasets to perform the analysis.
   Additional information can be found on the <a class='mixOmics-link' href='http://mixomics.org/methods/spls/' target='_blank'>mixOmics website</a> and
   in several scientific papers (e.g. <a class='ref-link' href='https://www.degruyter.com/document/doi/10.2202/1544-6115.1390/html' target='_blank'>L\u00EA Cao et.al. (2008)</a>).
   More information about the plots and the reducing and tuning methods can be found on our <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-plots').click();\">'Plots-Helppage'</a> and
   <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.</br>
   <b>Please adjust the number of components in the 'Analysis parameters' tab according to your selected dataset.</b>  
   We recommend using here a rather high number of components and then perform parameter tuning.") 
  })
  
  output$sPLStunedinfotext <- renderText({
    HTML("The mixOmics package provides the option to tune the parameters of the sPLS analysis. 
    This means that the optimal number of components and the optimal number of features to select on each component for every dataset will be calculated. <br/> 
    More detailed information can be found on our <a class='mixOmics-link' onclick=\"document.getElementById('tab-help-tuning').click();\">'Feature selection and tuning-Helppage'</a>.")
  })
}