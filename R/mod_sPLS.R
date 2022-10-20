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
                               bs4Dash::box(title = "General information", width = 12,
                                            htmlOutput(ns("sPLSinfotext"))
                               )  
                      ),
                      fluidRow(width = 12,
                               getAnalysisParametersComponent(ns)
                      ),
                      fluidRow(width = 12,
                               splsGetUi(ns)
                      )
      ),
      bs4Dash::column(width = 2,
                      fluidRow(),
                      fluidRow(),
                      getTuneBox(ns)
      ),
      bs4Dash::column(id = ns("tunedCol"), width = 5,
                      fluidRow(style = "padding-left: 7.5px;",
                               h1("sPLS tuned"),
                      ),
                      fluidRow(
                               bs4Dash::box(title = "General information", width = 12,
                                            htmlOutput(ns("sPLStunedinfotext"))
                               )
                      ),
                      fluidRow(width = 12,
                               getTunedParametersComponent(ns, TRUE)
                      ),
                      fluidRow(width = 12,
                               splsGetUi(ns, ".tuned")
                      )
      )
    )
  )
}

#' PLS Server Functions
mod_sPLS_server <- function(id, data, classes){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::hide("tunedCol")
    shinyjs::hide("switchRow")
    
    dataSelection <- reactiveValues()
    classSelection <- reactiveValues()
    useTunedVals <- reactiveVal(FALSE)
    tunedVals <- reactiveValues(ncomp = 2, keepX = NULL)
    
    results <- run_spls_analysis(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals)
    
    render_spls_ui_components(ns, input, output, tunedVals)
    
    observe_spls_ui_components(ns, input, output, data, dataSelection, classes, classSelection, results$result, useTunedVals, tunedVals)
    
    generate_spls_plots(ns, input, output, dataSelection, classSelection, results$result, results$resultTuned, tunedVals)
    
    generate_spls_error_messages(input, output, data, classes, dataSelection, classSelection, tunedVals)
    
    render_spls_infotexts(output)
  })
}

#' Render Ui functions
render_spls_ui_components <- function(ns, input, output, tunedVals){
  renderIndivComps(ns, input, output, TRUE, tunedVals)
  
  renderVarComps(ns, input, output, TRUE, tunedVals)
  
  renderLoadComp(ns, input, output, TRUE, tunedVals)
  
  renderSelVarComp(ns, input, output, TRUE, tunedVals)
  
  renderImgComp(ns, input, output, TRUE, tunedVals)
}

#'Observe different ui components
observe_spls_ui_components <- function(ns, input, output, data, dataSelection, classes, classSelection, result, useTunedVals, tunedVals){
  
  #' Observe data  
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
        dataSelection$data1 <- data$data[[choice]]$filtered
      }
      
      getSelectionComponent(ns("dataSelection1"), "Select first dataset:", choices = choices, width = "150")
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
        dataSelection$data2 <- data$data[[choice]]$filtered
      }
      
      getSelectionComponent(ns("dataSelection2"), "Select second dataset:", choices = choices, width = "fit-content")
    })
  })
  
  #' Observe classes
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

  #' Observe change of data selection
  observeEvent(input$dataSelection1, {
    dataSelection$data1 <- data$data[[input$dataSelection1]]$filtered
  })
  
  observeEvent(input$dataSelection2, {
    dataSelection$data2 <- data$data[[input$dataSelection2]]$filtered
  })
  
  #' Observe change of class selection
  observeEvent(input$classSelection, {
    classSelection$data <- classes$data[[input$classSelection]]
  })
  
  #' Observe change of data
  observeDataset <- reactive({
    list(dataSelection$data1, dataSelection$data2)
  })
  
  observeEvent(observeDataset(), {
    output$tune.switch <- renderUI({})
    useTunedVals(FALSE)
    shinyjs::hide("tunedCol")
    shinyjs::hide("switchRow")
  })
  
  #' Observe tune button
  observeEvent(input$tune, {
    tryCatch({
      tune_values(dataSelection, result, tunedVals)

      if (!is.null(tunedVals)){
        shinyjs::show("switchRow")
        output$tune.switch <- renderUI({materialSwitch(ns("tuneSwitch"), "Use tuned parameters", value = FALSE)})
      }
    }, error = function(cond){
      getErrorMessage(cond, trim = F)
    })
  })
  
  #' Observe tune switch
  observeEvent(input$tuneSwitch,{
    useTunedVals(input$tuneSwitch)
    if(input$tuneSwitch){
      shinyjs::show("tunedCol")
    } else {
      shinyjs::hide("tunedCol")
    }
  })
}

#' Tune the ncomp and keepX parameter for the given dataset
tune_values <- function(dataSelection, result, tunedVals){
  withProgress(message = 'Tuning parameters .... Please wait!', value = 1/4, {
    
    #tune ncomp
    set.seed(30)
    tune.spls <- mixOmics::perf(result(), validation = "Mfold", folds = 7, progressBar = TRUE, nrepeat = 50)
    ncomp <- tune.spls$measures$Q2.total$summary[which.max(tune.spls$measures$Q2.total$summary$mean), 2]
    
    incProgress(1/4)
    
    X <- dataSelection$data1
    Y <- dataSelection$data2
    
    #tune keepX
    set.seed(30)
    list_keepX <- getTestKeepX(ncol(X))
    BPPARAM <- BiocParallel::SnowParam(workers = parallel::detectCores()-1)
    tune.X <- mixOmics::tune.spls(X, Y, ncomp = ncomp,
                                  validation = "Mfold",
                                  test.keepX = list_keepX, 
                                  measure = "cor", BPPARAM = BPPARAM,
                                  folds = 5, nrepeat = 50, progressBar = TRUE)
    keepX <- tune.X$choice.keepX
    
    incProgress(1/4)
    
    #tune keepY
    set.seed(30)
    list_keepY <- getTestKeepX(ncol(Y))
    tune.Y <- mixOmics::tune.spls(X, Y, ncomp = ncomp,
                                  validation = "Mfold",
                                  test.keepY = list_keepY, 
                                  measure = "cor", BPPARAM = BPPARAM,
                                  folds = 5, nrepeat = 50, progressBar = TRUE)
    keepY <- tune.Y$choice.keepY
    
    incProgress(1/4)
  })
  
  tunedVals$ncomp = ncomp
  tunedVals$keepX = keepX
  tunedVals$keepY = keepY
}

#' Run analysis
run_spls_analysis <- function(ns, input, output, dataSelection, classSelection, useTunedVals, tunedVals){
  spls.result <- reactive({
    req(dataSelection$data1)
    req(dataSelection$data2)
    req(nrow(classSelection$data) == nrow(dataSelection$data1) && nrow(classSelection$data) == nrow(dataSelection$data2))
    req(identical(classSelection$data[,1], rownames(dataSelection$data1)) && 
          identical(classSelection$data[,1], rownames(dataSelection$data2)))
    X <- dataSelection$data1
    Y <- dataSelection$data2
    
    msg <- checkDataNcompCompatibility(X, input$ncomp)
    if (msg == ""){
      msg <- checkDataNcompCompatibility(Y, input$ncomp)
    }
    output$parameters.error <- renderText(msg)
    
    if(msg == ""){
      tryCatch({
        spls.result <- mixOmics::spls(X, Y,
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
      req(dataSelection$data1)
      req(dataSelection$data2)
      req(nrow(classSelection$data) == nrow(dataSelection$data1) && nrow(classSelection$data) == nrow(dataSelection$data2))
      req(identical(classSelection$data[,1], rownames(dataSelection$data1)) && 
            identical(classSelection$data[,1], rownames(dataSelection$data2)))
      X <- dataSelection$data1
      Y <- dataSelection$data2
    
        tryCatch({
        spls.result.tuned <- mixOmics::spls(X, Y, ncomp = tunedVals$ncomp, 
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
generate_spls_error_messages <- function(input, output, data, classes, dataSelection, classSelection, tunedVals){
  
  # Error message when selection is incompatible or  data or classes are missing
  inputSelChange <- reactive({
    list(data$data, classes$data, dataSelection$data1, dataSelection$data2, classSelection$data)
  })
  
  observeEvent(inputSelChange(), {
    output$errorMsg <- renderText({
      if(length(data$data) == 0){
        "Please upload some data to be able to use the analysis!"
      } else if(length(classes$data) == 0){
        "Please upload some classes/label information to be able to use the analysis!"
      } else {
        class <- classSelection$data
        data1 <- dataSelection$data1
        data2 <- dataSelection$data2
        
        req(data1)
        req(data2)
        req(class)
        if ((length(data1) != 0 && nrow(class) != nrow(data1)) ||
                   (length(data2) != 0 && nrow(class) != nrow(data2))){
          "The selected data and classes are incompatible due to their different amount of samples! 
              Please change your selection!"
        } else if(!identical(class[,1], rownames(data1)) && 
                          !identical(class[,1], rownames(data2))){
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
generate_spls_plots <- function(ns, input, output, dataSelection, classSelection, result, resultTuned, tunedVals){
  #' Create reactive values
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
  
  
  #' generate output plots
  plot.indiv <- function(){
    req(classSelection$data)
    if(!is.null(result())){
      title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(result(), classSelection$data[,2], title, comp.indiv(), indNames = input$indiv.names, 
                  repSpace = rep.space(), legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(result(), classSelection$data[,2], title, comp.indiv(), indNames = input$indiv.names, 
                  repSpace = rep.space(), legendPosition = "bottom")    
      }
    }
  }
  
  plot.var <- function(){
    if(!is.null(result())){
      plotVar(result(), comp.var(), input$var.names,
            pch = c(1,2), legend = TRUE)
    }
  }
  
  plot.load <- function(){
    req(input$load.comp)
    if(!is.null(result())){
      plotLoadings(result(), as.numeric(input$load.comp))
    }
  }
  
  plot.img <- function(){
    if(!is.null(result())){
      comp.img <- checkCompNcompCombination(result()$ncomp, comp.img())
      mixOmics::cim(result(), comp = comp.img, margin=c(8,10))
    }
  }
  
  plot.arrow <- function(){
    if(!is.null(result())){
      if(splsGetNcomp(input) >= 2){
        req(classSelection$data)
        title = colnames(classSelection$data)[2]
        if (ncol(classSelection$data) == 3){
          colors = getGroupColors(classSelection$data)
          plotArrow(result(), classSelection$data[,2], title, input$namesArrow, col.per.group = colors)
        } else {
          plotArrow(result(), classSelection$data[,2], title, input$namesArrow)
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
      title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotIndiv(resultTuned(), classSelection$data[,2], title, comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
                  repSpace = rep.space.tuned(), legendPosition = "bottom", col.per.group = colors)
      } else {
        plotIndiv(resultTuned(), classSelection$data[,2], title, comp.indiv.tuned(), indNames = input$indiv.names.tuned, 
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
      plotLoadings(resultTuned(), as.numeric(input$load.comp.tuned))
    }
  }
  
  plot.img.tuned <- function(){
    if (!is.null(resultTuned())){
      mixOmics::cim(resultTuned(), comp = comp.img.tuned(), margin=c(8,10))
    }
  }
  
  plot.arrow.tuned <- function(){
    if(!is.null(resultTuned()) & splsGetNcomp(input, tuned = TRUE, tunedVals) >= 2){
      req(classSelection$data)
      title = colnames(classSelection$data)[2]
      if (ncol(classSelection$data) == 3){
        colors = getGroupColors(classSelection$data)
        plotArrow(resultTuned(), classSelection$data[,2], title, input$namesArrow.tuned, col.per.group = colors)
      } else {
        plotArrow(resultTuned(), classSelection$data[,2], title, input$namesArrow.tuned)
      }
    }
  }
  
  selVarTable.tuned <- reactive({
    if (!is.null(resultTuned())){
      req(input$sel.var.comp.tuned)
      req(classSelection$data)
      title = colnames(classSelection$data)[2]
      selectVar(resultTuned(), as.numeric(input$sel.var.comp.tuned), XY = TRUE)
    }
  })
  
  table.selVarX.tuned <- function(){
    selVarTable.tuned()$X
  }
  
  table.selVarY.tuned <- function(){
    selVarTable.tuned()$Y
  }
  
  #' Sample plot
  output$Indiv <- renderPlot(
    plot.indiv()
  )
  
  #' Correlation Circle plot
  output$Var <- renderPlot(
    plot.var()
  )
  
  #' Loading plot
  output$Load <- renderPlot(
    plot.load()
  )
  
  #' Selected Variables Table
  output$X.Sel.Var <- DT::renderDataTable(
    table.selVarX()
  )
  output$Y.Sel.Var <- DT::renderDataTable(
    table.selVarY()
  )
  
  #' CIM plot
  output$Img <- renderPlot(
    plot.img()
  )
  
  #' Arrow plot
  output$Arrow <- renderPlot(
    plot.arrow()
  )
  
  #tuned
  #' Sample plot
  output$Indiv.tuned <- renderPlot(
    plot.indiv.tuned()
  )
  
  #' Correlation Circle plot
  output$Var.tuned <- renderPlot(
    plot.var.tuned()
  )
  
  #' Loading plot
  output$Load.tuned <- renderPlot(
    plot.load.tuned()
  )
  
  #' Selected variables table
  output$X.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarX.tuned()
  )
  output$Y.Sel.Var.tuned <- DT::renderDataTable(
    table.selVarY.tuned()
  )
  
  #' CIM plot
  output$Img.tuned <- renderPlot(
    plot.img.tuned()
  )
  
  #' Arrow plot
  output$Arrow.tuned <- renderPlot(
    plot.arrow.tuned()
  )
  
  output$ncomp.tuned <- renderText(
    paste("Number of components: ", tunedVals$ncomp)
  )
  
  output$keepX.tuned <- renderText(
    paste("Variables of dataset 1: ",  paste(tunedVals$keepX, collapse = ", "))
  )
  
  output$keepY.tuned <- renderText(
    paste("Variables of dataset 2: ",  paste(tunedVals$keepY, collapse = ", "))
  )
  
  #' Download handler
  output$Indiv.download <- getDownloadHandler("PLS_Sampleplot.png", plot.indiv)
  output$Var.download <- getDownloadHandler("PLS_Variableplot.png", plot.var)
  output$Load.download <- getDownloadHandler("PLS_Loadingsplot.png", plot.load, width = 2592, height = 1944)
  output$Img.download <- getDownloadHandler("PLS_Heatmap.png", plot.img, width = 2592, height = 1944)
  output$SelVarX.download <- getDownloadHandler("PLS_SelectedVariable1.csv", table.selVarX, type = "csv")
  output$SelVarY.download <- getDownloadHandler("PLS_SelectedVariables2.csv", table.selVarY, type = "csv")
  
  output$Indiv.download.tuned <- getDownloadHandler("PLS_tuned_Sampleplot.png", plot.indiv.tuned)
  output$Var.download.tuned <- getDownloadHandler("PLS_tuned_Variableplot.png", plot.var.tuned)
  output$Load.download.tuned <- getDownloadHandler("PLS_tuned_Loadingsplot.png", plot.load.tuned, width = 2592, height = 1944)
  output$Img.download.tuned <- getDownloadHandler("PLS_tuned_Heatmap.png", plot.img.tuned, width = 2592, height = 1944)
  output$SelVarX.download.tuned <- getDownloadHandler("PLS_tuned_SelectedVariable1.csv", table.selVarX.tuned, type = "csv")
  output$SelVarY.download.tuned <- getDownloadHandler("PLS_tuned_SelectedVariables2.csv", table.selVarY.tuned, type = "csv")
}

#' Information texts
render_spls_infotexts <- function(output){
  output$sPLSinfotext <- renderText({
   HTML("The standard PLS is a multivariate analysis used to analyse two datasets by maximising the covariance between the datasets components.
   The <b>s</b>parse <b>P</b>rojection to <b>L</b>atent <b>S</b>tructure is a variant of the PLS, 
   which combines the correlation calculation and final variable selection into one step instead of two like the standard PLS has. 
   It is an unsupervised analysis method like the PCA, so it only needs the two omics datasets to perform the analysis. <br/>
   Additional information can be found on the <a class='mixOmics-link' href='https://mixomicsteam.github.io/Bookdown/pls.html' target='_blank'>mixOmics website</a>
   and in several scientific papers.") 
  })
  
  output$sPLStunedinfotext <- renderText({
    HTML("The mixOmics package provides the option to tune the parameters of the sPLS analysis. 
    This means that the optimal number of components and the optimal number of features to select on each component for every dataset will be calculated. <br/> 
    More detailed information can be found on the <a class='mixOmics-link' href='https://mixomicsteam.github.io/Bookdown/pls.html#tuning:PLS' target='_blank'>mixOmics website</a>.")
  })
}