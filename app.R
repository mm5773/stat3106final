# Building Web Applications with Shiny Package


library(shiny)

library(shinythemes)

library(shinycssloaders)

library(tidymodels)

library(tidyverse)

library(ggExtra)

library(data.table)

library(recipes)

library(caret)

library(MASS)

#read in communities_data_final.txt
data_initial <- read.csv("~/Downloads/Spring 2024/Applied ML/stat3106final/communities_data_final.txt", header = TRUE)


# Define UI for application

ui <- fluidPage(
  
  titlePanel("My First Shiny Application"),
  
  navbarPage(
    
    title = ("STAT 3106"),
    
    theme = shinytheme("flatly"),
    
    tabPanel("Overview", icon = icon("info-circle"),
             
             titlePanel("Overview: User Instructions"),
             
             mainPanel(
               
               helpText("STAT 3106: Applied Machine Learning - Final Project ......"))
             
    ),
    
    tabPanel("Uploading and Transforming Data", icon = icon("folder-open"),
             
             titlePanel("Preprocessing and Splitting Data"),
             sidebarLayout(
               sidebarPanel(
                   selectInput("dataset", "Dataset:", choices = c("Communities & Crime", "Upload your own file")),
                   
                   conditionalPanel(condition = "input.dataset == 'Upload your own file'",
                                    fileInput("file", "Select your files:",
                                              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                   
                   actionButton("load_data_button", "Load Data"),
                   uiOutput("preprocessing_panel")
               ),
               mainPanel(
                 DT::DTOutput("data_preview"),
                 textOutput("preprocessing_output")
               )
             )
             
    ),
    tabPanel("Data Visualization",
             titlePanel("Data Visualization"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("plotType", "Choose Plot Type:",
                             choices = c("Scatterplot" = "scatter", "Histogram" = "hist")),
                 
                 #scatterplot
                 conditionalPanel(
                   condition = "input.plotType == 'scatter'",
                   selectInput("response", "Response Variable (Y)", choices = NULL),
                   selectInput("explanatory", "Explanatory Variable (X)", choices = NULL),
                   sliderInput("shade", "Transparency Rate", min = 0, max = 1, value = 0.5, step = 0.1),
                   checkboxInput("marginal", "Marginal Distributions", value = FALSE)
                 ),
                 
                 #histogram
                 conditionalPanel(
                   condition = "input.plotType == 'hist'",
                   selectInput("var", "Variable", choices = NULL),
                   numericInput("bins", "Number of bins", min = 1, max = 50, step = 1, value = 10),
                   radioButtons("color", "Color of bins:",
                                choices = list("Blue" = "blue", "Red" = "red", "Green" = "green"),
                                selected = "blue"),
                   actionButton("click", "Submit")
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plot",
                            plotOutput("plotOutput")),
                   
                   tabPanel("Numeric Summary",
                            DT::dataTableOutput("summaryOutput"),
                   )
                 )
               )
             )
    
    ), 
    tabPanel("Tuning Parameters and Model Optimization",
             titlePanel("SVM Hyperparameter Tuning"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("kernelType", "Kernel Type", choices = c("Linear" = "svmLinear", "Polynomial" = "svmPoly", "RBF" = "svmRadial")),
                 sliderInput("costRange", "Cost (C) Range",
                             min = 0.01, max = 1.0, 
                             value = c(0.01, 0.9),  # Default range selected
                             step = 0.1,           # Increment step size
                             ticks = TRUE,         # Shows tick marks
                             animate = TRUE),
                 conditionalPanel(
                   condition = "input.kernelType !== 'svmLinear'",
                   numericInput("gamma", "Gamma", value = 0.1, min = 0.01, max = 10, step = 0.01)
                 ),
                 actionButton("goButton", "Run Model")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plot",
                            plotOutput("plotPerformance")),
                   
                   tabPanel("Numeric Summary",
                            DT::dataTableOutput("modelResults"),
                   )
                 )
               )
             )
        )

             
    
    
    
  )
  
  
  
)

  
  server <- function(input, output, session) {
    preprocessing_done <- reactiveVal(FALSE)
    training <- reactiveVal()
    testing <- reactiveVal()
    dataset <- reactive({
      
      if(input$dataset == 'Upload your own file'){
        
        req(input$file)
        
        File <- input$file
        
        df <- data.frame(rbindlist(lapply(File$datapath, fread), use.names = TRUE, fill = TRUE))
        
        return(df)
        
      } else {
        
        return(data_initial)
      }
    })
    
    File <- dataset
    observeEvent(input$load_data_button, {
      preprocessing_done(FALSE)
      output$data_preview <- DT::renderDataTable({
        File()
      })
        
        output$preprocessing_panel <- renderUI({
          if (!is.null(dataset()) && input$dataset != "") {
            tagList(
              br(),
              numericInput("split_ratio", "Training-Test Split Ratio:", value = 0.75, min = 0.1, max = 0.9, step = 0.05),
              checkboxGroupInput("preprocessing_steps", "Select Preprocessing Steps:",
                                 choices = c("Convert to Factors", "Remove Near Zero Variance Predictors",
                                             "Impute Missing Values", "Center Numeric Predictors",
                                             "Scale Numeric Predictors", "Create Dummy Variables")),
              actionButton("preprocess_button", "Preprocess and Split Data")
            )
          }
        })
        
        observeEvent(input$preprocess_button, {
          preprocessing_done(TRUE)
          split_ratio <- input$split_ratio
          selected_steps <- input$preprocessing_steps
          print("removed rows with target variable")
      
          print(nrow(dataset()))
          data <- dataset()[complete.cases(dataset()$violentPerPop),]
          
          drop_mv_cols <- function(data, threshold){
            missing_cols <- colSums(is.na(data))
            cols_to_drop <- names(missing_cols[missing_cols >= threshold])
            data <- data[, !(names(data) %in% cols_to_drop)]
          }
          data <- drop_mv_cols(data, threshold=1872)
          data <- data[, !colnames(data) %in% c("communityname", "State", "countyCode", "communityCode")]
          print(nrow(data))
          
          shifted_variable <- data$violentPerPop - min(data$violentPerPop) + 1
          #print(class(shifted_variable))
          #print(shifted_variable)
          lm_model <- lm(shifted_variable ~ 1)
          boxcox_result <- boxcox(lm_model)
          lambda <- boxcox_result$x[which.max(boxcox_result$y)]
          print("reached here hi")
          data$violentPerPop <- if (lambda == 0) log(shifted_variable) else ((shifted_variable^lambda - 1) / lambda)
          
          #split data into training and test
          set.seed(1)
          indices <- sample(1:nrow(data), size = round(nrow(data) * split_ratio))
          train_data <- data[indices, ]
          test_data <- data[-indices, ]
          
          constant_columns <- sapply(train_data, function(x) length(unique(x)) == 1)
          train_data <- train_data[, !constant_columns]
          
          #preprocessing steps 
          if (!is.null(data)) {
            blueprint <- recipe(violentPerPop ~ ., data = train_data)
            
            if ("Convert to Factors" %in% selected_steps)
              blueprint <- blueprint %>% step_string2factor(all_nominal_predictors())
            
            if ("Remove Near Zero Variance Predictors" %in% selected_steps)
              blueprint <- blueprint %>% step_nzv(all_predictors())
            
            if ("Impute Missing Values" %in% selected_steps)
              blueprint <- blueprint %>% step_impute_knn(all_predictors())
            
            if ("Center Numeric Predictors" %in% selected_steps)
              blueprint <- blueprint %>% step_center(all_numeric_predictors())
            
            if ("Scale Numeric Predictors" %in% selected_steps)
              blueprint <- blueprint %>% step_scale(all_numeric_predictors())
            
            if ("Create Dummy Variables" %in% selected_steps)
              blueprint <- blueprint %>% step_dummy(all_nominal_predictors())
            
            blueprint <- blueprint %>% prep()
            prep_data <- prep(blueprint)
            
            train_data <- bake(blueprint, new_data = train_data)
            test_data <- bake(blueprint, new_data = test_data)
            
            #print(sapply(train_data, function(x) sum(is.na(x))))
            training(train_data)
            testing(test_data)
            
            output$preprocessing_output <- renderText({
              paste("Data preprocessed and split into training and test sets.\n",
                    "Training set size:", nrow(train_data),
                    "\nTest set size:", nrow(test_data))
            })
            
          }
        })
        output$data_preview <- DT::renderDataTable({
          if (!preprocessing_done()) {
            File()
          }
        })
        
    })
  
  ##
  
  observeEvent(File(), {
    
    updateSelectInput(session, "response",
                      
                      choices = names(File()))
  })
  
  
  
  observeEvent(File(), {
    
    updateSelectInput(session, "explanatory",
                      
                      choices = names(File()))
  }) 
  
  
  observeEvent(File(), {
    
    updateSelectInput(session, "var",
                      
                      choices = names(File()))
  })
  
  ##
  
  output$plotOutput <- renderPlot({
    if (input$plotType == "scatter") {
      p <- ggplot(data = File(), aes_string(x = input$explanatory, y = input$response)) +
        geom_point(alpha = input$shade) +
        theme_minimal()
   
      if (input$marginal) {
        p <- ggMarginal(p, type = "histogram")
      }
      
      return(p)
      
    } else if (input$plotType == "hist") {
      req(input$click)  
      ggplot(data = File(), aes_string(x = input$var)) +
        geom_histogram(binwidth = diff(range(File()[[input$var]]) / input$bins), fill = input$color, color = "black") +
        labs(x = input$var, y = "Frequency", title = "Histogram") +
        theme_minimal()
    }
  })
  
  output$summaryOutput <- DT::renderDataTable({
    if (input$plotType == "scatter") {
      summary_data <- summary(File()[[input$response]])
      return(data.frame(Measure = names(summary_data), Value = as.character(summary_data)))
    } else if (input$plotType == "hist") {
      summary_data <- summary(File()[[input$var]])
      return(data.frame(Measure = names(summary_data), Value = as.character(summary_data)))
    }
  })
  
  modelResults <- eventReactive(input$goButton, {
    # Assuming dataset is loaded and preprocessed already (adjust as needed)
    print("processing...")
    train_data <- training()
    cost_values <- c() 
    if (input$costRange[1] == 0.01){
      start_value = 0.1 
      cost_values <- c(0.01)
    }else{
      start_value = input$costRange[1]
    }
    values <- seq(from = start_value, to = input$costRange[2], by = 0.2)
    cost_values <- c(cost_values, values)
    print(cost_values)
    tuneGrid <- expand.grid(C = cost_values)
    #tuneGrid <- expand.grid(C = input$cost,
                            #Gamma = ifelse(input$kernelType != "svmLinear", input$gamma, NA))
    
    trainControl <- trainControl(method = "cv", number = 5, search = "grid", summaryFunction = defaultSummary)
    print("reached here")
    print(nrow(train_data))
    #train <- na.omit(train)
    #print(nrow(train))
    # Train the model
    #print(sapply(train_data, function(x) sum(is.na(x))))
    svmModel <- train(violentPerPop ~ ., data = train_data, method = input$kernelType,
                      trControl = trainControl, tuneGrid = tuneGrid, metric="RMSE")
    
    print(svmModel)
    return(svmModel)
  })
  
  
  output$plotPerformance <- renderPlot({
    req(modelResults())
    # Plotting the results
    if ("results" %in% names(modelResults())) {
      ggplot(modelResults()$results, aes(x = C, y = RMSE)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title = "Model Performance", x = "Cost (C)", y = "RMSE")
    } else {
      print("No results available for plotting")
    }
  })
  
  output$modelResults <- DT::renderDataTable({
    req(modelResults())
    print(modelResults())
    if ("results" %in% names(modelResults())) {
      DT::datatable(modelResults()$results, options = list(searching = FALSE, paging = FALSE))
    } else {
      print("No results data available")
    }
  })
  
  train_svm_model <- eventReactive(input$train_model, {
    resample_final <- trainControl(method = "none") 
    svm_model_final <- train(violentPerPop ~ ., data = train_data, trControl = resample_final,
                             tuneGrid = data.frame(C=input$C), method = input$method, metric="RMSE")
    
    return(svm_model_final)
  })
  
  output$svmFinalModelResults <- DT::renderDataTable({
    req(train_svm_model())
    svm_model_final <- train_model()
    svm_pred_train <- predict(svm_model_final, newdata = train_data)
    svm_pred_test <- predict(svm_model_final, newdata = test_data)
    
    results_train <- postResample(pred = svm_pred_train, obs = train_data$violentPerPop)
    results_test <- postResample(pred = svm_pred_test, obs = test_data$violentPerPop)
    print(results_train)
    print(results_test)
    
    rmse_train <- results_train[1]
    rmse_test <- results_test[1]
    return(list(results_train=results_train, results_test=results_test))
    
  })
  



  
}





# Run the application 

shinyApp(ui = ui, server = server)
