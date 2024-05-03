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
                 selectInput("dataset_type", "Dataset:", choices=c("Original", "Training", "Testing")),
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
                   numericInput("bins", "Number of bins", min = 1, max = 50, step = 1, value = 10)
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plot",
                            plotOutput("plotOutput")),
                   
                   tabPanel("Numeric Summary",
                            DT::dataTableOutput("summaryOutput")
                   )
                 )
               )
             )
    
    ), 
    tabPanel("Tuning Parameters and Model Optimization",
             titlePanel("Hyperparameter Tuning"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("modelType", "Select Model Type", choices = c("Random Forest" = "rf", "SVM" = "svm", "XGBoost" = "xgboost")),
                 conditionalPanel(
                   condition = "input.modelType == 'svm'",
                   selectInput("kernelType", "Kernel Type", choices = c("Linear" = "svmLinear", "Polynomial" = "svmPoly", "RBF" = "svmRadial")),
                   sliderInput("costRange", "Cost (C) Range",
                               min = 0.01, max = 1.0, 
                               value = c(0.01, 0.9),  # Default range selected
                               step = 0.1,           # Increment step size
                               ticks = TRUE,         # Shows tick marks
                               animate = TRUE),
                   conditionalPanel(
                     condition = "input.kernelType == 'svmPoly'",
                     sliderInput("degree", "degree",
                                 min = 1, max = 4, 
                                 value = c(1, 4),  # Default range selected
                                 step = 1,           # Increment step size
                                 ticks = TRUE,         # Shows tick marks
                                 animate = TRUE)
                   ),
                   conditionalPanel(
                     condition = "input.kernelType == 'svmRadial'",
                     sliderInput("sigma", "sigma",
                                 min = 0.01, max = 1.0, 
                                 value = c(0.01, 0.9),  # Default range selected
                                 step = 0.1,           # Increment step size
                                 ticks = TRUE,         # Shows tick marks
                                 animate = TRUE)
                   ),
                   actionButton("goButton", "Run Model")
                 ),
                 conditionalPanel(
                   condition = "input.modelType == 'rf'"
                 ),
                 conditionalPanel(
                   condition = "input.modelType == 'xgboost'"
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plot",
                            plotOutput("plotPerformance")),
                   
                   tabPanel("Numeric Summary",
                            DT::dataTableOutput("modelResults")
                   )
                 )
               )
             )
        ),
    tabPanel("Developing Final Model",
             titlePanel("Creating Final Model"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("kernelType", "Kernel Type", choices = c("Linear" = "svmLinear", "Polynomial" = "svmPoly", "RBF" = "svmRadial")),
                 numericInput("C", "C", value = 0.1, min = 0.01, max = 1),
                 conditionalPanel(
                   condition = "input.kernelType == 'svmRadial'",
                   numericInput("sigma_final", "sigma", value = 0.1, min = 0.01, max = 1)
                 ),
                 conditionalPanel(
                   condition = "input.kernelType == 'svmPoly'",
                   numericInput("degree_final", "degree", value = 2, min = 1, max = 4)
                 ),
                 actionButton("train_model", "Train Model")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Results",
                            DT::dataTableOutput("svmFinalModelResults")
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
    response_variable_name <- reactiveVal()
    response_variable <- reactiveVal()
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
        dataset()
      })
        
        output$preprocessing_panel <- renderUI({
          if (!is.null(dataset()) && input$dataset != "") {
            if (input$dataset == "Communities & Crime" || input$dataset != "Upload your own file") {
              choices <- c("violentPerPop", "murdPerPop", "rapesPerPop", 
                           "robbPerPop", "assaultPerPop", "burglPerPop", 
                           "larcPerPop", "autoTheftPerPop", "arsonsPerPop")
            } else {
              choices <- names(dataset()[sapply(dataset(), is.numeric)])
            }
            tagList(
              br(),
              selectInput("response_var", "Select Response Variable (numeric):", choices = choices),
              numericInput("split_ratio", "Training-Test Split Ratio:", value = 0.75, min = 0.1, max = 0.9, step = 0.05),
              checkboxGroupInput("preprocessing_steps", "Select Preprocessing Steps:",
                                 choices = c("Box-Cox Transform Response Variable", "Convert to Factors", "Remove Near Zero Variance Predictors",
                                             "Impute Missing Values", "Center Numeric Predictors",
                                             "Scale Numeric Predictors", "Create Dummy Variables")),
              actionButton("preprocess_button", "Preprocess and Split Data")
            )
          }
        })
        
        observeEvent(input$preprocess_button, {
          preprocessing_done(TRUE)
          cat("response variable: ", input$response_var)
          formula_text <- paste(input$response_var, "~ .")
          response_variable_temp <- as.formula(formula_text)
          response_variable(as.formula(formula_text))
          #print(response_variable)
          response_variable_name(all.vars(response_variable_temp)[1])
          print(response_variable_name)
          
          split_ratio <- input$split_ratio
          selected_steps <- input$preprocessing_steps
          print("removed rows with target variable")
      
          print(nrow(dataset()))
          data <- dataset()[complete.cases(dataset()[[response_variable_name()]]),]
          
          drop_mv_cols <- function(data, threshold){
            missing_cols <- colSums(is.na(data))
            cols_to_drop <- names(missing_cols[missing_cols >= threshold])
            data <- data[, !(names(data) %in% cols_to_drop)]
          }
          
          data <- drop_mv_cols(data, threshold=1872)
          data <- data[, !colnames(data) %in% c("communityname", "State", "countyCode", "communityCode")]
          print(nrow(data))
          
          
          if ("Box-Cox Transform Response Variable" %in% selected_steps){
            shifted_variable <- data[[response_variable_name()]] - min(data[[response_variable_name()]]) + 1
            #print(class(shifted_variable))
            #print(shifted_variable)
            lm_model <- lm(shifted_variable ~ 1)
            boxcox_result <- boxcox(lm_model)
            lambda <- boxcox_result$x[which.max(boxcox_result$y)]
            data[[response_variable_name()]] <- if (lambda == 0) log(shifted_variable) else ((shifted_variable^lambda - 1) / lambda)
          }
          
          #split data into training and test
          set.seed(1)
          indices <- sample(1:nrow(data), size = round(nrow(data) * split_ratio))
          train_data <- data[indices, ]
          test_data <- data[-indices, ]
          
          constant_columns <- sapply(train_data, function(x) length(unique(x)) == 1)
          train_data <- train_data[, !constant_columns]
          
          #preprocessing steps 
          if (!is.null(data)) {
            blueprint <- recipe(response_variable(), data = train_data)
            
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
              HTML(paste("Data preprocessed and split into training and test sets.", 
                    "Training set size:", nrow(train_data),
                    "Test set size:", nrow(test_data)))
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
    observeEvent(dataset(), {
      print("goes here")
      if (input$dataset == "Communities & Crime" || input$dataset != "Upload your own file"){
        print("here")
        columns_to_exclude <- c("communityname", "State", "countyCode", "communityCode")
        available_choices <- setdiff(names(File()), columns_to_exclude)
        
        updateSelectInput(session, "response", choices = available_choices, selected="violentPerPop")
        updateSelectInput(session, "explanatory", choices = available_choices, selected="pctBlack")
        updateSelectInput(session, "var", choices = available_choices, selected = "violentPerPop")
      }else{
        updateSelectInput(session, "explanatory", choices = names(File()))
        updateSelectInput(session, "response", choices = names(File()))
        updateSelectInput(session, "var", choices = names(File()))
      }
    })

  
  ##
  output$plotOutput <- renderPlot({
    if (input$plotType == "scatter") {
      print(input$dataset)
      data_for_plot <- dataset()
      if (input$dataset_type == "Original"){
        data_for_plot <- dataset()
      }else if (input$dataset_type == "Training"){
        cat("training", input$dataset)
        data_for_plot <- req(training())
        data_for_plot <- training()
      }else if (input$dataset_type == "Testing"){
        cat("testing", input$dataset)
        data_for_plot <- req(testing())
        data_for_plot <- testing()
      }
      
      print("structure of data", str(data_for_plot))
      p <- ggplot(data = data_for_plot, aes_string(x = input$explanatory, y = input$response)) +
        geom_point(alpha = input$shade) +
        theme_minimal()
   
      if (input$marginal) {
        p <- ggMarginal(p, type = "histogram")
      }
      
      return(p)
      
    } else if (input$plotType == "hist") {
      if (input$dataset == "Original"){
        data_for_hist <- File()[!is.na(File()[[input$var]]), ]
      }else if (input$dataset == "Training"){
        data_for_hist <- req(training())
        data_for_hist <- training()[!is.na(training()[[input$var]]), ]
      }else if (input$dataset == "Testing"){
        data_for_hist <- req(testing())
        data_for_hist <- testing()[!is.na(testing()[[input$var]]), ]
      }
      
      binwidth <- diff(range(data_for_hist[[input$var]], na.rm = TRUE)) / input$bins
      binwidth <- ifelse(binwidth > 0, binwidth, 1)
      ggplot(data = data_for_hist, aes_string(x = input$var)) +
        geom_histogram(binwidth = binwidth, fill = "blue", color = "black") +
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
    train_data <- training()
    cost_values <- c() 
    sigma_start_value <- c()
    if (input$costRange[1] == 0.01){
      start_value = 0.1 
      cost_values <- c(0.01)
    }else{
      start_value = input$costRange[1]
    }
    if (input$sigma[1] == 0.01){
      sigma_start_value = 0.1 
      sigma_values <- c(0.01)
    }else{
      sigma_start_value = input$costRange[1]
    }
    cost_values <- c(cost_values, seq(from = start_value, to = input$costRange[2], by = 0.2))
    sigma_values <- c(sigma_start_value, seq(from = sigma_start_value, to = input$sigma[2], by = 0.2))
    #cost_values <- c(cost_values, values)
    degree_values <- seq(from = input$degree[1], to=input$degree[2], by=1)
    print(cost_values)
    if (input$kernelType == "svmLinear"){
      tuneGrid <- expand.grid(C = cost_values)
    }else if (input$kernelType == "svmPoly"){
      tuneGrid <- expand.grid(C = cost_values,
                              degree = degree_values, scale=1)
    }else if (input$kernelType == "svmRadial"){
      tuneGrid <- expand.grid(C = cost_values,
                              sigma = sigma_values)
    }
    
    trainControl <- trainControl(method = "cv", number = 5, search = "grid", summaryFunction = defaultSummary)
    #print("reached here")
    #print(nrow(train_data))
    
    svmModel <- train(response_variable(), data = train_data, method = input$kernelType,
                      trControl = trainControl, tuneGrid = tuneGrid, metric="RMSE")
    
    print(svmModel)
    return(svmModel)
  })
  
  
  output$plotPerformance <- renderPlot({
    req(modelResults())
    # Plotting the results
    if ("results" %in% names(modelResults())) {
      if (input$kernelType == "svmRadial") {
        ggplot(modelResults()$results, aes(x = C, y = sigma, fill = RMSE)) +
          geom_tile() +  # Tile layer for heat map
          scale_fill_gradient(low = "blue", high = "red", name = "RMSE") +
          labs(title = "Radial SVM Performance", x = "Cost (C)", y = "Gamma (sigma)") +
          theme_minimal()
      }else if (input$kernelType == "svmPoly"){
        ggplot(modelResults()$results, aes(x = C, y = degree, fill = RMSE)) +
          geom_tile() +  # Tile layer for heat map
          scale_fill_gradient(low = "blue", high = "red", name = "RMSE") +
          labs(title = "Radial SVM Performance", x = "Cost (C)", y = "Degree") +
          theme_minimal()
      }else{
        ggplot(modelResults()$results, aes(x = C, y = RMSE)) +
          geom_line() +
          geom_point() +
          theme_minimal() +
          labs(title = "Model Performance", x = "Cost (C)", y = "RMSE")
      }
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
  
  train_model <- eventReactive(input$train_model, {
    train_data <- training()
    resample_final <- trainControl(method = "none") 
    grid <- data.frame()
    
    if (input$kernelType == "svmLinear"){
      grid = data.frame(C=input$C)
    }else if (input$kernelType == "svmPoly"){
      print(input$degree)
      grid = data.frame(C=input$C, degree=input$degree_final, scale=1)
    }else if (input$kernelType == "svmRadial"){
      print(input$sigma)
      grid = data.frame(C=input$C, sigma=input$sigma_final)
    }

    svm_model_final <- train(response_variable(), data = train_data, trControl = resample_final,
                             tuneGrid = grid, method = input$kernelType, metric="RMSE")
    
    return(svm_model_final)
  })
  
  output$svmFinalModelResults <- DT::renderDataTable({
    req(train_model())
    train_data <- training()
    test_data <- testing() 
    svm_model_final <- train_model()
    svm_pred_train <- predict(svm_model_final, newdata = train_data)
    svm_pred_test <- predict(svm_model_final, newdata = test_data)
    
    results_train <- postResample(pred = svm_pred_train, obs = train_data[[response_variable_name()]])
    results_test <- postResample(pred = svm_pred_test, obs = test_data[[response_variable_name()]])
    print(results_train)
    print(results_test)
    
    rmse_train <- results_train[1]
    rmse_test <- results_test[1]
    return(data.frame(Dataset = c("Train", "Test"), 
           RMSE = c(rmse_train, rmse_test)))
    
  })
  



  
}





# Run the application 

shinyApp(ui = ui, server = server)
