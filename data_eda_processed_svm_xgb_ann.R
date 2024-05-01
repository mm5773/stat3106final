library(caret)
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(MASS)
library(SuperLearner)

# Loading Data

communities_data <- read.table(file.choose(), header=FALSE, sep=",", na.strings = "?", stringsAsFactors=TRUE)

extract_column_names <- function(names_file){
  lines <- readLines(names_file)
  column_lines <- grep("^@attribute", lines, value = TRUE)
  column_names <- sapply(strsplit(column_lines, " "), '[',2)
  return(column_names)
}

names_file_path = file.choose()
attribute_names = extract_column_names(names_file_path)

colnames(communities_data) <- attribute_names
head(communities_data)

# Exploratory Data Analysis

## Identify missing values
sum(is.na(communities_data))
sapply(communities_data, function(x) sum(is.na(x)))
colSums(is.na(communities_data))

## Drop columns with identical large number of missing values
drop_mv_cols <- function(data, threshold){
  missing_cols <- colSums(is.na(data))
  cols_to_drop <- names(missing_cols[missing_cols >= threshold])
  data <- data[, !(names(data) %in% cols_to_drop)]
}
communities_data <- drop_mv_cols(communities_data, threshold=1872)

## Drop rows with NA for target variable 
communities_data <- communities_data[complete.cases(communities_data$violentPerPop), ]

## Drop all rows that are non-predictive 
communities_data <- communities_data[, !colnames(communities_data) %in% c("communityname",
                                                                          "State", "countyCode", "communityCode")]

## Identify near-zero variances
nzv_indices <- nearZeroVar(communities_data)
nzv_variable_names <- names(communities_data)[nzv_indices]
print(nzv_variable_names)

## Visualization
target_feature <- "violentPerPop"
boxplot(communities_data[[target_feature]], main = paste("Boxplot of", target_feature), xlab = target_feature)
hist(communities_data[[target_feature]], main = paste("Histogram of", target_feature), xlab = target_feature, target_feature="darkgreen")

## Transforming skewed target feature
shifted_variable <- communities_data$violentPerPop - min(communities_data$violentPerPop) + 1  # Adding 1 to avoid zero
lm_model <- lm(shifted_variable ~ 1)
boxcox_result <- boxcox(lm_model)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
communities_data$violentPerPop <- if (lambda == 0) log(shifted_variable) else ((shifted_variable^lambda - 1) / lambda)
hist(communities_data$violentPerPop, main = "Transformed Distribution of violentPerPop")

## Create subcategory column groups
race_cols <- c('pctWhite', 'pctAsian', 'pctBlack', 'pctHisp')
head(communities_data[race_cols])
age_cols <- c('pct12-21', 'pct12-29', 'pct16-24', 'pct65up')
head(communities_data[age_cols])
pop_cols <- c('pop', 'perHoush', 'persUrban', 'pctUrban', 'landArea', 'popDensity')
head(communities_data[pop_cols])
econ_cols <- c('medIncome', 'pctWwage', 'pctWfarm', 'pctWdiv', 'pctWsocsec', 'pctPubAsst', 'pctRetire', 'medFamIncome', 'perCapInc', 'whitePerCap', 'blackPerCap', 'NAperCap', 'asianPerCap', 'otherPerCap', 'hispPerCap', 'persPoverty', 'pctPoverty')
head(communities_data[econ_cols])
edu_cols <- c('pctLowEdu', 'pctNotHSgrad', 'pctCollGrad')
head(communities_data[edu_cols])
emp_cols <- c('pctUnemploy', 'pctEmploy', 'pctEmployMfg', 'pctEmployProfServ', 'pctOccupManu', 'pctOccupMgmt') 
head(communities_data[emp_cols])
fam_cols <- c('pctMaleDivorc', 'pctMaleNevMar', 'pctFemDivorc', 'pctAllDivorc', 'persPerFam', 'pct2Par', 'pctKids2Par', 'pctKids-4w2Par', 'pct12-17w2Par', 'pctWorkMom-6', 'pctWorkMom-18', 'kidsBornNevrMarr', 'pctKidsBornNevrMarr')
head(communities_data[fam_cols])
img_cols <- c('numForeignBorn', 'pctFgnImmig-3', 'pctFgnImmig-5', 'pctFgnImmig-8', 'pctFgnImmig-10', 'pctImmig-3', 'pctImmig-5', 'pctImmig-8', 'pctImmig-10', 'pctSpeakOnlyEng', 'pctNotSpeakEng')
head(communities_data[img_cols])
hous_cols <- c('pctLargHousFam', 'pctLargHous', 'persPerOccupHous', 'persPerOwnOccup', 'persPerRenterOccup', 'pctPersOwnOccup', 'pctPopDenseHous', 'pctSmallHousUnits', 'medNumBedrm', 'houseVacant', 'pctHousOccup', 'pctHousOwnerOccup', 'pctVacantBoarded', 'pctVacant6up', 'medYrHousBuilt', 'pctHousWOphone', 'pctHousWOplumb', 'ownHousLowQ', 'ownHousMed', 'ownHousUperQ', 'ownHousQrange', 'rentLowQ', 'rentMed', 'rentUpperQ', 'rentQrange', 'medGrossRent', 'medRentpctHousInc', 'medOwnCostpct', 'medOwnCostPctWO', 'persEmergShelt', 'persHomeless', 'pctForeignBorn', 'pctBornStateResid', 'pctSameHouse-5', 'pctSameCounty-5', 'pctSameState-5', 'pctUsePubTrans')
head(communities_data[hous_cols])
pol_cols <- c('numPolice', 'policePerPop', 'policeField', 'policeFieldPerPop', 'policeCalls', 'policCallPerPop', 'policCallPerOffic', 'policePerPop2', 'racialMatch', 'pctPolicWhite', 'pctPolicBlack', 'pctPolicHisp', 'pctPolicAsian', 'pctPolicMinority', 'officDrugUnits', 'numDiffDrugsSeiz', 'policAveOT', 'policCarsAvail', 'policOperBudget', 'pctPolicPatrol', 'gangUnit', 'pctOfficDrugUnit', 'policBudgetPerPop') 

## Calculate correlation measure for all predictor variables 
pred_x_targ_correlation <-function(data){
  for(col in colnames(data)){
    if(is.numeric(data[[col]])){
      print(col)
      cur_cor <- cor(data[[col]], as.numeric(communities_data$violentPerPop))
      print(cur_cor)
    }
    else{
      cat("Skipped", col, "\n")
    }
  }
}

pred_x_targ_correlation(communities_data)

## Calculate average correlation measure for subcategories
sub_pre_x_targ_correlation <- function(subcategory){
  cur_avg = 0
  div = 1
  for(col in subcategory){
    if(is.numeric(communities_data[[col]])){
      cur_cor <- cor(communities_data[[col]], as.numeric(communities_data$violentPerPop))
      cur_avg <- cur_avg + abs(cur_cor)
      div <- div + 1
    }
    else{
      cat("Skipped", col, "\n")
    }
  }
  avg = cur_avg / div
  print(avg)
}

sub_pre_x_targ_correlation(race_cols)
sub_pre_x_targ_correlation(age_cols)
sub_pre_x_targ_correlation(pop_cols)
sub_pre_x_targ_correlation(econ_cols)
sub_pre_x_targ_correlation(edu_cols)
sub_pre_x_targ_correlation(emp_cols)
sub_pre_x_targ_correlation(fam_cols)
sub_pre_x_targ_correlation(img_cols)
sub_pre_x_targ_correlation(hous_cols)

# Data Splitting 

set.seed(1)
indices <- sample(1:nrow(communities_data), size = nrow(communities_data) * 0.75)
train <- communities_data[indices, ]
test <- communities_data[-indices, ]

# Data Pre-Processing and Feature Engineering

blueprint <- recipe(violentPerPop ~ ., data = train) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_impute_knn(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  prep()

communities_train <- bake(blueprint, new_data = train)
communities_test <- bake(blueprint, new_data = test)

hist(communities_data$violentPerPop, main = "Original Dataset")
hist(communities_train$violentPerPop, main = "Training Dataset")
hist(communities_test$violentPerPop, main = "Testing Dataset")

# Models

## SVM 
resample <- trainControl(
  method = "cv",
  number = 5,
  search = "grid",
  summaryFunction = defaultSummary) 

SVMGrid_Lin <- expand.grid(C = c(0.01, 0.1, 0.3))  # Linear SVM

SVMGrid_Poly <- expand.grid(C = c(0.01, 0.1),   # Polynomial SVM
                            
                            degree = c(2, 3),
                            
                            scale = 1)

SVMGrid_RBF <- expand.grid (sigma = c(0.01, 0.1),  # RBF (Radial Basis Function) SVM
                            
                            C = c(0.01, 0.1))

SVM_Linear <- train(violentPerPop ~ ., data = communities_train,
                    
                    method = "svmLinear",
                    
                    trControl = resample,
                    
                    verbose = FALSE,
                    
                    tuneGrid = SVMGrid_Lin,
                    
                    metric = "RMSE")

SVM_Poly <- train(violentPerPop ~ ., data = communities_train,
                  
                  method = "svmPoly",
                  
                  trControl = resample,
                  
                  verbose = FALSE,
                  
                  tuneGrid = SVMGrid_Poly,
                  
                  metric = "RMSE")

SVM_RBF <- train(violentPerPop ~ ., data = communities_train,
                 
                 method = "svmRadial",
                 
                 trControl = resample,
                 
                 verbose = FALSE,
                 
                 tuneGrid = SVMGrid_RBF,
                 
                 metric = "RMSE")

resamps <- resamples(list(SVM_Linear = SVM_Linear,
                          
                          SVM_Poly = SVM_Poly,
                          
                          SVM_RBF = SVM_RBF))

summary(resamps)
#Based on the summary results for SVM_Linear, SVM_Poly, and SVM_RBF, SVM_Linear 
#seems to be the best model since it has on lower RMSE and higher Rsquared values. 

plot(SVM_Linear)
SVM_Linear
#the optimal parameter for SVM linear is C=0.1

resample_final <- trainControl(method = "none") 

SVM_Final <- train(violentPerPop ~ ., data = communities_train,
                   
                   method = "svmLinear",
                   
                   trControl = resample_final,
                   
                   verbose = FALSE,
                   
                   tuneGrid = data.frame(C=0.1),
                   
                   metric = "RMSE")

svm_pred_train <- predict(SVM_Final, newdata = communities_train)
results_train <- postResample(pred = svm_pred_train, obs = communities_train$violentPerPop)
cat("RMSE:", results_train[1], "\nMAE:", results_train[2], "\n")

svm_pred_test <- predict(SVM_Final, newdata = communities_test)
results_test <- postResample(pred = svm_pred_test, obs = communities_test$violentPerPop)
cat("RMSE:", results_test[1], "\nMAE:", results_test[2], "\n")

## XGBoost
XGB_Grid <- expand.grid(nrounds = c(100, 150),   
                            max_depth = c(4, 6), 
                            eta = c(0.05, 0.1),    
                            min_child_weight = 5, 
                            subsample = 0.4, 
                            gamma = 0,
                            colsample_bytree = 1)

XGB_Fit <- train(violentPerPop ~ .,
                data = communities_train, 
                method = "xgbTree",
                trControl = resample, 
                tuneGrid = XGB_Grid,
                metric = "RMSE")
plot(XGB_Fit)
XGB_Fit

XGB_Final <- train(violentPerPop ~ .,
                   data = communities_train, 
                   method = "xgbTree",
                   verbose = FALSE,
                   trControl = resample_final, 
                   tuneGrid = data.frame(nrounds = 150,   
                                         max_depth = 4, 
                                         eta = 0.2,    
                                         min_child_weight = 6, 
                                         subsample = 0.7, 
                                         gamma = 0,
                                         colsample_bytree = 1),
                   metric = "RMSE",
                   importance = TRUE)

xgb_pred_train <- predict(XGB_Final, newdata = communities_train)
xgb_train_results <- postResample(xgb_pred_train, communities_train$violentPerPop)
cat("RMSE:", xgb_train_results[1], "\nMAE:", xgb_train_results[2], "\n")

xgb_pred_test <- predict(XGB_Final, newdata = communities_test)
XGB_test_results <- postResample(XGB_pred_test, communities_test$violentPerPop)
cat("RMSE:", XGB_test_results[1], "\nMAE:", XGB_test_results[2], "\n")

## ANN Model
ANN_Grid <- expand.grid(layer1 = c(25, 45),
                          layer2 = c(0, 30),
                          layer3 = c(0, 15))

ANN_Fit <- train(violentPerPop ~ .,
                 data = communities_train, 
                 method = "mlpML",
                 trControl = resample, 
                 tuneGrid = ANN_Grid,
                 metric = "RMSE")

plot(ANN_Fit)
ANN_Fit

ANN_Final <- train(violentPerPop ~., 
                   data = communities_train,
                   method = "mlpML",
                   trControl = resample_final,
                   metric = "RMSE",
                   tuneGrid = data.frame(layer1 = 25,
                                         layer2 = 0,
                                         layer3 = 15))

ANN_pred_train <- predict(ANN_Final, newdata = communities_train)
ANN_train_results <- postResample(ANN_pred_train, communities_train$violentPerPop)
cat("RMSE:", ANN_train_results[1], "\nMAE:", ANN_train_results[2], "\n")

ANN_pred_test <- predict(ANN_Final, newdata = communities_test)
ANN_test_results <- postResample(ANN_pred_test, communities_test$violentPerPop)
cat("RMSE:", ANN_test_results[1], "\nMAE:", ANN_test_results[2], "\n")

## Stacked Model




