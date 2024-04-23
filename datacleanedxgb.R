library(caret)
library(tidyverse)
library(ggplot2)
library(tidymodels)

#loading data
communities_data <- read.table(file.choose(), header = FALSE, sep = ",")

extract_column_names <- function(names_file){
  lines <- readLines(names_file)
  column_lines <- grep("^@attribute", lines, value = TRUE)
  column_names <- sapply(strsplit(column_lines, " "), '[',2)
  return(column_names)
}

names_file_path = file.choose()
attribute_names = extract_column_names(names_file_path)

colnames(communities_data) <- attribute_names

#create subcategory column groups
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

#find and remove missing values in data set for police subcategory & target variable 
communities_data[communities_data == "?"] <- NA
sum(is.na(communities_data))

sapply(communities_data, function(x) sum(is.na(x)))

colSums(is.na(communities_data))

#drop columns with 1675 missing values
drop_mv_cols <- function(data, threshold){
  missing_cols <- colSums(is.na(data))
  cols_to_drop <- names(missing_cols[missing_cols >= threshold])
  data <- data[, !(names(data) %in% cols_to_drop)]
  
  return(data)
}
communities_data <- drop_mv_cols(communities_data, threshold=1675)

#drop rows with NA for target variable 
communities_data <- communities_data[complete.cases(communities_data$violentPerPop), ]

#find near-zero variances in the dataset
nzv_indices <- nearZeroVar(communities_data)
nzv_variable_names <- names(communities_data)[nzv_indices]
print(nzv_variable_names)

#calculate correlation measure for all predictor variables 
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

#calculate average correlation measure for subcategories
#test average correlation per each subcategory 

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

#subcategories: race_cols, age_cols, pop_cols, econ_cols, edu_cols, emp_cols, fam_cols, img_cols, hous_cols
sub_pre_x_targ_correlation(race_cols)
sub_pre_x_targ_correlation(age_cols)
sub_pre_x_targ_correlation(pop_cols)
sub_pre_x_targ_correlation(econ_cols)
sub_pre_x_targ_correlation(edu_cols)
sub_pre_x_targ_correlation(emp_cols)
sub_pre_x_targ_correlation(fam_cols)
sub_pre_x_targ_correlation(img_cols)
sub_pre_x_targ_correlation(hous_cols)

#drop all rows that are non-predictive 
communities_data <- communities_data[, !colnames(communities_data) %in% c("communityname",
                                                                          "State", "countyCode", "communityCode")]

#making sure that target feature is continuous
communities_data$violentPerPop <- as.numeric(communities_data$violentPerPop)

#creating training and testing sets 
set.seed(1)
indices <- sample(1:nrow(communities_data), size = nrow(communities_data) * 0.75)
train <- communities_data[indices, ]
test <- communities_data[-indices, ]

# Pre-processing and feature engineering
blueprint <- recipe(violentPerPop ~ ., data = train) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_impute_knn(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>% #do we need to scale 
  step_dummy(all_nominal_predictors()) %>%
  prep()

communities_train <- bake(blueprint, new_data = train)
communities_test <- bake(blueprint, new_data = test)

plot(communities_data$violentPerPop, main = "Original Dataset")
plot(communities_train$violentPerPop, main = "Training Dataset")
plot(communities_test$violentPerPop, main = "Testing Dataset")

### XGBoost

resample <- trainControl(
  method = "cv",
  number = 5,
  search = "grid",
  summaryFunction = defaultSummary) 

xg_grid <- expand.grid(nrounds = c(100, 200, 300),   
                            max_depth = c(4, 6, 8, 10), 
                            eta = c(0.05, 0.1, 0.2, 0.3),    
                            min_child_weight = c(5, 10, 15), 
                            subsample = c(0.4, 0.6), 
                            gamma = 0,
                            colsample_bytree = 1)

xg_fit <- train(violentPerPop ~ .,
                data = communities_train, 
                method = "xgbTree",
                trControl = resample, 
                tuneGrid = xg_grid,
                metric = "RMSE")

resample_final <- trainControl(method = "none") 

xg_final <- train(violentPerPop ~ .,
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
                   metric = "ROC",
                   importance = TRUE)

# Training set results

XGB_pred_train <- predict(XGB_final, newdata = cca_train)
XGB_train_results <- confusionMatrix(cca_train$V16, XGB_pred_train)
print(XGB_train_results)

# Test set results

XGB_pred_test <- predict(XGB_final, newdata = cca_test)
XGB_test_results <- confusionMatrix(cca_test$V16, XGB_pred_test)
print(XGB_test_results)

# Evaluation metrics

XGB_Accuracy <- XGB_train_results$overall["Accuracy"]
print(XGB_Accuracy)

XGB_Kappa <- XGB_train_results$overall["Kappa"]
print(XGB_Kappa)