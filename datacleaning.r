#install packages 
library(caret)
library(tidyverse)
library(ggplot2)
library(tidymodels)

#data loading
communities_data <- read.table(file.choose(), header = FALSE, sep = ",")

#create column names + grouping
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
colSums(is.na(communities_data))
#drop columns with 1872 missing values
drop_mv_cols <- function(data, threshold){
  missing_cols <- colSums(is.na(data))
  cols_to_drop <- names(missing_cols[missing_cols >= threshold])
  data <- data[, !(names(data) %in% cols_to_drop)]
  
  return(data)
}
communities_data <- drop_mv_cols(communities_data, threshold=1872)

#drop rows with NA for target variable 
communities_data <- communities_data[complete.cases(communities_data$violentPerPop), ]

#find near-zero variances in the dataset
nzv_indices <- nearZeroVar(communities_data)
nzv_variable_names <- names(communities_data)[nzv_indices]
print(nzv_variable_names)

#visualize variable distributions using boxplots
boxplots <- function(data){
  for (col in colnames(data)){
      if(is.numeric(data[[col]])){
        name = paste("/Users/mercedesmoore/Desktop/Boxplots/", col, ".png", sep ="")
        png(name)
        boxplot(data[[col]], main = paste("Boxplot of", col), xlab = col)
        dev.off()
      }
      else{
        cat("Skipped", col, "\n")
      }
  }
}

boxplots(communities_data)

#visualize variable distributions using histograms
histograms <- function(data){
  for (col in colnames(data)){
      if(is.numeric(data[[col]])){
        name = paste("/Users/mercedesmoore/Desktop/Histograms/", col, ".png", sep ="")
        png(name)
        hist(data[[col]], main = paste("Histogram of", col), xlab = col, col="pink")
        dev.off()
      }
      else{
        cat("Skipped", col, "\n")
      }
  }
}

histograms(communities_data)

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
communities_data <- communities_data[, !colnames(communities_data) %in% c("communityname", "State", "countyCode", "communityCode")]

#drop all extraneous target variables 
communities_data <- communities_data[, !colnames(communities_data) %in% c("rapes", "rapesPerPop", "robberies", "robbbPerPop", "assaults", "assaultPerPop", "burglaries", "burglPerPop", "larcenies", "larcPerPop", "autoTheft", "autoTheft", "autoTheftPerPop", "arsons", "arsonsPerPop", "nonViolPerPop")]

#make sure all rows are numeric
communities_data$violentPerPop <- as.numeric(communities_data$violentPerPop)
communities_data$otherPerCap <- as.numeric(communities_data$otherPerCap)


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
  step_scale(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  prep()

communities_train <- bake(blueprint, new_data = train)
communities_test <- bake(blueprint, new_data = test)