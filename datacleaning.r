# install relevant libraries 
library(caret)
library(tidymodels)
library(tidyverse)
library(ggplot2)

#read in data file 
data = read.table('CommViolPredUnnormalizedData.txt', na.strings = "?")

head(data)