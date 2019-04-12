# link to GitHub Repo: https://github.com/jncolema/CaseStudy2DDS/tree/master

getwd()

#Downloading the data
# install.packages("downloader")
library(downloader)
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/UNIT%2014/CaseStudy2-data.csv","Raw Data/Case Study 2.csv")
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/UNIT%2014/CaseStudy2Validation%20No%20Attrition.csv", "Raw Data/Validation Data.csv")
list.files()

#Importing the downloaded data
EmpInfo <-read.csv("Raw Data/Case Study 2.csv", header = TRUE, sep = ",")
head(EmpInfo)
str(EmpInfo)

#Exploratory plots
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("purrr")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)

#Bar chart of factor variables
EmpInfo %>%
  keep(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_bar() +
  theme(axis.text.x=element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(size=6))

#HIstogram of integer variables
EmpInfo %>%
  keep(is.integer) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram() +
  theme(axis.text.x=element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(size=6))

#Cleaning Table
##Removing Over18 variable since only 1 level of factor & EmployeeCount & StandardHours
EmpInfo2 <- EmpInfo[,-c(10, 23, 28)]
head(EmpInfo2)
str(EmpInfo2)
#Casting Rating Variables as Factors
EmpInfo2$Education <- as.factor(EmpInfo2$Education)
EmpInfo2$EnvironmentSatisfaction <- as.factor(EmpInfo2$EnvironmentSatisfaction)
EmpInfo2$JobInvolvement <- as.factor(EmpInfo2$JobInvolvement)
EmpInfo2$JobLevel <- as.factor(EmpInfo2$JobLevel)
EmpInfo2$JobSatisfaction <- as.factor(EmpInfo2$JobSatisfaction)
EmpInfo2$PerformanceRating <- as.factor(EmpInfo2$PerformanceRating)
EmpInfo2$RelationshipSatisfaction <- as.factor(EmpInfo2$RelationshipSatisfaction)
EmpInfo2$StockOptionLevel <- as.factor(EmpInfo2$StockOptionLevel)
EmpInfo2$TrainingTimesLastYear <- as.factor(EmpInfo2$TrainingTimesLastYear)
EmpInfo2$WorkLifeBalance <- as.factor(EmpInfo2$WorkLifeBalance)


#Grouping Categorical Variables
EmpFactors <- EmpInfo2[,c("Attrition", "BusinessTravel", "Department", "EducationField", "Gender", "JobRole", "MaritalStatus", "OverTime", "Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")]
EmpInfo2 %>% keep(is.factor)
#Grouping Integer Variables
EmpNumeric <- EmpInfo2 %>% keep(is.integer)

#summary statistics
mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x),max(x),IQR(x))
  names(result)<-c("N","Mean","SD","SE","Min","Max","IQR")
  return(result)
}


EmpNumStats <- round(sapply(EmpNumeric, mysummary), 2)
EmpNumStats
EmpFactStats <- sapply(sapply(EmpFactors, table), prop.table)
EmpFactStats <- sapply(EmpFactStats, signif)
EmpFactStats

#Checking for NAs
colSums(is.na(EmpInfo2))


#Correlation Plot of Integer Variables
install.packages("corrplot")  
library(corrplot)
EmpCorrelations <- cor(EmpInfo2 %>% keep(is.integer))
# EmpCorrelations <- cor(EmpInfo[,-c(10, 23, 28)] %>% keep(is.integer))
corrplot(EmpCorrelations, type = "lower", diag = TRUE)

#Attrition by variable
DeptAttr <- table(EmpInfo2$Department,EmpInfo2$Attrition)
margin.table(DeptAttr, 1) # Department Totals 
margin.table(DeptAttr, 2) # Attrition Totals

prop.table(DeptAttr) # cell percentages
prop.table(DeptAttr, 1) # row percentages 
prop.table(DeptAttr, 2) # column percentages


# list of variable names
variables <- sort(names(EmpInfo2))


#KNN
# install.packages("caret")
library(caret)

#creating training and test set
train_perc <- .6
train_indices <- sample(seq(1,nrow(EmpInfo2),by = 1), train_perc*nrow(EmpInfo2))
train <- EmpInfo2[train_indices,]
test <- EmpInfo2[-train_indices,]

#k=3 Confusion Matrix
class::knn(train[,c()], test[,-3], train$Attrition, k=3)
confusionMatrix


Use linear regression for prediction

Use KNN for classification