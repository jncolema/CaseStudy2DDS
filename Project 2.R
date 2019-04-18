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

#-----
#Importing Attrition Validation File
Attr_Validation <-read.csv("Raw Data/Validation Data.csv", header = TRUE, sep = ",")
str(Attr_Validation)

#cleaning Validation Table
##Removing Over18 variable since only 1 level of factor & EmployeeCount & StandardHours
Attr_Validation <- Attr_Validation[,-c(9, 22, 27)]
str(EmpInfo2)
#Casting Rating Variables as Factors
Attr_Validation$Education <- as.factor(Attr_Validation$Education)
Attr_Validation$EnvironmentSatisfaction <- as.factor(Attr_Validation$EnvironmentSatisfaction)
Attr_Validation$JobInvolvement <- as.factor(Attr_Validation$JobInvolvement)
Attr_Validation$JobLevel <- as.factor(Attr_Validation$JobLevel)
Attr_Validation$JobSatisfaction <- as.factor(Attr_Validation$JobSatisfaction)
Attr_Validation$PerformanceRating <- as.factor(Attr_Validation$PerformanceRating)
Attr_Validation$RelationshipSatisfaction <- as.factor(Attr_Validation$RelationshipSatisfaction)
Attr_Validation$StockOptionLevel <- as.factor(Attr_Validation$StockOptionLevel)
Attr_Validation$TrainingTimesLastYear <- as.factor(Attr_Validation$TrainingTimesLastYear)
Attr_Validation$WorkLifeBalance <- as.factor(Attr_Validation$WorkLifeBalance)
#----
#Importing Salary Prediction File
Income_Predict <-read.csv("Raw Data/Validation No Salary.csv", header = TRUE)
str(Income_Predict)
head(Income_Predict)
#cleaning Prediction File
##Removing Over18 variable since only 1 level of factor & EmployeeCount & StandardHours
Income_Predict <- Income_Predict[,-c(10, 22, 27)]
str(EmpInfo2)
#Casting Rating Variables as Factors
Income_Predict$Education <- as.factor(Income_Predict$Education)
Income_Predict$EnvironmentSatisfaction <- as.factor(Income_Predict$EnvironmentSatisfaction)
Income_Predict$JobInvolvement <- as.factor(Income_Predict$JobInvolvement)
Income_Predict$JobLevel <- as.factor(Income_Predict$JobLevel)
Income_Predict$JobSatisfaction <- as.factor(Income_Predict$JobSatisfaction)
Income_Predict$PerformanceRating <- as.factor(Income_Predict$PerformanceRating)
Income_Predict$RelationshipSatisfaction <- as.factor(Income_Predict$RelationshipSatisfaction)
Income_Predict$StockOptionLevel <- as.factor(Income_Predict$StockOptionLevel)
Income_Predict$TrainingTimesLastYear <- as.factor(Income_Predict$TrainingTimesLastYear)
Income_Predict$WorkLifeBalance <- as.factor(Income_Predict$WorkLifeBalance)
#----
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

#Attrition by Categorical variable
# DeptAttr <- table(EmpInfo2$Department,EmpInfo2$Attrition)
# margin.table(DeptAttr, 1) # Department Totals 
# margin.table(DeptAttr, 2) # Attrition Totals
# 
# prop.table(DeptAttr) # cell percentages
# prop.table(DeptAttr, 1) # row percentages 
# prop.table(DeptAttr, 2) # column percentages

TotalAttr <- table(EmpInfo2$Attrition)
BusTravAttr <- table(EmpInfo2$BusinessTravel,EmpInfo2$Attrition)
DeptAttr <- table(EmpInfo2$Department,EmpInfo2$Attrition)
EduFieldAttr <- table(EmpInfo2$EducationField,EmpInfo2$Attrition)
GenderAttr <- table(EmpInfo2$Gender,EmpInfo2$Attrition)
RoleAttr <- table(EmpInfo2$JobRole,EmpInfo2$Attrition)
MaritalAttr <- table(EmpInfo2$MaritalStatus,EmpInfo2$Attrition)
OverTimeAttr <- table(EmpInfo2$OverTime,EmpInfo2$Attrition)

EducationAttr <- table(EmpInfo2$Education,EmpInfo2$Attrition)
EnvirSatAttr <- table(EmpInfo2$EnvironmentSatisfaction,EmpInfo2$Attrition)
JobInvAttr <- table(EmpInfo2$JobInvolvement,EmpInfo2$Attrition)
JobLevAttr <- table(EmpInfo2$JobLevel,EmpInfo2$Attrition)
JobSatAttr <- table(EmpInfo2$JobSatisfaction,EmpInfo2$Attrition)
PerRatAttr <- table(EmpInfo2$PerformanceRating,EmpInfo2$Attrition)
RelSatAttr <- table(EmpInfo2$RelationshipSatisfaction,EmpInfo2$Attrition)
StockAttr <- table(EmpInfo2$StockOptionLevel,EmpInfo2$Attrition)
TrainTimeAttr <- table(EmpInfo2$TrainingTimesLastYear,EmpInfo2$Attrition)
WorkLifeAttr <- table(EmpInfo2$WorkLifeBalance,EmpInfo2$Attrition)



TotalAttr_Prop <- prop.table(TotalAttr)
BusTravAttr_Prop <- data.frame(prop.table(BusTravAttr, 1))
DeptAttr_Prop <- data.frame(prop.table(DeptAttr, 1) )
EduFieldAttrAttr_Prop <- data.frame(prop.table(EduFieldAttr, 1) )
GenderAttr_Prop <- data.frame(prop.table(GenderAttr, 1)) 
RoleAttr_Prop <- data.frame(prop.table(RoleAttr, 1) )
MaritalAttr_Prop <- data.frame(prop.table(MaritalAttr, 1)) 
OverTimeAttr_Prop <- data.frame(prop.table(OverTimeAttr, 1)) 

EducationAttr_Prop <- data.frame(prop.table(EducationAttr, 1))
EnvirSatAttr_Prop <- data.frame(prop.table(EnvirSatAttr, 1)) 
JobInvAttr_Prop <- data.frame(prop.table(JobInvAttr, 1)) 
JobLevAttr_Prop <- data.frame(prop.table(JobLevAttr, 1)) 
JobSatAttr_Prop <- data.frame(prop.table(JobSatAttr, 1)) 
PerRatAttr_Prop <- data.frame(prop.table(PerRatAttr, 1)) 
RelSatAttr_Prop <- data.frame(prop.table(RelSatAttr, 1)) 
StockAttr_Prop <- data.frame(prop.table(StockAttr, 1)) 
TrainTimeAttr_Prop <- data.frame(prop.table(TrainTimeAttr, 1)) 
WorkLifeAttr_Prop <- data.frame(prop.table(WorkLifeAttr, 1)) 



#Plotting Attrition Perctages for Categorical Variables
ggplot(data=BusTravAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Business Travel Attrition")
ggplot(data=DeptAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Department Attrition")
ggplot(data=EduFieldAttrAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Attrition by Education Field")
ggplot(data=GenderAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Attrition by Gender")
ggplot(data=RoleAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Job Role Attrition") +
  theme(axis.text.x=element_text(angle=90))
ggplot(data=MaritalAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Attrition by Marital Status")
ggplot(data=OverTimeAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Overtime Attrition")
  
ggplot(data=EducationAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Education Attrition")
ggplot(data=EnvirSatAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Environment Satisfaction Attrition")
ggplot(data=JobInvAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Job Involvement Attrition")
ggplot(data=JobLevAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Job Level Attrition")
ggplot(data=JobSatAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Job Satisfaction Attrition")
ggplot(data=PerRatAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Attrition by Performance Rating")
ggplot(data=RelSatAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Attrition by Relationship Satisfaction")
ggplot(data=StockAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Attrition by Stock Option Level")
ggplot(data=TrainTimeAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Training Time Attrition") +
  ggtitle("Overtime Attrition")
ggplot(data=WorkLifeAttr_Prop, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme (axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Attrition") +
  ggtitle("Work Life Balance Attrition")


#Attrition doesn't appear to vary by gender, performance rating, relationship satisfaction
#but there does appear to be larger variance depending on Job Role, Marital Status, and Overtime, Job Involvement, Job Level, .

#Plotting Continuous Variable Variance by Attrition
ggplot(data=EmpInfo2, aes(x=Age, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=DailyRate, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..)))
ggplot(data=EmpInfo2, aes(x=DistanceFromHome, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=HourlyRate, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=MonthlyRate, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=MonthlyIncome, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=TotalWorkingYears, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=YearsAtCompany, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=YearsInCurrentRole, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=YearsSinceLastPromotion, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 
ggplot(data=EmpInfo2, aes(x=YearsWithCurrManager, fill=Attrition)) + ylab("Percent") +
  geom_histogram(aes(y=(..count..)/sum(..count..))) 

# Attrition varies proportionately with variables above


##Naive Bayes Model to classify Attrition
#Referencing https://rpubs.com/riazakhan94/naive_bayes_classifier_e1071
library(caret)

#Partitioning dataset
set.seed(7)
trainIndex <- createDataPartition(EmpInfo2$Attrition, p=0.7)$Resample1
train <- EmpInfo2[trainIndex, ]
test <- EmpInfo2[-trainIndex, ]
#checking totals
print(table(EmpInfo2$Attrition))
print(table(train$Attrition))

#Creating Classifier
# install.packages("e1071")
library(e1071)

printALL <- function(model){
  trainPred <- predict(model, newdata = train, type = "class")
  trainTable <- table(train$Attrition, trainPred)
  testPred <- predict(Classifier_Model, newdata=test, type="class")
  testTable <- table(test$Attrition, testPred)
  trainAcc <- (trainTable[1,1]+trainTable[2,2])/sum(trainTable)
  testAcc <- (testTable[1,1]+testTable[2,2])/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
  message("Training Sensitivity") #Need at least 0.6
  print(trainTable[1,1]/(trainTable[1,1]+trainTable[2,1]))
  message("Test Sensitivity") #Need at least 0.6
  print(testTable[1,1]/(testTable[1,1]+testTable[2,1]))
  message("Training Specificity") #Need at least 0.6
  print(trainTable[2,2]/(trainTable[1,2]+trainTable[2,2]))
  message("Test Specificity") #Need at least 0.6
  print(testTable[2,2]/(testTable[1,2]+testTable[2,2]))
}
Classifier_Model <- NB_Classfier3 #Change for each new model
printALL(Classifier_Model)




#All in Model
NB_Classfier1 <- naiveBayes(Attrition~., data=train)
print(NB_Classfier1)
Classifier_Model <- NB_Classfier1 #Change for each new model
printALL(Classifier_Model)

#Model2
#Removing DailyRate, Department, Gender, PerformanceRating, RelationshipSatisfaction due to low variance.
#Removing Age, MonthlyRate, TotalWorkingYears due to high correlation with mulitple other variables
NB_Classfier2 <- naiveBayes(Attrition~BusinessTravel+ DistanceFromHome+ Education+ EducationField+ EnvironmentSatisfaction+ HourlyRate+ JobInvolvement+ JobLevel+ JobRole+ JobSatisfaction+ MaritalStatus+ MonthlyIncome+ NumCompaniesWorked+ OverTime+ PercentSalaryHike+ StockOptionLevel+ TrainingTimesLastYear+ WorkLifeBalance+ YearsAtCompany+ YearsInCurrentRole+ YearsSinceLastPromotion+ YearsWithCurrManager
, data=train)
print(NB_Classfier2)
Classifier_Model <- NB_Classfier2 #Change for each new model
printALL(Classifier_Model)

#Model3
#Adding Top 5 Variables with highest Attrition Variance
NB_Classfier3 <- naiveBayes(Attrition~EducationField+JobRole+OverTime+JobInvolvement+JobLevel, data=train)
print(NB_Classfier3)
Classifier_Model <- NB_Classfier3 #Change for each new model
printALL(Classifier_Model)
##This model has best stats

#Predicting Attrition for Validation Dataset
Predicted_Attr <- predict(Classifier_Model,newdata = Attr_Validation,type = 'class')
Attr_Validation$Attrition <- Predicted_Attr
head(Attr_Validation)
dim(Attr_Validation)
write.csv(Attr_Validation[,c(1,34)],"Case2PredictionsColeman Attrition.csv")




# #All Variables
# ID, Age, Attrition, BusinessTravel, DailyRate, Department, DistanceFromHome, Education, EducationField, EnvironmentSatisfaction, Gender, HourlyRate, JobInvolvement, JobLevel, JobRole, JobSatisfaction, MaritalStatus, MonthlyIncome, MonthlyRate, NumCompaniesWorked, OverTime, PercentSalaryHike, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager
# 
# ID, Age, Attrition, BusinessTravel, DailyRate, Department, DistanceFromHome, Education, EducationField, EnvironmentSatisfaction, Gender, HourlyRate, JobInvolvement, JobLevel, JobRole, JobSatisfaction, MaritalStatus, MonthlyIncome, MonthlyRate, NumCompaniesWorked, OverTime, PercentSalaryHike, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager



#Linear Regression
#Full Model - Internal Validation

#HIstogram of integer variables
ggplot(data=EmpInfo2, aes(x=MonthlyIncome)) +
  geom_histogram(binwidth = 500) 

EmpInfo2$logMonthlyIncome <-log(EmpInfo2$MonthlyIncome)

ggplot(data=EmpInfo2, aes(x=logMonthlyIncome)) +
  geom_histogram(binwidth = .01) 

# EmpInfo2$sqrtMonthlyIncome <-sqrt(EmpInfo2$MonthlyIncome)
# 
# ggplot(data=EmpInfo2, aes(x=sqrtMonthlyIncome)) +
#   geom_histogram() 

library(caret)
library(leaps)
#Fit full linear model
LinearModel <- lm(MonthlyIncome ~ ., train)
summary(LinearModel)

#Referencing http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
TrainControl <- trainControl(method = "cv", number = 10)

# Backward Selection
BackwardModel <- train(MonthlyIncome ~., data = train, method = "leapBackward", trControl = TrainControl)     
BackwardModel$results #includes RMSE
BackwardPredict <- predict(BackwardModel, newdata = test)
test$BackwardPredict <- BackwardPredict

# Forward Selection
ForwardModel <- train(MonthlyIncome ~., data = train, method = "leapForward", trControl = TrainControl)
ForwardModel$results #includes RMSE
ForwardPredict <- predict(ForwardModel, newdata = test)
test$ForwardPredict <- ForwardPredict

# Stepwise Selection
StepwiseModel <- train(MonthlyIncome ~., data = train, method = "leapSeq", trControl = TrainControl)
StepwiseModel$results #includes RMSE
StepwisePredict <- predict(StepwiseModel, newdata = test)
test$StepwisePredict <- StepwisePredict


#Calculating ASE stats for each model
ASEholderForward = c()
ASEholderBackward = c()
ASEholderStepwise = c()

ASEholderForward = sum((test$ForwardPredict - test$MonthlyIncome)^2)/length(test$MonthlyIncome)
ASEholderBackward = sum((test$BackwardPredict - test$MonthlyIncome)^2)/length(test$MonthlyIncome)
ASEholderStepwise = sum((test$StepwisePredict - test$MonthlyIncome)^2)/length(test$MonthlyIncome)

ASEholderForward
ASEholderBackward
ASEholderStepwise

##Backward Selection has lowest RMSE and ASE and will be used on the Validation Dataset
Final_Predictions <- predict(BackwardModel, newdata = Income_Predict)
str(Income_Predict)
head(Income_Predict)

Income_Predict$MonthlyIncome <- Final_Predictions
head(Income_Predict)
dim(Income_Predict)
write.csv(Income_Predict[,c(1,34)],"Case2PredictionsColeman Salary.csv")




------------------------------
#Referencing https://daviddalpiaz.github.io/r4sl/the-caret-package.html#regression
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}


sim_lm_mod = train(
  MonthlyIncome ~ .,
  data = train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)

get_best_result(sim_lm_mod)$RMSE

##Histogram of residuals with normal curve superimposed
lm_resid <- resid(sim_lm_mod)
hist(lm_resid, prob = TRUE)
curve(dnorm(x, mean=mean(lm_resid), sd=sd(lm_resid)), add=TRUE, 
      col = "blue", main='Histogram of Residuals for Linear Model', xlab='Residuals')
box()

##Plot of residuals, by index
plot(lm_resid)
lines(rep(0,length(lm_resid)))

#Full Model on Validation File
Predicted <- predict(sim_lm_mod, newdata = train)



---------------------------------


xfit <- seq(min(sresid), max(sresid), length=40) 
yfit <- dnorm(xfit) 



