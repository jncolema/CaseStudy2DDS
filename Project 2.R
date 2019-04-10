# link to GitHub Repo: https://github.com/jncolema/CaseStudy2DDS/tree/master

getwd()

# install.packages("downloader")
library(downloader)
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/UNIT%2014/CaseStudy2-data.csv","Case Study 2.csv")
download("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/UNIT%2014/CaseStudy2Validation%20No%20Attrition.csv", "Validation Data.csv")
list.files()

EmpInfo <-read.csv("Case Study 2.csv", header = TRUE, sep = ",")
head(EmpInfo)
str(EmpInfo)
