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

git config --global --edit

After doing this, you may fix the identity used for this commit with:
  
  git commit --amend --reset-author

6 files changed, 1202 insertions(+)
create mode 100644 .gitignore
create mode 100644 Case Study 2.csv
create mode 100644 CaseStudy2DDS.Rproj
create mode 100644 Project 2.R
create mode 100644 Validation Data.csv
create mode 100644 Validation No Salary.xlsx

install.packages("git2r")
library(git2r)
push(getwd(), "origin", "refs/heads/master")

git merge origin/master --allow-unrelated-histories
merge.git_b