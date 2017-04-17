file_name = "/Users/Ed/Dropbox/Documents/Academic/Faculty/Teaching/Fall 2016/MSBA 6410/Lectures/Lecture 2/mobileApp-classVersion.csv"
mob1 <- read.csv(file_name)
View(mob1)

#good summary stats package
#install.packages("Hmisc")
library(Hmisc)
library(dplyr)

#detecting missing values
describe(mob1)

mob2<-tbl_df(mob1)
mob2[is.na(mob1$app_age_current_version),]

#visually detecting missing values
#install.packages("VIM")
library("VIM")

aggr(mob1, prop=FALSE, numbers=TRUE)
matrixplot(mob1)

describe(mob1$app_age_current_version)

#calculate median, omit rows with missing values
median(mob1$app_age_current_version)
median(mob1$app_age_current_version,  na.rm=TRUE)

#replace missing values with median
imp.median <- function (a){
  missing <- is.na(a)
  imputed <- a
  imputed[missing] <- median(a,  na.rm=TRUE)
  return (imputed)
}

mob1 <- read.csv(file_name)
describe(mob1$app_age_current_version)
mob1$app_age_current_version <- imp.median(mob1$app_age_current_version)
describe(mob1$app_age_current_version)


#impute missing values via simple random resampling
random.imp <- function (a){
  missing <- is.na(a) #this isolates the rows of the vector that has missing values
  n.missing <- sum(missing) #gives the number of missing values
  a.obs <- a[!missing] #this gets the empirical distribution 
  imputed <- a #temp placeholder to get the imputed values
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE) #impute using empirical dist.
  # make sure its with replacement
  return (imputed)
}  
  
mob1 <- read.csv(file_name)
describe(mob1$app_age_current_version)
mob1$app_age_current_version <- random.imp(mob1$app_age_current_version)
describe(mob1$app_age_current_version)
