
############## Read the csv and covert to data table ###############
library(dplyr)
mobile <- read.csv("mobileApp.csv")
head(mobile,5)
mobileApps<-tbl_df(mobile)

############## to filter and work only the chinese data ##############
mobile_China<-filter(mobileApps,region == 'CN')
View(mobile_China)

############## to list all the columns that do not start with the letter 'a'  ##########
not_start_with_a<-select(mobile,-starts_with('a'))
head(not_start_with_a,5)

############## to create a new variable called sales = 1/rank  ############
newdata_with_sales<-mutate(mobileApps,sales = 1/rank)
head(newdata_with_sales,5)

############## app store average ratings ################
by_app_store <- group_by(mobileApps,app_store)
by_app_store_China<-filter(by_app_store,region=='CN')
View(by_app_store_China)

#############to compare the average ratings in china in the two major app stores ################
summarise(by_app_store_China, average_rating = mean(average_rating, na.rm = TRUE))

########### To list the price, developer rating, and rating count of the top 10 selling(ranked best) apps in each app store in china
sort_rank<-mobile_China[order(mobile_China$rank),]
head(sort_rank,5)
top_rank<-subset(sort_rank, rank < 11)
View(top_rank[,c("rank","price","average_rating","rating_count")])

############ To comapre US vs China with respect to top 10 developers##############

#subset the mobileApps data table into 3 relevant columns- Country, developer and rating count 
dev_rating<-data.table(mobileApps$region,mobileApps$developer,mobileApps$rating_count)
head(dev_rating,5)

# assigning columns names to the subset data table 
col_names <- c("Country",
               "Developer",
               "Rating_count")

colnames(dev_rating) <- col_names
head(dev_rating,5)

#subset dev_rating into China and USA 
dev_rating_China<-filter(dev_rating,Country=='CN')
head(dev_rating_China,5)
dev_rating_USA<-filter(dev_rating,Country=='US')
head(dev_rating_USA,5)

#Compare to 10 developers for US and China in terms of rating count 
dev_rating_China[order(dev_rating_China$Rating_count,decreasing=T)[1:10],]
dev_rating_USA[order(dev_rating_USA$Rating_count,decreasing=T)[1:10],]
