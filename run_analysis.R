run_analysis <- function(basepath){

#basepath is the path which contains .../UCI HAR Dataset
#Need libraries plyr and dplyr
  
#------------------Step 1-----------------#

setwd(basepath)
library(plyr)
library(dplyr)
  
#Test data
X_test<-read.table("./test/X_test.txt")
Y_test<-read.table("./test/y_test.txt")
subject_test<-read.table("./test/subject_test.txt")
features<-read.table("features.txt")

#Train data
X_train<-read.table("./train/X_train.txt")
Y_train<-read.table("./train/y_train.txt")
subject_train<-read.table("./train/subject_train.txt")

#Features
features<-read.table("features.txt")
features<-rename(features, c("V1"="Number", "V2"="Description"))
namesForX <- features$Description

#------------------Step 4-----------------#
names(X_test)<-namesForX
names(X_train)<-namesForX
Y_test<-rename(Y_test, c("V1"="Activity"))
Y_train<-rename(Y_train, c("V1"="Activity"))
subject_test<-rename(subject_test, c("V1"="Subject"))
subject_train<-rename(subject_train, c("V1"="Subject"))
#------------------Step 4 end-----------------#

#Combine test, train
totaltest <- cbind(subject_test, Y_test, X_test)
totaltrain <- cbind(subject_train, Y_train, X_train)

#Combine training and test set
total<-rbind(totaltrain, totaltest)

#------------------Step 1 end-----------------#


#------------------Step 2-----------------#

#Find indices of the mean columns
meanindex<-which(grepl("mean()", namesForX))
meanFreqindex<-which(grepl("meanFreq()", namesForX))
meanindex<-setdiff(meanindex, meanFreqindex)

#Find indices of the std columns
stdindex<- which(grepl("std()", namesForX))

#Merge the two (then add 2, because subject and features are the first two columns)
totalindex<-append(meanindex, stdindex)
totalindex <- totalindex + 2
totalindex<- append(c(1,2),totalindex)

#Only want mean and std
totalms<-total[,totalindex]

#------------------Step 2 end-----------------#


#------------------Step 3-----------------#

#Read activities file
activities <- read.table("activity_labels.txt")
activities$V2<-as.character(activities$V2)
numofacts<-nrow(activities)

#Loop through each activity and replace
for (i in 1:numofacts){
    totalms$Activity[totalms$Activity ==as.character(i)] <- activities[i,2]
}

#------------------Step 3 end-----------------#

#------------------Step 4-----------------#
    #Code for Step 4 is embedded in step 1
#------------------Step 4 end-----------------#


#------------------Step 5-----------------#
totalmsvar<-length(totalms)
summarydata<-aggregate(totalms[, 3:totalmsvar], list(Subject=totalms$Subject, Activity=totalms$Activity), mean)
#------------------Step 5 end-----------------#
}