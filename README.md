# Getting-CleaningData_CourseProject

author: "angieFelipe".

date: "30 de abril de 2016"

This is Angie Felipe Getting and Cleaning Data Course Project

## INTRODUCTION
The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
I have been required to submit: 

        1) a tidy data set as described below, 
        
        2) a link to a Github repository with your script for performing the analysis, and 
        
        3) a code book that describes the variables, the data, and any transformations or work that I performed to clean up the data called CodeBook.md.
        
        
        I have  also included a README.md (this one) in the repo with my scripts. This repo explains how all of the scripts work and how they are connected.

## SOME BACKGROUND

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:


http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

I have create one R script called run_analysis.R that does the following:

        0) Downloads all the information required, reads and create dataframes and all that I need to proceed with the project
           (main data frames are called xtest and xtrain
           
        1) Merges the training and the test sets to create one data set. in data frame HARdataset
        
        2) Extracts only the measurements on the mean and standard deviation for each measurement. in data frame HARsubset
        
        3) Uses descriptive activity names to name the activities in the data set in data frame HARsubset
        
        4) Appropriately labels the data set with descriptive variable names. in data frame HARsubset
        
        5) From the data set in step 4, I have created a second, independent tidy data set named with the average of each variable for each activity and each subject. in data frame HARaverage.subset
        
        
I have recorded the data frames accordingly in HARdataset.txt, HARsubset.txt and HARaverage.txt. Codebook for all data sets are available in CodeBook.md in this repo


##Explanation of the script



###creating an especific directory for data
if(!file.exists("data")){
        dir.create("data")
}

###saving urls indicated by the assignment

infoUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.names"

datasetUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"

dataProjectUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

htmlinfoUrl<-"http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"


###downloading data and date

download.file(dataProjectUrl,destfile = "./data/dataset.zip",method = "curl")

dataset.downloadingDate<-date() 

##unziping files
unzip("./data/dataset.zip", exdir = "./data")

dataset.info<-unzip("./data/dataset.zip", list = T)


##reading appropiate files & create appropiate data set also
(4) Appropriately labels the data set with descriptive variable names.

 ####reading list of features
 
variable.names<-read.table(paste0("./data/",
                        dataset.info[2,1]),
                        stringsAsFactors = F)
                       
####rearrenging variable names
erase.punctuation<-  function(x) {x<-gsub("-","",x);
                                x<-gsub("\\(","",x);
                                x<-gsub(")","",x);
                                x<-gsub("\\,","",x)}
                                
variable.names$V3<-sapply(variable.names$V2,erase.punctuation)

variable.names$V3<-tolower(variable.names$V3)


###reading activity names

activity.names<-read.table(paste0("./data/",dataset.info[1,1]))


###test data set
####main data set for test

xtest<-read.table(paste0("./data/",dataset.info[17,1])) 

####changing variable names per features

names(xtest)<-variable.names$V3

####incorporating ADL info

activity<-read.table(paste0("./data/",dataset.info[18,1]))

names(activity)<-"activity"

####incorporating subject info

subject<-read.table(paste0("./data/",dataset.info[16,1]))

names(subject)<-"subject"


#####creating a variable to further distinguish between test and train data set after merge
use<-rep_len("test",length.out = nrow(xtest))

names(use)<-"use"


##creating the test dataset by adding columns (variables)
xtest<-cbind(xtest,activity)

xtest<-cbind(xtest,subject)

xtest<-cbind(xtest,use)



##training data set

####main data set for train
xtrain<-read.table(paste0("./data/",dataset.info[31,1])) 

####changing variable names per features

names(xtrain)<-variable.names$V3

#incorporating ADL info

activitytrain<-read.table(paste0("./data/",dataset.info[32,1]))

names(activitytrain)<-"activity"

####incorporating subject info

subjecttrain<-read.table(paste0("./data/",dataset.info[30,1]))

names(subjecttrain)<-"subject"

####creating a variable to further distinguish between test and train data set after merge

use<-rep_len("train",length.out = nrow(xtrain))

names(use)<-"use"

####creating the test dataset by adding columns (variables)
xtrain<-cbind(xtrain,activitytrain)

xtrain<-cbind(xtrain,subjecttrain)

xtrain<-cbind(xtrain,use)


##(1) Merges the training and the test sets to create one data set.
both data sets have the same variable but refer to different subjects
 rbind will apply perfectly for the purpose

HARdataset<-rbind(xtest,xtrain)

###erasing temp vectors and dataframes
rm(activity)

rm(activitytrain)

rm(subject)

rm(subjecttrain)

rm(use)

rm(xtest)

rm(xtrain)


##(2) extract only the measurements on the mean and standard deviation for each measurement
selection of variables whith mean or std in their names and the 3 variables created by me

##creating a vector with the indexes of the appropiate columns as they incorporate mean and std in their names
plus the 3 indexes of the variables I have created
variablesaescoger<-which(grepl("mean",names(HARdataset)) | 
                        grepl("std",names(HARdataset)) |
                        grepl("activity",names(HARdataset)) |
                        grepl("subject",names(HARdataset)) |
                        grepl("use",names(HARdataset))
                        )

####subsetting
                        
HARsubset<-HARdataset[,variablesaescoger]

####erase temporary vector
rm(variablesaescoger) 

##(3)use descriptive ativity names to name the activities in the data set

#### I have used the information of activity names in data frame to transform
column-variable activity of the data frame in a factor with appropiates labels

HARsubset$activity<-factor(as.character(HARsubset$activity), 
                           levels = activity.names$V1, 
                           labels=activity.names$V2)

##(5) - From the data set in step 4, creates a second, 
independent tidy data set with the average of each variable 
for each activity and each subject.

####I will use the dplyr package
library("dplyr") 

#### create the new data frame
HARaverage.subset<-HARsubset 

####putting data in the new data set  with the average of each variable 
for each activity and each subject

HARaverage.subset<-HARsubset %>% group_by(activity,subject) %>% 
        summarise_each_(funs(mean),names(HARaverage.subset)[1:85]) 
        
#recording the new data set
write.table(HARdataset,file = "./data/HARdataset.txt")
write.table(HARsubset,file = "./data/HARsubset.txt")
write.table(HARaverage.subset,file = "./data/HARaverage.txt")
write.table(variable.names ,file = "./data/features.txt")



