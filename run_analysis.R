

#creating an especific directory for data
if(!file.exists("data")){
        dir.create("data")
}

#saving urls indicated by the assignment

infoUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.names"
datasetUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
dataProjectUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
htmlinfoUrl<-"http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"

#downloading data and date

download.file(dataProjectUrl,destfile = "./data/dataset.zip",method = "curl")
dataset.downloadingDate<-date() 

#unziping files
unzip("./data/dataset.zip", exdir = "./data")
dataset.info<-unzip("./data/dataset.zip", list = T)

#reading appropiate files & create appropiate data set also
#(4) Appropriately labels the data set with descriptive variable names.

variable.names<-read.table(paste0("./data/",
                        dataset.info[2,1]),
                        stringsAsFactors = F)#reading list of features
#rearrenging variable names
erase.punctuation<-  function(x) {x<-gsub("-","",x);
                                x<-gsub("\\(","",x);
                                x<-gsub(")","",x);
                                x<-gsub("\\,","",x)}
variable.names$V3<-sapply(variable.names$V2,erase.punctuation)
variable.names$V3<-tolower(variable.names$V3)

#reading activity names
activity.names<-read.table(paste0("./data/",dataset.info[1,1]))


#test data set

xtest<-read.table(paste0("./data/",dataset.info[17,1])) #main data set for test
names(xtest)<-variable.names$V3#changing variable names per features

activity<-read.table(paste0("./data/",dataset.info[18,1]))#incorporating ADL info
names(activity)<-"activity"

subject<-read.table(paste0("./data/",dataset.info[16,1]))#incorporating subject info
names(subject)<-"subject"

#creating a variable to further distinguish between test and train data set after merge
use<-rep_len("test",length.out = nrow(xtest))
names(use)<-"use"

#creating the test dataset by adding columns (variables)
xtest<-cbind(xtest,activity)
xtest<-cbind(xtest,subject)
xtest<-cbind(xtest,use)


#training data set

xtrain<-read.table(paste0("./data/",dataset.info[31,1])) #main data set for train
names(xtrain)<-variable.names$V3#changing variable names per features

activitytrain<-read.table(paste0("./data/",dataset.info[32,1]))#incorporating ADL info
names(activitytrain)<-"activity"


subjecttrain<-read.table(paste0("./data/",dataset.info[30,1]))#incorporating subject info
names(subjecttrain)<-"subject"

#creating a variable to further distinguish between test and train data set after merge
use<-rep_len("train",length.out = nrow(xtrain))
names(use)<-"use"
#creating the test dataset by adding columns (variables)
xtrain<-cbind(xtrain,activitytrain)
xtrain<-cbind(xtrain,subjecttrain)
xtrain<-cbind(xtrain,use)


# (1) Merges the training and the test sets to create one data set.
#both data sets have the same variable but refer to different subjects
# rbind will apply perfectly for the purpose

HARdataset<-rbind(xtest,xtrain)

#erasing temp vectors and dataframes
rm(activity)
rm(activitytrain)
rm(subject)
rm(subjecttrain)
rm(use)
rm(xtest)
rm(xtrain)
rm(variable.names)

#(2) extract only the measurements on the mean and standard deviation for each measurement
#selection of variables whith mean or std in their names and the 3 variables created by me

variablesaescoger<-which(grepl("mean",names(HARdataset)) | 
                        grepl("std",names(HARdataset)) |
                        grepl("activity",names(HARdataset)) |
                        grepl("subject",names(HARdataset)) |
                        grepl("use",names(HARdataset))
                        )
HARsubset<-HARdataset[,variablesaescoger]

#(3)use descriptive ativity names to name the activities in the data set

HARsubset$activity<-factor(as.character(HARsubset$activity), 
                           levels = activity.names$V1, 
                           labels=activity.names$V2)

#(5) - From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.
library("dplyr") # library to use
HARaverage.subset<-HARsubset # create the new data frame

#obtaining a new data set with with the average of each variable 
#for each activity and each subject

HARaverage.subset<-HARsubset %>% group_by(activity,subject) %>% 
        summarise_each_(funs(mean),names(HARaverage.subset)[1:85]) 
        


