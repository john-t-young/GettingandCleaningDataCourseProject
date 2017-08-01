## import Libraries
library(data.table)
library(reshape2)
library(dplyr)

## Loading train data
x_train <- fread("./UCI HAR Dataset/train/X_train.txt")
y_train <- fread("./UCI HAR Dataset/train/Y_train.txt")
subject_test <- fread("./UCI HAR Dataset/test/subject_test.txt")

## Loading test data
x_test <- fread("./UCI HAR Dataset/test/X_test.txt")
y_test <- fread("./UCI HAR Dataset/test/Y_test.txt")
subject_train <- fread("./UCI HAR Dataset/train/subject_train.txt")

#Features and activity data loading
features <- fread("./UCI HAR Dataset/features.txt")
activity_labels <- fread("./UCI HAR Dataset/activity_labels.txt")
setnames(activity_labels, "V1", "Activity")

## 1. Merges the training and the test sets to create one data set.
x_train[,Data := "Train"] ## Adding Train Identifier for Train X Data
x_test[,Data := "Test"] ## Adding Train Identifier for Test X Data
y_train[,Data := "Train"] ## Adding Train Identifier for Train Y Data
y_test[,Data := "Test"] ## Adding Train Identifier for Test Y Data
subject_test[,Data := "Test"] ## Adding Train Identifier for Subject test Data
subject_train[,Data := "Train"] ## Adding Train Identifier for Subject Train Data


merged_data_x <- bind_rows(x_train,x_test) # merging data using dplyr lib
merged_data_y <- bind_rows(y_train,y_test) # merging data using dplyr lib
merged_subject <- bind_rows(subject_test,subject_train) # merging data using dplyr lib

## removing unwanted data from the environment
rm(x_test); rm(x_train); rm(y_test); rm(y_train); # rm(subject_test);rm(subject_train)

## Finding matches of mean and std from the features
matches <- features[V2 %like% "mean|std|Mean|Std"]

## updating column Identifier and limiting data to the identified mean and std columns
matches[,V1:= paste("V",V1, sep = "")]
matchedcolumns <- matches[,V1]
matchedcolumns <- c(matchedcolumns, "Data")
limited_x_data <- merged_data_x[,..matchedcolumns] ## limited columns dataset of x

## Assuming the records in the Y dataset and Subject is in the same order of rows in X.
## merging data x and y
full_dataset <- limited_x_data[, Activity:= merged_data_y[,V1]] # merge x,y
full_dataset <- full_dataset[, Subject:= merged_subject[,V1]] # merge x,y,subject
full_dataset <- as.data.table(left_join(full_dataset,activity_labels, by="Activity", x.all = TRUE, suffix=c("","Y"))) # add activity labels
full_dataset <- full_dataset[,c("Activity", "V1Y"):= NULL]
setnames(full_dataset, "V2Y", "Activity")

matches[,V2:= gsub("\\()",  "", V2)]
matches[,V2:= gsub("\\-",  "", V2)]
setnames(full_dataset, matches$V1, matches$V2) ## dataset that has x,y,labels,activities

full_dataset <- full_dataset %>% group_by(Subject,Activity) %>% summarise_all(mean)

fwrite(full_dataset,"output.txt")


