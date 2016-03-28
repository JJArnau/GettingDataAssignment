run_analysis <- function()
{
  library(plyr)
  
  #Set my Working Directory
  setwd("C:/Users/arnau.jj/Documents/3. Career/3. Training & Certification/1. Coursera/2. Getting and Cleaning Data")
  
  #Download file from link
  URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  fileName <- "./data/getdataW4Assignment.zip"
  #file <- download.file(URL, destfile = fileName)
  
  #Unzip file and save into specific folder
  unzip(fileName, exdir = "./data")
  
  #Step 1: Merges the training and the test sets to create one data set.
  
  #Read files as data tables 
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  
  #Merge data tables
  x_data <- rbind(x_train, x_test)
  y_data <- rbind(y_train, y_test)
  subject_data <- rbind(subject_train, subject_test)
  
  #Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #Read Features file
  features <- read.table("./data/UCI HAR Dataset/features.txt")
  
  #Read the columns with "mean" or "std" in their names.
  mean_std_features <- grep("-(mean|std)\\(\\)", features[, 2])
  
  #Subset the columns and add the "right" names
  x_data <- x_data[, mean_std_features]
  names(x_data) <- features[mean_std_features, 2]
  
  #Step 3: Uses descriptive activity names to name the activities in the data set
  
  #Read Activity Labels File
  activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
  
  # Update value with "right" activity name and change column name
  y_data[, 1] <- activities[y_data[, 1], 2]
  names(y_data) <- "Activity"
  
  #Step 4: Appropriately labels the data set with descriptive variable names.
  
  #Add "right" name
  names(subject_data) <- "Subject"
  
  #Merge all data tables
  Full_data <- cbind(x_data, y_data, subject_data)
  
  # Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  averages <- ddply(Full_data, .(Subject, Activity), function(x) colMeans(x[, 1:66]))
  write.table(averages, "./data/averagesOutputdata.txt", row.name=FALSE)
}