#run_analysis.R

#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Here are the data for the project:

#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#You should create one R script called run_analysis.R that does the following.

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Good luck!

#1
combine_data <- function(data_dir='UCI HAR Dataset') {
  
  #Read test and train files
    
  test.labels <- read.table(paste0(data_dir,"/test/y_test.txt"), col.names="label")
  test.subjects <- read.table(paste0(data_dir,"/test/subject_test.txt"), col.names="subject")
  test.data <- read.table(paste0(data_dir,"/test/X_test.txt"))
  train.labels <- read.table(paste0(data_dir,"/train/y_train.txt"), col.names="label")
  train.subjects <- read.table(paste0(data_dir,"/train/subject_train.txt"), col.names="subject")
  train.data <- read.table(paste0(data_dir,"/train/X_train.txt"))
  
  #merging the data files together
  
  merge.test <- cbind(test.subjects, test.labels, test.data)
  merge.training <- cbind(train.subjects, train.labels, train.data)
  merge.data <- rbind(merge.test, merge.training)
  
  return(merge.data)
}

#2
#argument should be your output from combine_data
mean.std <- function(merge.data) {
  col_mean <- sapply(merge.data, mean)
  col_sd <- sapply(merge.data, sd)
  measurements <- data.table(mean=col_mean, SD=col_sd)
  return(measurements)
}


#3
#use the data from the first function as your data argument
activities <- function(merge.data, data_dir='UCI HAR Dataset') {
  activity_names <- read.table(paste0(data_dir, "/activity_labels.txt"))
  colnames(activity_names) <- c("label", "activity")
  merge_names <- merge(activity_names, merge.data, by = "label")
  return(merge_names)
  
}

#4 
#use the data from #3 as your first argument
label.names <- function(merged_names, data_dir='UCI HAR Dataset') {
  features.names <- read.table(paste0(data_dir, "/features.txt"),stringsAsFactors=FALSE)
  features.names <- features.names$V2
  features.names <- c("label", "activity", "subject", features.names)
  colnames(merged_names) <- features.names
  return(merged_names)

}

#this runs functions 1,3, and 4 and gives you the #4 output to use for #5
get.labeled.data <- function(data_dir='UCI HAR Dataset') {
  combined.data <- combine_data()
  combined.data <- activities(combined.data)
  combined.data <- label.names(combined.data)
  valid_column_names <- make.names(names=names(d), unique=TRUE, allow_=TRUE)
  names(combined.data) <- valid_column_names
  return(combined.data)
}

#5
#argument is the #4 data output
tidy.data <- function(merged_names=NULL, data_dir='UCI HAR Dataset') {
  if(is.null(merged_names)) {
    merged_names=get.labeled.data(data_dir)
  }
  by_activity <- group_by(merged_names, activity, subject) %>%
                 summarise_each(funs(mean))
  return(by_activity)
  
}  
  



