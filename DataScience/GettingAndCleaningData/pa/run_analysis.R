library("dplyr")

## Downloads and extracts UCI HAR dataset to the working directory
download_and_extract_UCI_HAR_dataset <- function() {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile="UCI_HAR_Dataset.zip")
  unzip("UCI_HAR_Dataset.zip", overwrite = TRUE)
}

## Converts a string to camel-case convertion
## Example: my_string_xxx -> MyStringXxx
camel <- function(s) {
  capital_first_letter <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(s, "_"), function(x) paste(capital_first_letter(x), collapse=""))
}

## Prepare cleaned UCI HAR dataset as described in programming assignment:
##
## merged test and training data with descriptive labels and 
## limited to mean and standard deviation for each measurement, ready for further processing
##
## Function expects its data from "UCI HAR Dataset" directory under working directory
make_tidy_UCI_HAR_dataset <- function() {
  # Merge X, Y, Subject training and test data
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt", comment.char="")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt", comment.char="")
  x_all <- rbind(x_test, x_train)
  
  y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
  y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
  y_all <- rbind(y_test, y_train)
  
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  subject_all <- rbind(subject_test, subject_train)
  names(subject_all) <- "Subject"
  
  # Read variables, beautify variable names
  vars <- read.table("UCI HAR Dataset/features.txt", col.names=c("FeatureId", "FeatureName"))$FeatureName
  vars <- gsub("()", "", vars, fixed = TRUE)
  vars <- gsub("-", "_", vars, fixed = TRUE)
  vars <- gsub(",", "_", vars, fixed = TRUE)
  vars <- gsub("(", "_", vars, fixed = TRUE)
  vars <- gsub(")$", "", vars)
  vars <- gsub(")", "", vars)
  vars <- gsub("__", "_", vars, fixed = TRUE)
  vars <- gsub("^t", "Time", vars)
  vars <- gsub("^angle_t", "angle_Time", vars)
  vars <- gsub("^f", "Frequency", vars)
  vars <- gsub("BodyBody", "Body", vars)
  vars <- gsub("Acc", "Accelerometer", vars)
  vars <- gsub("Gyro", "Gyroscope", vars)
  vars <- gsub("Mag", "Magnitude", vars)
  
  names(x_all) <- vars
  
  # Read activities
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                           col.names=c("ActivityId", "ActivityName"),
                           colClasses=c("integer", "character"))
  activities$ActivityName <- sapply(activities$ActivityName, function(x) camel(tolower(x)))
  
  # Transform Y to list of activities
  y_all[, 1] = activities[y_all[, 1], 2]
  names(y_all) <- "Activity"

  # Select only mean and stddev columns
  x_all_mean_std <- x_all[, grep("mean|std", names(x_all), ignore.case=TRUE)]

  # Merge all data in one data frame
  data <- cbind(subject_all, y_all, x_all_mean_std)
  
  data
}

## Make a summary: prepare mean values for every variable for every subject and activity
make_UCI_HAR_dataset_mean_values <- function(data) {
  mean_values <- summarise_each(group_by(data, Subject, Activity), funs(mean))
  write.table(mean_values, file="tidy_data_mean_values.txt", row.name=FALSE)

  mean_values
}
