setwd("~/projects/Course3/assign/UCI HAR Dataset")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")


## merge x_train, y_train, subject_train
read_train <- function() {
        X_train <- tbl_df(read.table(file = "train/X_train.txt"))
        subject_train <- read.table(file = "train/subject_train.txt")
        y_train <- read.table(file = "train/y_train.txt")
        X_train <<-
                mutate(X_train,
                       subject_id = subject_train$V1,
                       activity_id = y_train$V1)
}

## merge x_test, y_test, subject_test
read_test <- function() {
        X_test <- tbl_df(read.table(file = "test/X_test.txt"))
        subject_test <- read.table(file = "test/subject_test.txt")
        y_test <- read.table(file = "test/y_test.txt")
        X_test <<-
                mutate(X_test,
                       subject_id = subject_test$V1,
                       activity_id = y_test$V1)
}

## merge x_train, x_test
merge_df <- function() {
        merged_df <<- tbl_df(bind_rows(X_train, X_test))
}


## read features and select mean and std
read_features <- function() {
        features <- tbl_df(read.table(file = "features.txt"))
        features <<- features[grep("*mean*|*std*", features$V2), ]
}

## read activities and assign rownames
read_activities <- function() {
        activities <<- tbl_df(read.table(file = "activity_labels.txt"))
        names(activities) <<- c("activity_id", "activity_name")
}


## read activities and assign rownames
make_final_df <- function(){
        final_df <- select(merged_df, features$V1, subject_id, activity_id)
        final_df <- setNames(final_df, c(as.character(features$V2), "subject_id", "activity_id"))
        final_df <<- full_join(final_df, activities)
}

## change variable names to correct 
correct_names <- function(){
        names(final_df) <- tolower(names(final_df))
        names(final_df) <- gsub("-", "", names(final_df))
        names(final_df) <- gsub("_", "", names(final_df))
        names(final_df) <- sub("\\()", "", names(final_df))
        names(final_df) <- sub("\\()", "", names(final_df))
        names(final_df) <- sub("^t", "time", names(final_df))
        names(final_df) <<- sub("^f", "freq", names(final_df))
}


## create independent tidy data set with the average of each variable for each activity and each subject

make_final_df2 <- function() {
        final_df2 <- group_by(final_df, activityname, subjectid)
        final_df2 <<- summarise_all(final_df2, mean)
}

