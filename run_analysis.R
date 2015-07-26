run_analysis <- function(){
    #Merges the training and the test sets to create one data set.
    library(data.table)
    library(reshape2)
    setwd("~/R/Coursera/GettingAndCleaning/Getting-and-Cleaning-Data-Course-Project")
    
    ##Load all of the values into R
    subjectTrain <- fread("UCI HAR Dataset/train/subject_train.txt")
    subjectTest <- fread("UCI HAR Dataset/test/subject_test.txt")
    yTrain <- fread("UCI HAR Dataset/train/y_train.txt")
    yTest <- fread("UCI HAR Dataset/test/y_test.txt")
    train <- data.table(read.table("UCI HAR Dataset/train/X_train.txt"))
    test <- data.table(read.table("UCI HAR Dataset/test/X_test.txt"))
    features <- fread("UCI HAR Dataset/features.txt")
    
    #Merge all loaded values
    subject <- rbind(subjectTrain, subjectTest)
    setNames(subject, "subject")
    activity <- rbind(yTrain, yTest)
    setNames(activity, "activity")
    data <- rbind(train, test)
    setnames(data, features$V2)
    data <- cbind(activity, subject, data)
    
    #Extracts only the measurements on the mean and standard deviation for each measurement. 
    mean_std <- data[,c(1, 2, grep('mean\\(\\)|std\\(\\)', names(data), ignore.case=T)), with=FALSE]
    
    #Uses descriptive activity names to name the activities in the data set
    activityLabels <- fread("UCI HAR Dataset/activity_labels.txt")
    
    library(plyr)
    mean_std$activity <- mapvalues(mean_std$activity, from=activityLabels$V1, to=activityLabels$V2)
    
    #Appropriately labels the data set with descriptive variable names. 
    #I think I did that when I cbinded subject into the dataset while merging all loaded values
    
    #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    dataMelted <- melt(mean_std, id=c("subject","activity"))
    dataCast <- dcast(dataMelted, subject + activity ~ variable, mean)
    write.table(dataCast, "tidy.txt", row.names = FALSE)
}