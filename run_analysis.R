##Load "dplyr" package
library(dplyr)
filename <- "Coursera_dt.zip"

## Check if archieve already exists.
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="curl")
}  

## Check if folder exists
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}

##Assign data frames

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
##merge datasets
xd <- rbind(x_train, x_test)
yd <- rbind(y_train, y_test)
subjectd <- rbind(subject_train, subject_test)
mergedd <- cbind(subjectd, yd, xd)

##extraxt mean & sd measurements
tidy_dt <- mergedd %>% select(subject, code, contains("mean"), contains("std"))

##Use activity names to name to activaties
tidy_dt$code <- activities[tidy_dt$code, 2]

##update the labels in data set
names(tidy_dt)[2] = "activity"
names(tidy_dt)<-gsub("Acc", "Accelerometer", names(tidy_dt))
names(tidy_dt)<-gsub("Gyro", "Gyroscope", names(tidy_dt))
names(tidy_dt)<-gsub("BodyBody", "Body", names(tidy_dt))
names(tidy_dt)<-gsub("Mag", "Magnitude", names(tidy_dt))
names(tidy_dt)<-gsub("^t", "Time", names(tidy_dt))
names(tidy_dt)<-gsub("^f", "Frequency", names(tidy_dt))
names(tidy_dt)<-gsub("tBody", "TimeBody", names(tidy_dt))
names(tidy_dt)<-gsub("-mean()", "Mean", names(tidy_dt), ignore.case = TRUE)
names(tidy_dt)<-gsub("-std()", "STD", names(tidy_dt), ignore.case = TRUE)
names(tidy_dt)<-gsub("-freq()", "Frequency", names(tidy_dt), ignore.case = TRUE)
names(tidy_dt)<-gsub("angle", "Angle", names(tidy_dt))
names(tidy_dt)<-gsub("gravity", "Gravity", names(tidy_dt))

##Create a new data set with the average of each variable for each activity and each subject
dt2 <- tidy_dt %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(dt2, "dt2.txt", row.name=FALSE)

##Check variable names

str(dt2)