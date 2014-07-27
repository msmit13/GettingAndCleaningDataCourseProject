setwd("C:/Users/msmit13/Documents/Admin/Data Science Courses/Class 3 Cleansing Data/Course Project")
training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# Read features and make the feature names better suited for R with some substitutions
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

## Merges the training and the test sets to create one data set
allData = rbind(training, testing)

## Extracts only the measurements on the mean and standard deviation for each measurement
colsWeWant <- grep(".*Mean.*|.*Std.*", features[,2])
features <- features[colsWeWant,]
colsWeWant <- c(colsWeWant, 562, 563)
allData <- allData[,colsWeWant]
## Use descriptive activity names to name the activities in the data set
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
  currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)

## Create a second, independent tidy data set with the average of each variable
tidy = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)
tidy[,90] = NULL
tidy[,89] = NULL
write.table(tidy, "tidy.txt", sep="\t")
