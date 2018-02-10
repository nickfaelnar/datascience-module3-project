library("data.table")
library("reshape2")

# Prepare Data
## Download dataset
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", file.path(getwd(), "dataFiles.zip"))
## Unzip zipped file
unzip(zipfile = "dataFiles.zip")

# Read and Filter Data
## Read Activity Labels Data
activityLabelsFile <- file.path(getwd(), "UCI HAR Dataset/activity_labels.txt")
activityLabelsData <- fread(activityLabelsFile, col.names = c("activityId", "activityName"))
## Read Features File
featuresFile <- file.path(getwd(), "UCI HAR Dataset/features.txt")
featuresData <- fread(featuresFile, col.names = c("featureId", "featureName"))

## Extracts only the measurements on the mean and standard deviation for each measurement.
## Used "(mean|std)\\(" regex so that we will only retrieve mean and std.
## Added "\\(" to prevent retrieval of meanFreq() and others.
featuresFiltered <- grep("(mean|std)\\(\\)", featuresData[, featureName])
measurementsFiltered <- featuresData[featuresFiltered, featureName]

# Load Train Data
trainXFile <- file.path(getwd(), "UCI HAR Dataset/train/X_train.txt")
trainXData <- fread(trainXFile)
trainXFiltered <- trainXData[, featuresFiltered, with = FALSE]

data.table::setnames(trainXFiltered, colnames(trainXFiltered), measurementsFiltered)

trainYFile <- file.path(getwd(), "UCI HAR Dataset/train/Y_train.txt")
trainYData <- fread(trainYFile, col.names = "activity")

trainSubjectFile <- file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt")
trainSubject <- fread(trainSubjectFile, col.names = c("subject"))

trains <- cbind(trainSubject, trainYData, trainXFiltered)

# Load Test Data
testXFile <- file.path(getwd(), "UCI HAR Dataset/test/X_test.txt")
testXData <- fread(testXFile)
testXFiltered <- testXData[, featuresFiltered, with = FALSE]

data.table::setnames(testXFiltered, colnames(testXFiltered), measurementsFiltered)

testYFile <- file.path(getwd(), "UCI HAR Dataset/test/Y_test.txt")
testYData <- fread(testYFile, col.names = "activity")

testSubjectFile <- file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt")
testSubjectData <- fread(testSubjectFile, col.names = c("subject"))

tests <- cbind(testSubjectData, testYData, testXFiltered)

# Merge Train and Test Data. Replace with actual Activity Names
mergedTrainTestData <- rbind(trains, tests)
mergedTrainTestData[["activity"]] <- factor(mergedTrainTestData[, activity], levels = activityLabelsData[["activityId"]], labels = activityLabelsData[["activityName"]])
mergedTrainTestData[["subject"]] <- as.factor(mergedTrainTestData[, subject])

# Create independent data set and show average for each activity and each subject.
tidyData <- data.table::copy(mergedTrainTestData)
tidyData <- reshape2::melt(data = tidyData, id = c("subject", "activity"))
tidyData <- reshape2::dcast(data = tidyData, subject + activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x = tidyData, file = "tidyData.csv", quote = FALSE)