# Getting-and-Cleaning-Data-Course-Project
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones, and the data for the project from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Purpose of the Project
To collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. Following is submitted in addition to this process note-
1) a tidy data set as described below, 
2) a link to a Github repository with the script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work that was performed to clean up the data, called CodeBook.md. 

### The process followed was as detailed below

#### Libraries used

library(plyr)

library(dplyr)

library(data.table)


#### to download the zip folder in temp folder & then unzip

temp <- tempfile()

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", temp)

unzip(temp, list = TRUE)

ytest <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))

xtest <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))

SubjectTest <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))

ytrain <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))

xtrain <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))

SubTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))

features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))

unlink(temp)

### Data Cleaning

colnames(xtrain) <- t(features[2])

colnames(xtest) <- t(features[2])

## merging x & y train as no common ID

xtrain$activities <- ytrain[, 1]

xtrain$participants <- SubTrain[, 1]

xtest$activities <- ytest[, 1]

xtest$participants <- SubjectTest[, 1]

#### Assignment Tasks – 1 to 5

####1. Merges the training and the test sets to create one data set.

maindata <- rbind(xtrain, xtest)

duplicated(colnames(maindata))

maindata <- maindata[, !duplicated(colnames(maindata))]

#### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean <- grep("mean()", names(maindata), value = FALSE, fixed = TRUE)

mean <- append(mean, 471:477)

InstrumentmeanMatrix <- maindata[mean]

#### for std

std <- grep("std()", names(maindata), value = FALSE)

InstrumentsdMatrix <- maindata[std]

#### 3. Uses descriptive activity names to name the activities in the data set

##### changing classs for replacing strings

maindata$activities <- as.character(maindata$activities)

maindata$activities[maindata$activities == 1] <- "Walking"

maindata$activities[maindata$activities == 2] <- "Walking Upstairs"

maindata$activities[maindata$activities == 3] <- "Walking Downstairs"

maindata$activities[maindata$activities == 4] <- "Sitting"

maindata$activities[maindata$activities == 5] <- "Standing"

maindata$activities[maindata$activities == 6] <- "Laying"

maindata$activities <- as.factor(maindata$activities)

#### 4. Appropriately labels the data set with descriptive variable names.

##### check data
names(maindata)

#### Sample below of the output
[1] "tBodyAcc-mean()-X"    "tBodyAcc-mean()-Y"       "tBodyAcc-mean()-Z"                    
[4] "tBodyAcc-std()-X"     "tBodyAcc-std()-Y"        "tBodyAcc-std()-Z"                    
[7] "tBodyAcc-mad()-X"     "tBodyAcc-mad()-Y"        "tBodyAcc-mad()-Z" …

#### use gsub to replace list of abbreviations with clear extensions

names(maindata) <- gsub("Acc", "Accelerometer", names(maindata))

names(maindata) <- gsub("Mag", "Magnitude", names(maindata))

names(maindata) <- gsub("Gyro", "Gyroscope", names(maindata))

names(maindata) <- gsub("^t", "time", names(maindata))

names(maindata) <- gsub("^f", "frequency", names(maindata))

names(maindata) <- gsub("tBody", "TimeBody", names(maindata))

names(maindata) <-gsub("-mean()", "Mean", names(maindata), ignore.case = TRUE)

names(maindata) <-gsub("-std()", "STD", names(maindata), ignore.case = TRUE)

names(maindata) <-gsub("-freq()", "Frequency", names(maindata), ignore.case = TRUE)

names(maindata) <-gsub("angle", "Angle", names(maindata))

names(maindata) <-gsub("gravity", "Gravity", names(maindata))

#### properly label participants' names

maindata$participants <- as.character(maindata$participants)

maindata$participants[maindata$participants == 1] <- "Participant 1"

maindata$participants[maindata$participants == 2] <- "Participant 2"

maindata$participants[maindata$participants == 3] <- "Participant 3"

maindata$participants[maindata$participants == 4] <- "Participant 4"

maindata$participants[maindata$participants == 5] <- "Participant 5"

maindata$participants[maindata$participants == 6] <- "Participant 6"

maindata$participants[maindata$participants == 7] <- "Participant 7"

maindata$participants[maindata$participants == 8] <- "Participant 8"

maindata$participants[maindata$participants == 9] <- "Participant 9"

maindata$participants[maindata$participants == 10] <- "Participant 10"

maindata$participants[maindata$participants == 11] <- "Participant 11"

maindata$participants[maindata$participants == 12] <- "Participant 12"

maindata$participants[maindata$participants == 13] <- "Participant 13"

maindata$participants[maindata$participants == 14] <- "Participant 14"

maindata$participants[maindata$participants == 15] <- "Participant 15"

maindata$participants[maindata$participants == 16] <- "Participant 16"

maindata$participants[maindata$participants == 17] <- "Participant 17"

maindata$participants[maindata$participants == 18] <- "Participant 18"

maindata$participants[maindata$participants == 19] <- "Participant 19"

maindata$participants[maindata$participants == 20] <- "Participant 20"

maindata$participants[maindata$participants == 21] <- "Participant 21"

maindata$participants[maindata$participants == 22] <- "Participant 22"

maindata$participants[maindata$participants == 23] <- "Participant 23"

maindata$participants[maindata$participants == 24] <- "Participant 24"

maindata$participants[maindata$participants == 25] <- "Participant 25"

maindata$participants[maindata$participants == 26] <- "Participant 26"

maindata$participants[maindata$participants == 27] <- "Participant 27"

maindata$participants[maindata$participants == 28] <- "Participant 28"

maindata$participants[maindata$participants == 29] <- "Participant 29"

maindata$participants[maindata$participants == 30] <- "Participant 30"

maindata$participants <- as.factor(maindata$participants)

#### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

maindata.dt <- data.table(maindata)

#### take the average of every column by participants & activities 

TidyData <- maindata.dt[, lapply(.SD, mean), by = 'participants,activities']

write.table(TidyData, file = "Tidy.txt", row.names = FALSE)

#### TidyData obtained, Codebook.md & Tidy.txt file submitted




