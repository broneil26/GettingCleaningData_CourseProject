## 1. Merges the training and the test sets to create one data set.
## Read data and label files into R.
test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
testLabel <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
trainLabel <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
testSubject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
trainSubject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

## Combine files
moveData <- rbind(train, test)
comboLabel <- rbind(trainLabel, testLabel)
comboSubject <- rbind(trainSubject, testSubject)
names(comboLabel) <- "activity"
names(comboSubject) <- "subject"

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## Keep features needed
labels <- read.table("./data/UCI HAR Dataset/features.txt")
meanstd <- grep("mean\\(\\)|std\\(\\)", labels[, 2])

## Keep data from combined set with required features (mean, std only)
moveData <- moveData[,meanstd]
names(moveData) <- gsub("\\(\\)", "", labels[meanstd, 2])

## 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
activityLabel <- activity[comboLabel[, 1], 2]


## 4. Appropriately labels the data set with descriptive variable names.
mergeData <- cbind(comboSubject, activityLabel, moveData)
## write out data file if desired
## write.table(mergeData, "merged_data.txt",row.name = FALSE)

## ##5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(mergeData,by=list(mergeData$subject, mergeData$activityLabel)
                       , FUN=mean, na.rn=TRUE)
        ##Sort by Subject and Activity
        tidyData <- tidyData[order(tidyData$Group.1, tidyData$Group.2),]
        
        ##Drop un-needed variables created by last step
        tidyData$subject <- tidyData$activityLabel <-NULL

        ##Fix labels for subject and activity
        library(reshape)
        tidyData <- rename(tidyData, c(Group.1="subject",Group.2="activity"))

# write the 2nd dataset
write.table(tidyData, "tidyData.txt", row.name = FALSE) 
