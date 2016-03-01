########################################################################################
####### 1. Merges the training and the test sets to create one data set.
setwd('C:/Users/LVARGAS/Documents/MEGA/Cursera/Getting and Cleaning Data/Week 4')

if(dir.exists('./data')){
        print('The directory exist!!')
}else{
        dir.create('./data')
        print('The directory has ben created!!')
}

# Download the file
dataUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip'
download.file(dataUrl, destfile = './data/dataProject.zip')

# Unzip the file
unzip('./data/dataProject.zip', exdir = './data/dataProject')

# Get the files list
path <- file.path('./data', 'UCI HAR Dataset')
files <- list.files(path, recursive = TRUE)
files

# Funcion for read data
readFile <- function(dirName, fileName){
        read.table(file.path(path, dirName, fileName),header = FALSE)
}

# Read data and assign them to an object
dataTest_x <- readFile('test', 'X_test.txt')
str(dataTest_x)
dataTrain_x <- readFile('train', 'X_train.txt')
str(dataTrain_x)
dataTest_y <- readFile('test', 'y_test.txt')
str(dataTest_y)
dataTrain_y <- readFile('train', 'y_train.txt')
str(dataTrain_y)
dataSubjectTrain <- readFile('train', 'subject_train.txt')
str(dataSubjectTrain)
dataSubjectTest <- readFile('test', 'subject_test.txt')
str(dataSubjectTest)

# Concatenate train and test tables by rowns
data_x <- rbind(dataTest_x, dataTrain_x)
data_y <- rbind(dataTest_y, dataTrain_y)
dataSubject <- rbind(dataSubjectTest, dataSubjectTrain)

# Set names to variables
data_x_names <- read.table(file.path(path, 'features.txt'), header = FALSE)
names(data_x) <- data_x_names$V2
head(data_x, 2)
names(data_y) <- ('activity')
head(data_y)
names(dataSubject) <- ('subject')
head(dataSubject)

# Join all data
data <- cbind(dataSubject, data_y, data_x)
head(data,2)
names(data)

########################################################################################
####### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Get the variables names containing the words ''mean" or "std"
data_x_namesMeanStd <- data_x_names$V2[grep('mean\\(\\)|std\\(\\)',data_x_names$V2) ]
length(data_x_namesMeanStd)

# Get the subset of data that containig the names with "mean" or "std"
varMeanStd <- c(as.character(data_x_namesMeanStd), "subject", "activity" )
data2 <- subset(data, select=varMeanStd)
str(data2,2)
names(data2)

########################################################################################
####### 3. Uses descriptive activity names to name the activities in the data set

# Read the file with the descriptive names for activities, using the "readFile()" fuction
labelsActivity <- readFile('', 'activity_labels.txt')

# Write the descriptive labels for activities
data2$activity <- factor(data2$activity, levels = labelsActivity$V1, labels = labelsActivity$V2)
str(data2)

########################################################################################
####### 4. Appropriately labels the data set with descriptive variable names.

names(data2)<-gsub("^t", "time", names(data2))
names(data2)<-gsub("^f", "frequency", names(data2))
names(data2)<-gsub("Acc", "Accelerometer", names(data2))
names(data2)<-gsub("Gyro", "Gyroscope", names(data2))
names(data2)<-gsub("Mag", "Magnitude", names(data2))
names(data2)<-gsub("BodyBody", "Body", names(data2))

########################################################################################
####### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data3 <- data2 %>% group_by(subject, activity) %>% summarise_each(funs(mean))
str(data2)

write.table(data3, file = "tidydata.txt",row.name=FALSE)

install.packages('memisc')
library(memisc)
description(data3)

codebook(data3)
Write(descrition(data3),
      file="Data-desc.txt")
Write(codebook(data3),
      file="Data-cdbk.txt")
