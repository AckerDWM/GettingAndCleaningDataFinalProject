# Download and unzip data
path <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(path, "temp.zip")
unzip("temp.zip", exdir = "./")
file.remove("temp.zip")

# Load test data
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Load training data
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

# Combine test and training datasets
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

# Load feature names
features <- read.table("UCI HAR Dataset/features.txt")

# Isolate means and standard deviations features from combined data
idx.means.and.stds <- grep("mean\\(\\)|std\\(\\)", features$V2)
X.means.and.stds <- X[idx.means.and.stds]
names(X.means.and.stds) <- features$V2[idx.means.and.stds]
X.means.and.stds$subject <- subject$V1
X.means.and.stds$activity <- y$V1

# Load activity names and use them to turn factorize the activity column
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
X.means.and.stds$activity <- factor(
  X.means.and.stds$activity,
  levels=activities$V1,
  labels=as.character(activities$V2))

# rename columns so that they are more readable
existing.labels <- c("^t", "^f", "Acc", "Gyro", "Mag", "BodyBody")
replacement.labels <- c("Time", "Frequency", "Accelerometer", 
                        "Gyroscope", "Magnitude", "Body")
for (i in 1:6) {
  colnames(X.means.and.stds) <- gsub(
    existing.labels[i], 
    replacement.labels[i], 
    colnames(X.means.and.stds))
}

# summarize data, taking means by activity and subject
tidy.data <- aggregate(
  X.means.and.stds[,1:66],
  by=list(X.means.and.stds$activity, X.means.and.stds$subject), 
  mean)
colnames(tidy.data)[1:2] <- c("activity", "subject")

# write the summarized data to a file
write.table(tidy.data, "tidy_data.txt", row.names=FALSE)

require(dplyr)
require(tidyr)
tidy <- tbl_df(tidy.data)

tidier <- as.data.frame(matrix(nrow=180*66, ncol=10))
colnames(tidier) <- c('Subject', 'Activity', 'Domain', 'Signal',
                      'Instrument', 'Variable', 'Magnitude', 
                      "Jerk", 'Direction', 'Average')

for (i in 3:68) {
  tidier.range <- (((i - 3)* 180 + 1) : ((i - 2) * 180))
  name <- colnames(tidy)[i]
  
  if (grepl("Frequency", name)) {
    tidier$Domain[tidier.range] <- "Frequency"
  } else {
    tidier$Domain[tidier.range] <- "Time"
  }
  
  if (grepl("Body", name)) {
    tidier$Signal[tidier.range] <- "Body"
  } else {
    tidier$Signal[tidier.range] <- "Gravity"
  }
  
  if (grepl("Accelerometer", name)) {
    tidier$Instrument[tidier.range] <- "Accelerometer"
  } else {
    tidier$Instrument[tidier.range] <- "Gyroscope"
  }
  
  if (grepl("mean", name)) {
    tidier$Variable[tidier.range] <- "Mean"
  } else {
    tidier$Variable[tidier.range] <- "Standard.deviation"
  }
  
  if (grepl("Jerk", name)) {
    tidier$Jerk[tidier.range] <- TRUE
  } else {
    tidier$Jerk[tidier.range] <- FALSE
  }
  
  if (grepl("Magnitude", name)) {
    tidier$Magnitude[tidier.range] <- TRUE
  } else {
    tidier$Magnitude[tidier.range] <- FALSE
  }
  
  if (grepl("-Z", name)) {
    tidier$Direction[tidier.range] <- "Z"
  } else if (grepl("-X", name)) {
    tidier$Direction[tidier.range] <- "X"
  } else if (grepl("-Y", name)) {
    tidier$Direction[tidier.range] <- "Y"
  }
  
  tidier$Subject[tidier.range] <- tidy$subject
  tidier$Activity[tidier.range] <- tidy$activity
  tidier$Average[tidier.range] <- tidy[name][[1]]
}







