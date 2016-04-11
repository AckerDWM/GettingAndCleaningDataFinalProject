path <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(path, "temp.zip")
unzip("temp.zip", exdir = "./")
file.remove("temp.zip")

X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

features <- read.table("UCI HAR Dataset/features.txt")

idx.means.and.stds <- grep("mean\\(\\)|std\\(\\)", features$V2)
X.means.and.stds <- X[idx.means.and.stds]
names(X.means.and.stds) <- features$V2[idx.means.and.stds]
X.means.and.stds$subject <- subject$V1
X.means.and.stds$activity <- y$V1

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
X.means.and.stds$activity <- factor(
  X.means.and.stds$activity,
  levels=activities$V1,
  labels=as.character(activities$V2))

existing.labels <- c("^t", "^f", "Acc", "Gyro", "Mag", "BodyBody")
replacement.labels <- c("Time", "Frequency", "Accelerometer", 
                        "Gyroscope", "Magnitude", "Body")
for (i in 1:6) {
  colnames(X.means.and.stds) <- gsub(
    existing.labels[i], 
    replacement.labels[i], 
    colnames(X.means.and.stds))
}

tidy.data <- aggregate(
  X.means.and.stds[,1:66],
  by=list(X.means.and.stds$activity, X.means.and.stds$subject), 
  mean)
colnames(tidy.data)[1:2] <- c("activity", "subject")

write.table(tidy.data, "tidy_data.txt", row.names=FALSE)








