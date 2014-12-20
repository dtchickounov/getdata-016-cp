library(dplyr)

# Get raw data from text files in work directory
training_labels <- read.table(file='train//y_train.txt', stringsAsFactor = TRUE)
training_set <- read.table(file='train//X_train.txt', stringsAsFactor = FALSE)
training_subject <- read.table(file='train//subject_train.txt', stringsAsFactor = TRUE)
test_labels <- read.table(file='test//y_test.txt', stringsAsFactor = TRUE)
test_set <- read.table(file='test//X_test.txt', stringsAsFactor = FALSE)
test_subject <- read.table(file='test//subject_test.txt', stringsAsFactor = TRUE)

# 1. Merges the training and the test sets to create one data set.
data_set <- rbind(cbind(training_subject, training_labels, training_set), cbind(test_subject, test_labels, test_set))

# 4. Appropriately labels the data set with descriptive variable names. 
features <- read.table(file='features.txt', stringsAsFactor = FALSE)
names(data_set) <- c(c('SubjectId', 'ActivityId'), features[, 2])

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
featureCols <- features[grep('std[()]|mean[()]', features[,2]), 1]
data_set <- data_set[, c(1,2, featureCols + 2)]

# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table(file='activity_labels.txt', stringsAsFactor = FALSE)
data_set <- merge(activity_labels, data_set, by.x = "V1", by.y = "ActivityId")
data_set <- data_set[, -c(1)] ; names(data_set)[1] <- 'ActivityName'

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
dt <- tbl_df(data_set)
tidy <- dt %>% 
        group_by(SubjectId,ActivityName) %>% 
        summarise_each(funs(mean))

write.table(tidy, file="tidy_data_set.txt", row.names=FALSE)