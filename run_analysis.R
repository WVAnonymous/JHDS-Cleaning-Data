#1. Merges the training and the test sets to create one data set.
# set the working directory to the UCI HAR dataset folder with test and train as the subfolders
# before executing the script.

#load the files
  X_test        <- read.table("test/X_test.txt",header = FALSE)
  subject_test  <- read.table("test/subject_test.txt",header = FALSE)
  y_test        <- read.table("test/y_test.txt",header = FALSE)
  X_train       <- read.table("train/X_train.txt",header = FALSE)
  subject_train <- read.table("train/subject_train.txt",header = FALSE)
  y_train       <- read.table("train/y_train.txt",header = FALSE)

#merge the test files
  test_merge    <-  cbind(X_test,subject_test,y_test) 
  train_merge   <-  cbind(X_train,subject_train,y_train) 
  full_merge    <-  rbind(test_merge,train_merge) 


#2. Extracts only the measurements with the mean and standard deviation for each measurement. 
  
  hw_col_nums   <- c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,
                     121,122,123,124,125,126,161,162,163,164,165,166,
                     201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,
                     345,346,347,348,349,350,
                     424,425,426,427,428,429,
                     503,504,516,517,529,530,542,543,562,563)
  hw_set <- full_merge[,hw_col_nums]
  
  
#3. Uses descriptive activity names to name the activities in the data set
#   most efficient storage practice is to explicitly name the factor levels, not rename in dataset
  
  base_names      <- read.table("features.txt",header = FALSE)
  base_names      <- c(as.character(base_names[,2]),"subject","activity")
  hw_col_names    <- base_names[hw_col_nums]
  
  colnames(hw_set) <- hw_col_names
  levels(hw_set$activity) <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")


  
  #4. Appropriately labels the data set with descriptive variable names. 
  
  new_col_names <- tolower(hw_col_names)
  new_col_names <- gsub("[^[:alpha:]]", "", new_col_names)
  new_col_names <- gsub("acc"," acceleration ",new_col_names)
  new_col_names <- gsub("mean"," mean ", new_col_names)
  new_col_names <- gsub("gyro"," gyroscope ", new_col_names)
  new_col_names <- gsub("jerk"," jerk ", new_col_names)
  new_col_names <- gsub("std"," standard deviation ", new_col_names)
  new_col_names <- gsub("bodybody","body ", new_col_names)
  new_col_names <- gsub("mag"," magnitude ", new_col_names)
  new_col_names <- gsub("  "," ", new_col_names)
  
  colnames(hw_set) <- new_col_names

  #5. From the data set in step 4, creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject.
  
  tidy_data <- melt(hw_set, id.vars = c("subject","activity"))
  tidy_set <- dcast(tidy_data,  subject + activity ~ variable , mean)
  write.table(tidy_set, "tidy_set.txt", row.name = FALSE)
  
