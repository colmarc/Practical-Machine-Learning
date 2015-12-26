library(caret) #ML library
library(rattle) #Visualization library

set.seed(11111)



Url_train <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
Url_test <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(Url_train), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(Url_test), na.strings=c("NA","#DIV/0!",""))


inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]



low_var_variable <- nearZeroVar(myTraining, saveMetrics=TRUE)
low_var_variables <- names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                              "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                              "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                              "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                              "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                              "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                              "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                              "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                              "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                              "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                              "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                              "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                              "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                              "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                              "stddev_yaw_forearm", "var_yaw_forearm")
myTraining <- myTraining[!low_var_variables]
myTraining <- myTraining[c(-1)]


training_loop <- myTraining #creating another subset to iterate in loop
for(i in 1:length(myTraining)) { #for every column in the training dataset
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(training_loop)) {
      if( length( grep(names(myTraining[i]), names(training_loop)[j]) ) ==1)  { #if the columns are the same:
        training_loop <- training_loop[ , -j] #Remove that column
      }   
    } 
  }
}

myTraining <- training_loop


columns_1 <- colnames(myTraining)
columns_2 <- colnames(myTraining[, -58]) #already with classe column removed
myTesting <- myTesting[columns_1]
testing <- testing[columns_2]

for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}



tree <- train(classe ~ ., data=myTraining, method="rpart")
fancyRpartPlot(tree$finalModel)
predicted <- predict(tree, myTesting)
confusionMatrix(predicted, myTesting$classe)


forest <- randomForest(classe ~. , data=myTraining)
predicted <- predict(forest, myTesting, type = "class")
confusionMatrix(predicted, myTesting$classe)