#Rainfall Prediction - group 3

## loading packages
library("data.table")
library("tidyverse")
library("gridExtra")
library("rpart")
library("rpart.plot")
library("randomForest")
library("dplyr")    
library('ggplot2')
library("neuralnet")
library("caret")
library("scales")
library("class")

#set working directory
setwd("C:/Users/HP/Desktop/BA with R/project")

#Loading the dataset
rain <- read.csv("weatherAUS.csv")
summary(rain)

#Removing the date column and creating another data frame 
rainfall <- rain[,2:23]
View(rainfall)
str(rainfall)
dim(rainfall)
class(rainfall)
names(rainfall)


# remove NA values of the columns and replace with 0 or mean according to the analysed missing values from the data set summary. 
rainfall$Rainfall[is.na(rainfall$Rainfall)] <- 0
rainfall$Evaporation[is.na(rainfall$Evaporation)] <- 0
rainfall$Sunshine[is.na(rainfall$Sunshine)] <- 0
rainfall$WindGustSpeed[is.na(rainfall$WindGustSpeed)] <- 0
rainfall$WindSpeed9am[is.na(rainfall$WindSpeed9am)] <- 0
rainfall$WindSpeed3pm[is.na(rainfall$WindSpeed3pm)] <- 0
rainfall$Cloud9am[is.na(rainfall$Cloud9am)] <- 0
rainfall$Cloud3pm[is.na(rainfall$Cloud3pm)] <- 0
rainfall$WindDir9am[is.na(rainfall$WindDir9am)] <- 0
rainfall$WindDir3pm[is.na(rainfall$WindDir3pm)] <- 0
rainfall$MinTemp[is.na(rainfall$MinTemp)] <- mean(rainfall$MinTemp, na.rm = TRUE) 
rainfall$MaxTemp[is.na(rainfall$MaxTemp)] <- mean(rainfall$MaxTemp, na.rm = TRUE) 
rainfall$Humidity9am[is.na(rainfall$Humidity9am)] <- mean(rainfall$Humidity9am, na.rm = TRUE)
rainfall$Humidity3pm[is.na(rainfall$Humidity3pm)] <- mean(rainfall$Humidity3pm, na.rm = TRUE)
rainfall$Pressure9am[is.na(rainfall$Pressure9am)] <- mean(rainfall$Pressure9am, na.rm = TRUE)
rainfall$Pressure3pm[is.na(rainfall$Pressure3pm)] <- mean(rainfall$Pressure3pm, na.rm = TRUE)
rainfall$Temp9am[is.na(rainfall$Temp9am)] <- mean(rainfall$Temp9am, na.rm = TRUE)
rainfall$Temp3pm[is.na(rainfall$Temp3pm)] <- mean(rainfall$Temp3pm, na.rm = TRUE)
rainfall <- rainfall[!is.na(rainfall$RainToday),]
rainfall <- rainfall[!is.na(rainfall$RainTomorrow),]
#filtering the data without location, WindGustDir.
rainfall <- rainfall[!is.na(rainfall$Location),]
rainfall <- rainfall[!is.na(rainfall$WindGustDir),]


# defining levels- Factorising the categorical data
rainfall$RainTomorrow<-factor(rainfall$RainTomorrow, levels=c("No","Yes"))
rainfall$RainToday<-factor(rainfall$RainToday, levels=c("No","Yes"))
rainfall$RainToday<-factor(rainfall$RainToday, levels=c("No","Yes"))
rainfall$WindDir9am<-factor(rainfall[,10])
rainfall$WindDir3pm<-factor(rainfall[,11])
rainfall$WindGustDir<-factor(rainfall[,8])
rainfall$Location<-factor(rainfall[,2])

#summary of the dataset after cleansing.
summary(rainfall)
View(rainfall)

#Plot 1: Visualization of Rainfall over the years 
ggplot(data = rainfall, aes(x = Rainfall)) + geom_histogram(binwidth = 10)

#plot 2: Location Wise Rain Percentage
rain %>% 
  select(Location, RainTomorrow) %>% 
  count(Location, RainTomorrow) %>% 
  drop_na() %>% 
  group_by(Location) %>%
  mutate(Rain_percent = n / sum(n)) %>% 
  ggplot(aes(Rain_percent , Location, fill = RainTomorrow)) +
  geom_col() +
  ylab("Location") +
  xlab("%ofRainyDays")

# plot 3 :Sunshine and RainTomorrow - we can see that it is more significant predictor for RainTomorrow
rain %>% 
  select(Sunshine, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Sunshine, fill = RainTomorrow)) + 
  geom_density( alpha = 0.3)

# plot 4 : Exploring Monthly Rain
rain %>% 
  mutate(div = lubridate::month(rain$Date, label = TRUE)) %>%
  count(Location, div, RainTomorrow) %>% 
  drop_na() %>%
  group_by(Location, div) %>%
  mutate(Rain_percent = n / sum(n)) %>% 
  ggplot(aes(Rain_percent , div, fill = RainTomorrow)) +
  geom_col() +
  facet_wrap(~Location)


# plot 5 : Min and Max Temperature and RainTomorrow
rain %>% 
  select(MinTemp, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(MinTemp, fill = RainTomorrow)) + 
  geom_density( alpha = 0.5)

rain %>% 
  select(MaxTemp, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(MaxTemp, fill = RainTomorrow)) + 
  geom_density( alpha = 0.5)


# plot 6 : RainToday and RainTomorrow
rain %>% 
  select(Rainfall, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Rainfall, fill = RainTomorrow)) + 
  geom_density( alpha = 0.4) +
  xlim(0, 5)

# plot:7 WindGustDir and RainTomorrow
rain %>% 
  select(WindGustDir, RainTomorrow) %>% 
  count(WindGustDir, RainTomorrow) %>% 
  drop_na() %>% 
  group_by(WindGustDir) %>%
  mutate(Rain_percent = n / sum(n)) %>% 
  ggplot(aes(Rain_percent , WindGustDir, fill = RainTomorrow)) +
  geom_col()

#plot 8: WindDir and RainTomorrow
rain %>% 
  select(WindDir9am, RainTomorrow) %>% 
  count(WindDir9am, RainTomorrow) %>% 
  drop_na() %>% 
  group_by(WindDir9am) %>%
  mutate(percent_Rain = n / sum(n)) %>% 
  ggplot(aes(percent_Rain , WindDir9am, fill = RainTomorrow)) +
  geom_col()

rain %>% 
  select(WindDir3pm, RainTomorrow) %>% 
  count(WindDir3pm, RainTomorrow) %>% 
  drop_na() %>% 
  group_by(WindDir3pm) %>%
  mutate(percent_Rain = n / sum(n)) %>% 
  ggplot(aes(percent_Rain , WindDir3pm, fill = RainTomorrow)) +
  geom_col()


# plot 9 : Humidity VS RainTomorrow - seems to have a big effect
rain %>% 
  select(Humidity9am, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Humidity9am, fill = RainTomorrow)) + 
  geom_density(alpha = 0.4)

rain %>% 
  select(Humidity3pm, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Humidity3pm, fill = RainTomorrow)) + 
  geom_density(alpha = 0.4)


# plot 10 : pressure and RainTomorrow
rain %>% 
  select(Pressure9am, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Pressure9am, fill = RainTomorrow)) + 
  geom_density(alpha = 0.5)

rain %>% 
  select(Pressure3pm, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Pressure3pm, fill = RainTomorrow)) + 
  geom_density(alpha = 0.5)


# plot 11: Temperature and RainTomorrow
rain %>% 
  select(Temp9am, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Temp9am, fill = RainTomorrow)) + 
  geom_density(alpha = 0.3)

rain %>% 
  select(Temp3pm, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(Temp3pm, fill = RainTomorrow)) + 
  geom_density(alpha = 0.3)


# stat 12: RainToday Vs RainTomorrow in percentage - to check the effect of the RainToday on RainTomorrow
NoNo <- percent(sum(rainfall$RainToday == "No" & rainfall$RainTomorrow == "No") / length(rainfall[,1]), accuracy = .001) 
YesYes <- percent(sum(rainfall$RainToday == "Yes" & rainfall$RainTomorrow == "Yes") / length(rainfall[,1]), accuracy = .001) 
NoYes <- percent(sum(rainfall$RainToday == "No" & rainfall$RainTomorrow == "Yes") / length(rainfall[,1]), accuracy = .001) 
YesNo <- percent(sum(rainfall$RainToday == "Yes" & rainfall$RainTomorrow == "No") / length(rainfall[,1]), accuracy = .001)

Today_Tomorrow <- data.frame(
  group = c("NoYes", "YesYes", "NoNo", "YesNo"),
  value = c(NoYes, YesYes, NoNo, YesNo)
)
Today_Tomorrow
View(Today_Tomorrow)


# Decision Tree model
rain_1<-data.frame(rainfall)
sample_size1 = dim(rain_1)[1]
set.seed(123)
sample_1 <- sample(1:sample_size1, 0.8 * sample_size1, replace=FALSE)
training_Set1 <- rain_1[sample_1, ]
testing_Set1 <- rain_1[-sample_1, ]

# Decision tree model with training data set
Model1 <- rpart(RainTomorrow ~ .,data=training_Set1, method = "class")

# decision tree model plot
rpart.plot(Model1,type = 4, box.palette = c("red", "green"), fallen.leaves = TRUE,extra = 101, split.font =1, varlen = -10)

#  cp value for removing overfitting
plotcp(Model1)

#cp value =0.018, pruned model
pruned_Model <- prune(Model1, cp = 0.018)

# pruned decision tree model plot
rpart.plot(pruned_Model)

# Perform predictions from the testing set
predictedvalues_1<-predict(pruned_Model, testing_Set1, type = "class")

#  testing set size
size_Test1 = dim(testing_Set1)[1]

# misclassification
error1 = sum(predictedvalues_1 != testing_Set1$RainTomorrow)

# Calculate the misclassification rate
misclassification_rate1 = error1/size_Test1
misclassification_rate1


#  misclassified points
Wrong = (predictedvalues_1 != testing_Set1$RainTomorrow)
# Yes/No values 
Yes_values = (predictedvalues_1 == 'Yes')
No_values = (predictedvalues_1 == 'No')
# Get the data points that are misclassified and are classified as yes
WrongAndYes = (Wrong & Yes_values)
errorYes = sum(WrongAndYes)
WrongAndNo = (Wrong & No_values)
errorNo = sum(WrongAndNo)
#misclassification rate
misclassification_rate1 = (errorYes+errorNo)/size_Test1
misclassification_rate1

# confusion matrix
confusion_matrix1 = confusionMatrix(predictedvalues_1, testing_Set1$RainTomorrow)
confusion_matrix1


# logistic regression
# copy the data frame
  rain_2<-data.frame(rainfall)
  set.seed(123)
  sample_size2 = dim(rain_2)[1]
  rate = 0.8
  sample_2 <- sample(1:sample_size2, rate * sample_size2, replace=FALSE)
  training_Set2 <- rain_1[sample_2, ]
  testing_Set2 <- rain_1[-sample_2, ]
  model2 <- glm(RainTomorrow ~ ., data = training_Set2, family = binomial(logit))
  predicted_values2 <-predict(model2, testing_Set2, type = "response")
  TestSize2 = dim(testing_Set2)[1]
  predicted_values2= rep(0,TestSize2)
  predicted_values2=ifelse(predicted_values2 > 0.2,'Yes','No')
  error2 = sum(predicted_values2 != testing_Set2$RainTomorrow)
  
  # misclassification rate
  misclassification_rate2 = error2/TestSize2
  misclassification_rate2

# confusion matrix
confusion_matrix_2 = confusionMatrix(data = as.factor(predicted_values2), reference = testing_Set2$RainTomorrow)
confusion_matrix_2



# copy the data frame- knn 
rain_3<-data.frame(rainfall)

# normalize numeric column
for (i in 1: (ncol(rain_3)-1)){
  if(is.numeric(rain_3[1,i])){
    rain_3[,i] = (rain_3[,i] - mean(rain_3[,i])) / sd(rain_3[,i])
  }
  
}

# factor column to character
for (i in 1: (ncol(rain_3)-1)){
  if(is.factor(rain_3[1,i])){
    rain_3[,i] <- as.integer(rain_3[,i])
  }
  
}
summary(rain_3)

# Create Training and Testing Sets
set.seed(123)
size3 = dim(rain_3)[1]
rate3 = 0.8
sample_3 <- sample(1:size3, rate3 * size3, replace=FALSE)
trainingSet3 <- subset(rain_3[sample_3, ])
testingSet3 <- subset(rain_3[-sample_3, ])
# Get the features of the training set
trainingfeatures <- subset(trainingSet3, select=c(-RainTomorrow))
# Get the labels of the training set
traininglabels <- trainingSet3$RainTomorrow
# Get the features of the testing set
testingfeatures <- subset(testingSet3, select=c(-RainTomorrow))
# Get the labels of the training set
testinglabels <- testingSet3$RainTomorrow

for (i in 3:7){
  # call KNN
  predictedLabels = knn(trainingfeatures,testingfeatures,traininglabels,k=i)
  
  # Get the number of data points in the data set
  TestSize3 = dim(testingSet3)[1]
  # Get the number of data points that are misclassified
  error3 = sum(predictedLabels != testingSet3$RainTomorrow)
  # Calculate the misclassification rate
  misclassification_rate3 = error3/TestSize3
  # Display the misclassification rate
  print(i) # 
  print(misclassification_rate3) # 
}

# when k = 7, misclassification rate is the lowest
set.seed(123)
# call KNN
predictedLabels = knn(trainingfeatures,testingfeatures,traininglabels,k=7)

# Get the number of data points in the data set
TestSet = dim(testingSet3)[1]
# Get the number of data points that are misclassified
error = sum(predictedLabels != testingSet3$RainTomorrow)
# Calculate the misclassification rate
misclassification_rate = error/TestSet
# Display the misclassification rate

print(misclassification_rate)

confusion_matrix = confusionMatrix(data = as.factor(predictedLabels), reference = testingSet3$RainTomorrow)
confusion_matrix

# based on the 3 models, we can see that the decision tree model has higher accuracy 
#and takes less time for computing.

