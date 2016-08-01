---
title: "Blood donation-Decision trees"
author: "Aditya Padal"
date: "4 November 2015"
output: word_document
---


```{r}
getwd()
setwd('path')
train <- read.csv('C:/Users/VISWANATH/Desktop/Aditya/Study/Blood Donation/BloodDonation.csv')
test <-  read.csv('C:/Users/VISWANATH/Desktop/Aditya/Study/Blood Donation/TestData.csv')
str(train)
str(test)

head(train)
head(test)
```

Data Cleaning
```{r}
#changing the predictor to factor
train$Donate <- factor(train$Donate)

#checking for missing values
sapply(train[1:5], function (x) {sum(is.na(x))})
#train[!complete.cases(train),]

summary(train)
#to check for correlation between variables
cor(train[,unlist(lapply(train, is.numeric))])
train$Volume <- NULL
test$Volume<-NULL
```
reengineering
```{r}

train$freq <- (train$Months_SFD - train$Months_SLD)/train$Num_Donations

```
Building a decision tree
```{r}
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
train_dt1 <- rpart(Donate ~ Months_SLD+Num_Donations+Months_SFD+freq, data = train, method = "class")
plot(train_dt1)
text(train_dt1)
fancyRpartPlot(train_dt1)
my_prediction <- predict(train_dt1, test)
my_solution <- data.frame(test$Student,my_prediction)
head(my_solution)

write.csv(my_solution,"C:/Users/VISWANATH/Desktop/Aditya/Study/Blood Donation/Sol_DT2.csv")
```

building using random forest
```{r}
library(randomForest)
my_forest <- randomForest(as.factor(Donate) ~ Months_SLD+Num_Donations+Months_SFD, 
                            data = train, importance = TRUE, ntree = 1000, method = "class")

my_prediction <- predict(my_forest, test)
my_solution <- data.frame(test$Student,my_prediction)
head(my_solution)

write.csv(my_solution,"C:/Users/VISWANATH/Desktop/Aditya/Study/Blood Donation/Sol_DT2.csv")
```

