### Hypertune parameters for various models
### Anna Gow - Summer 2018

# TODO: Comment in/out whichever code you want to run (depending which models you want to optimize / stack)
# Run after cleaning text and creating DTM

# NOTE: The XGBoost function is not working as of August 27, 2018 
# There must be a syntax error introduced accidentally - check function documentation

# Turn off scientific notation
options(scipen=10)

#======================================================================================================
# Load packages

library(data.table)
library(ggplot2)
library(mltools)  # For generating CV folds and one-hot-encoding
library(class)  # K-Nearest Neighbors model
library(LiblineaR)  # Support Vector Machine and Logistic Regression
library(randomForest) # adding Random Forest model

#======================================================================================================
# Load the data

# TODO: Will need to change this depending on your settings (i.e. where file is saved)
setwd("C:/Users/GowA/Documents")
df <- read.csv("DTM_Q_RL.csv", stringsAsFactors = FALSE)
names(df)[names(df) == "X"] <- "ID"

# NOTE: USe the following code if you want to try binary classification
#df$Level[df$Level == 1] <- 0
#df$Level[df$Level == 2] <- 1
#df$Level[df$Level == 3] <- 1


#--------------------------------------------------
# Split into 80/20 train/test 
smp_size <- floor(0.80 * nrow(df))
train_idx <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_idx, ]
test <- df[-train_idx, ]

#--------------------------------------------------
# Make data table and fix column types

train <- as.data.table(train)
test <- as.data.table(test)

train[, Level := factor(Level)]
test[, Level := factor(Level)]

#--------------------------------------------------
# Build folds for cross validation and stacking

train[, FoldID := folds(Level, nfolds=5L, stratified=TRUE, seed=1996)]  # mltools function

#======================================================================================================
# KNN
#
# Do a grid search for k = 1, 2, ... 30 by cross validating model using folds 1-5
# I.e. [test=f1, train=(f2, f3, f4, f5)], [test=f2, train=(f1, f3, f4, f5)], ...

# BEST 83
knnCV <- list()
colnames_vect <- colnames(train)
remove <- c("Level", "ID", "FoldID", "Coleman.Liau.grade", "FOG")
colnames_vect <- colnames_vect[!colnames_vect %in% remove]
knnCV[["Features"]] <- colnames_vect
knnCV[["ParamGrid"]] <- CJ(k=seq(30, 200))
knnCV[["BestScore"]] <- 0

# Loop through each set of parameters
for(i in seq_len(nrow(knnCV[["ParamGrid"]]))){

  # Get the ith set of parameters
  params <- knnCV[["ParamGrid"]][i]

  # Build an empty vector to store scores from each train/test fold
  scores <- numeric()

  # Build an empty list to store predictions from each train/test fold
  predsList <- list()

  # Loop through each test fold, fit model to training folds and make predictions on test fold
  for(foldID in 1:5){

    # Build the train/test folds
    testFold <- train[J(FoldID=foldID), on="FoldID"]
    trainFolds <- train[!J(FoldID=foldID), on="FoldID"]  # Exclude fold i from trainFolds

    # Train the model & make predictions
    testFold[, Pred := knn(train=trainFolds[, knnCV$Features, with=FALSE], test=testFold[, knnCV$Features, with=FALSE], cl=trainFolds$Level, k=params$k)]
    predsList <- c(predsList, list(testFold[, list(ID, FoldID, Pred)]))

    # Evaluate predictions (accuracy rate) and append score to scores vector
    score <- mean(testFold$Pred == testFold$Level)
    scores <- c(scores, score)
  }

  # Measure the overall score. If best, tell knnCV
  score <- mean(scores)

  # Insert the score into ParamGrid
  knnCV[["ParamGrid"]][i, Score := score][]
  print(paste("Params:", paste(colnames(knnCV[["ParamGrid"]][i]), knnCV[["ParamGrid"]][i], collapse = " | ")))

  if(score > knnCV[["BestScore"]]){
    knnCV[["BestScores"]] <- scores
    knnCV[["BestScore"]] <- score
    knnCV[["BestParams"]] <- knnCV[["ParamGrid"]][i]
    knnCV[["BestPreds"]] <- rbindlist(predsList)
  }
}

# Check the best parameters
knnCV[["BestParams"]]

# Plot the score for each k value
knnCV[["ParamGrid"]]
ggplot(knnCV[["ParamGrid"]], aes(x=k, y=Score))+geom_line()+geom_point()


#======================================================================================================
# SVM
#
# Do a grid search for k = 1, 2, ... 30 by cross validating model using folds 1-5
# I.e. [test=f1, train=(f2, f3, f4, f5)], [test=f2, train=(f1, f3, f4, f5)], ...

svmCV <- list()
svmCV[["Features"]] <- colnames_vect
svmCV[["ParamGrid"]] <- CJ(type=c(1, 2, 5), cost=c(.01, .1, 1, 10, 100, 1000, 2000), Score=NA_real_)
svmCV[["BestScore"]] <- 0

# Loop through each set of parameters
for(i in seq_len(nrow(svmCV[["ParamGrid"]]))){

  # Get the ith set of parameters
  params <- svmCV[["ParamGrid"]][i]

  # Build an empty vector to store scores from each train/test fold
  scores <- numeric()

  # Build an empty list to store predictions from each train/test fold
  predsList <- list()

  # Loop through each test fold, fit model to training folds and make predictions on test fold
  for(foldID in 1:5){

    # Build the train/test folds
    testFold <- train[J(FoldID=foldID), on="FoldID"]
    trainFolds <- train[!J(FoldID=foldID), on="FoldID"]  # Exclude fold i from trainFolds

    # Train the model & make predictions
    svm <- LiblineaR(data=trainFolds[, svmCV$Features, with=FALSE], target=trainFolds$Level, type=params$type, cost=params$cost)
    testFold[, Pred := predict(svm, testFold[, svmCV$Features, with=FALSE])$predictions]
    predsList <- c(predsList, list(testFold[, list(ID, FoldID, Pred)]))

    # Evaluate predictions (accuracy rate) and append score to scores vector
    score <- mean(testFold$Pred == testFold$Level)
    scores <- c(scores, score)
  }

  # Measure the overall score. If best, tell svmCV
  score <- mean(scores)

  # Insert the score into ParamGrid
  svmCV[["ParamGrid"]][i, Score := score][]
  print(paste("Params:", paste(colnames(svmCV[["ParamGrid"]][i]), svmCV[["ParamGrid"]][i], collapse = " | ")))

  if(score > svmCV[["BestScore"]]){
    svmCV[["BestScores"]] <- scores
    svmCV[["BestScore"]] <- score
    svmCV[["BestParams"]] <- svmCV[["ParamGrid"]][i]
    svmCV[["BestPreds"]] <- rbindlist(predsList)
  }
}

# Check the best parameters
svmCV[["BestParams"]]

# Plot the score for each (cost, type) pairs
svmCV[["ParamGrid"]]
ggplot(svmCV[["ParamGrid"]], aes(x=cost, y=Score, color=factor(type)))+geom_line()+geom_point()+scale_x_log10()

# #======================================================================================================
# # Random Forest
# #
# # Do a grid search for ntree = 50, 100, 250, 500, 1000, 2500, 5000 by cross validating model using folds 1-5
# # I.e. [test=f1, train=(f2, f3, f4, f5)], [test=f2, train=(f1, f3, f4, f5)], ...
# 
# rfCV <- list()
# rfCV[["Features"]] <- colnames_vect
# rfCV[["ParamGrid"]] <- CJ(ntree = c(50, 100, 250, 500))
# rfCV[["BestScore"]] <- 0
# 
# # Loop through each set of parameters
# for(i in seq_len(nrow(rfCV[["ParamGrid"]]))){
# 
#   # Get the ith set of parameters
#   params <- rfCV[["ParamGrid"]][i]
# 
#   # Build an empty vector to store scores from each train/test fold
#   scores <- numeric()
# 
#   # Build an empty list to store predictions from each train/test fold
#   predsList <- list()
# 
#   # Loop through each test fold, fit model to training folds and make predictions on test fold
#   for(foldID in 1:5){
# 
#     # Build the train/test folds
#     testFold <- train[J(FoldID=foldID), on="FoldID"]
#     trainFolds <- train[!J(FoldID=foldID), on="FoldID"]  # Exclude fold i from trainFolds
# 
#     # Train the model & make predictions
#     rf <- randomForest(trainFolds$Level~.,data=trainFolds[, rfCV$Features, with=FALSE], ntree=params$ntree)
#     testFold[, Pred := predict(rf,data=trainFolds[, rfCV$Features, with=FALSE])]
#     predsList <- c(predsList, list(testFold[, list(ID, FoldID, Pred)]))
# 
#     # Evaluate predictions (accuracy rate) and append score to scores vector
#     score <- mean(testFold$Pred == testFold$Level)
#     scores <- c(scores, score)
#   }
# 
#   # Measure the overall score. If best, tell knnCV
#   score <- mean(scores)
# 
#   # Insert the score into ParamGrid
#   rfCV[["ParamGrid"]][i, Score := score][]
#   print(paste("Params:", paste(colnames(rfCV[["ParamGrid"]][i]), rfCV[["ParamGrid"]][i], collapse = " | ")))
# 
#   if(score > rfCV[["BestScore"]]){
#     rfCV[["BestScores"]] <- scores
#     rfCV[["BestScore"]] <- score
#     rfCV[["BestParams"]] <- rfCV[["ParamGrid"]][i]
#     rfCV[["BestPreds"]] <- rbindlist(predsList)
#   }
# }
# 
# # Check the best parameters
# rfCV[["BestParams"]]
# 
# # Plot the score for each number of trees
# rfCV[["ParamGrid"]]
# ggplot(rfCV[["ParamGrid"]], aes(x=ntree, y=Score))+geom_line()+geom_point()


# #======================================================================================================
# # XGBoost
# #
# # Do a grid search for various parameters by cross validating model using folds 1-5
# # I.e. [test=f1, train=(f2, f3, f4, f5)], [test=f2, train=(f1, f3, f4, f5)], ...
# library(xgboost)
# xgCV <- list()
# xgCV[["Features"]] <- colnames_vect
# xgCV[["ParamGrid"]] <- CJ(eta = c(0.01, 0.1, 1), nrounds = c(10, 50, 100, 250, 500))
# xgCV[["BestScore"]] <- 0
# 
# # Loop through each set of parameters
# for(i in seq_len(nrow(xgCV[["ParamGrid"]]))){
# 
#   # Get the ith set of parameters
#   params <- xgCV[["ParamGrid"]][i]
# 
#   # Build an empty vector to store scores from each train/test fold
#   scores <- numeric()
# 
#   # Build an empty list to store predictions from each train/test fold
#   predsList <- list()
#   
#   # Convert "Level" to 0 or 1
#   train$Level <- as.numeric(train$Level) - 1
#   test$Level <- as.numeric(test$Level) - 1
# 
#   # Loop through each test fold, fit model to training folds and make predictions on test fold
#   for(foldID in 1:5){
# 
#     # Build the train/test folds
#     testFold <- train[J(FoldID=foldID), on="FoldID"]
#     trainFolds <- train[!J(FoldID=foldID), on="FoldID"] # Exclude fold i from trainFolds
# 
#     # Train the model & make predictions
#     bst <- xgboost(data=as.matrix(trainFolds[, xgCV$Features, with=FALSE]), label = trainFolds$Level, eta=params$eta, nrounds = params$nrounds, objective = "binary:logistic")
#     testFold[, Pred := predict(bst,data=as.matrix(trainFolds[, xgCV$Features, with=FALSE]))]
#     predsList <- c(predsList, list(testFold[, list(ID, FoldID, Pred)]))
# 
#     # Evaluate predictions (accuracy rate) and append score to scores vector
#     score <- mean(testFold$Pred == testFold$Level)
#     scores <- c(scores, score)
#   }
# 
#   # Measure the overall score. If best, tell knnCV
#   score <- mean(scores)
# 
#   # Insert the score into ParamGrid
#   xgCV[["ParamGrid"]][i, Score := score][]
#   print(paste("Params:", paste(colnames(xgCV[["ParamGrid"]][i]), xgCV[["ParamGrid"]][i], collapse = " | ")))
# 
#   if(score > xgCV[["BestScore"]]){
#     xgCV[["BestScores"]] <- scores
#     xgCV[["BestScore"]] <- score
#     xgCV[["BestParams"]] <- xgCV[["ParamGrid"]][i]
#     xgCV[["BestPreds"]] <- rbindlist(predsList)
#   }
# }
# 
# # Check the best parameters
# xgCV[["BestParams"]]
# 
# # Plot the score for each number of trees
# xgCV[["ParamGrid"]]

#======================================================================================================
# Ensemble KNN, SVM using Logistic Regression

# Extract predictions
metas.knn <- knnCV[["BestPreds"]]
metas.svm <- svmCV[["BestPreds"]]
#metas.rf <- rfCV[["BestPreds"]]
#metas.xg <- xgCV[["BestPreds"]]

# Insert regular predictions into train
train[metas.knn, Meta.knn := Pred, on="ID"]
train[metas.svm, Meta.svm := Pred, on="ID"]
#train[metas.rf, Meta.rf := Pred, on="ID"]
#train[metas.xg, Meta.xg := Pred, on="ID"]

# One-hot encode predictions
train <- one_hot(train, cols=c("Meta.knn", "Meta.svm"), dropCols=FALSE)
#train <- one_hot(train, cols=c("Meta.svm", "Meta.xg"), dropCols=FALSE)
#train <- one_hot(train, cols=c("Meta.knn", "Meta.rf"), dropCols=FALSE)
#train <- one_hot(train, cols=c("Meta.svm", "Meta.rf"), dropCols=FALSE)
#train <- one_hot(train, cols=c("Meta.knn", "Meta.svm", "Meta.rf"), dropCols=FALSE)

#--------------------------------------------------
# Cross Validation

lrCV <- list()
lrCV[["Features"]] <- setdiff(colnames(train), c("ID", "FoldID", "Level", "Meta.knn", "Meta.svm"))
#lrCV[["Features"]] <- setdiff(colnames(train), c("ID", "FoldID", "Level", "Meta.svm", "Meta.xg"))
#lrCV[["Features"]] <- setdiff(colnames(train), c("ID", "FoldID", "Level", "Meta.knn", "Meta.rf"))
lrCV[["ParamGrid"]] <- CJ(type=c(0, 6, 7), cost=c(.001, .01, .1, 1, 10, 100))
lrCV[["BestScore"]] <- 0

# Loop through each set of parameters
for(i in seq_len(nrow(lrCV[["ParamGrid"]]))){
  
  # Get the ith set of parameters
  params <- lrCV[["ParamGrid"]][i]
  
  # Build an empty vector to store scores from each train/test fold
  scores <- numeric()
  
  # Build an empty list to store predictions from each train/test fold
  predsList <- list()
  
  # Loop through each test fold, fit model to training folds and make predictions on test fold
  for(foldID in 1:5){
    
    # Build the train/test folds
    testFold <- train[J(FoldID=foldID), on="FoldID"]
    trainFolds <- train[!J(FoldID=foldID), on="FoldID"]  # Exclude fold i from trainFolds
    
    # Train the model & make predictions
    logreg <- LiblineaR(data=trainFolds[, lrCV$Features, with=FALSE], target=trainFolds$Level, type=params$type, cost=params$cost)
    testFold[, Pred := predict(logreg, testFold[, lrCV$Features, with=FALSE])$predictions]
    predsList <- c(predsList, list(testFold[, list(ID, FoldID, Pred)]))
    
    # Evaluate predictions (accuracy rate) and append score to scores vector
    score <- mean(testFold$Pred == testFold$Level)
    scores <- c(scores, score)
  }
  
  # Measure the overall score. If best, tell lrCV
  score <- mean(scores)
  
  # Insert the score into ParamGrid
  lrCV[["ParamGrid"]][i, Score := score][]
  print(paste("Params:", paste(colnames(lrCV[["ParamGrid"]][i]), lrCV[["ParamGrid"]][i], collapse = " | ")))
  
  if(score > lrCV[["BestScore"]]){
    lrCV[["BestScores"]] <- scores
    lrCV[["BestScore"]] <- score
    lrCV[["BestParams"]] <- lrCV[["ParamGrid"]][i]
    lrCV[["BestPreds"]] <- rbindlist(predsList)
  }
}

knnCV[["BestParams"]]
svmCV[["BestParams"]]
#xgCV[["BestParams"]]
#rfCV[["BestParams"]]
lrCV[["BestParams"]]

#======================================================================================================
# Make predictions on the holdout set

# knn
Meta.knn <- knn(train=train[, knnCV$Features, with=FALSE], test=test[, knnCV$Features, with=FALSE], cl=train$Level, k=knnCV$BestParams$k)
test <- cbind(test, Meta.knn)
#test[, Meta.knn := knn(train=train[, knnCV$Features, with=FALSE], test=test[, knnCV$Features, with=FALSE], cl=train$Level, k=knnCV$BestParams$k)]

# svm
svm <- LiblineaR(data=train[, svmCV$Features, with=FALSE], target=train$Level, type=svmCV$BestParams$type, cost=svmCV$BestParams$cost)
Meta.svm <- predict(svm, test[,svmCV$Features, with = FALSE])$predictions
test <- cbind(test, Meta.svm)
#test[, Meta.svm := predict(svm, test[, svmCV$Features, with=FALSE])$predictions]

# # XGBoost
# xg <- xgboost(data=train[, xgCV$Features, with=FALSE], label = train$Level, eta=xgCV$BestParams$eta, nrounds = xgCV$BestParams$nrounds)
# Meta.xg <- predict(xg, data = train[, xgCV$Features, with = FALSE])
# test <- cbind(test, Meta.xg)


# # rf
# rf <- randomForest(train$Level~., data = train[, rfCV$Features, with=FALSE], ntree=rfCV$BestParams$ntree)
# Meta.rf <- predict(rf, test[,rfCV$Features, with = FALSE])
# test <- cbind(test, Meta.rf)

# ensemble
test <- one_hot(test, cols=c("Meta.knn", "Meta.svm"), dropCols=FALSE)
#test <- one_hot(test, cols=c("Meta.knn", "Meta.rf"), dropCols=FALSE)
logreg <- LiblineaR(data=trainFolds[, lrCV$Features, with=FALSE], target=trainFolds$Level, type=lrCV$BestParams$type, cost=lrCV$BestParams$cost)
Pred.ensemble <- predict(logreg, test[,lrCV$Features, with = FALSE])$predictions
test <- cbind(test, Pred.ensemble)
#test[, Pred.ensemble := predict(logreg, test[, lrCV$Features, with=FALSE])$predictions]

# Results
mean(test$Level == test$Meta.knn)
mean(test$Level == test$Meta.svm)
#mean(test$Level == test$Meta.xg)
#mean(test$Level == test$Meta.rf)
mean(test$Level == test$Pred.ensemble)
