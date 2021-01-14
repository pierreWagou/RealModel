prediction_phoneme <- function(dataset) {
  # Chargement de l’environnement
  load("env.Rdata")
  library(splines)
  idx.res <- ncol(dataset)
  dataset$y <- as.factor(dataset$y)
  data.ns <- data.frame(as.matrix(dataset[, -idx.res]) %*% ns(1:(idx.res-1), df=28))
  data.ns$y <- dataset$y
  data.ns[, -29] <- scale(data.ns[, -29])
  predictions <- predict(ph.model, data.ns)
  return(predictions)
}

prediction_letter <- function(dataset) {
  load("env.Rdata")
  library(keras)
  deepLearningModel <-unserialize_model(model.serialize)
  X_test <- as.matrix(dataset[, c(-1)])
  X_test <- array_reshape(X_test, c(nrow(X_test), 16, 1))
  yhat <- predict_classes(deepLearningModel, X_test)
  correspLetterNumber = c('A', "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  yhat <- correspLetterNumber[yhat+1]
  yhat <- as.factor(yhat)  
  predictions <- yhat
  return(predictions)
}

prediction_bike <- function(dataset) {
  load("env.Rdata")
  library(randomForest)
  dataset$dteday <- as.integer(substr(dataset$dteday, 9, 10))
  Fencode <- function(d,ColName){
    d[,paste(ColName,"_cos", sep="")] <- cos(2*pi*d[,ColName]/max(d[,ColName]))
    d[,paste(ColName,"_sin", sep="")] <- sin(2*pi*d[,ColName]/max(d[,ColName]))
    d[,ColName] <- NULL
    return(d)
  }
  dataset <- Fencode(dataset, "dteday")
  dataset <- Fencode(dataset, "weekday")
  if (max(dataset$weathersit)>3){
    dataset[which(dataset$weathersit==4),]$weathersit <- 3
  }# if the weathersit=4,we will treat it as 3,because we don't have cas 4 in the training dataset
  dataset$weathersit <- as.factor(dataset$weathersit)
  dataset$season <- as.factor(dataset$season)
  dataset$mnth <- as.factor(dataset$mnth)
  dataset$holiday <- as.factor(dataset$holiday)
  dataset$workingday <- as.factor(dataset$workingday)
  dataset <- subset(dataset, select=-c(atemp))
  dataset <- subset(dataset, select=-c(yr, instant))
  predictions <- predict(model.bick,newdata=dataset)
  return(predictions)
}
