
###Random Forest Classification
 library(randomForest)
 library(MASS)
 data(fgl)
 set.seed(17)
 fgl.rf <- randomForest(type ~ ., data = fgl,
                          mtry = 2, importance = TRUE,
                          do.trace = 100, proximity = TRUE)
 print(fgl.rf)

 
 
### Random Forest vs. SVM 
  library(ipred)
  set.seed(131)
  error.RF <- numeric(10)
  for(i in 1:10) error.RF[i] <-
    errorest(type ~ ., data = fgl,
               model = randomForest, mtry = 2)$error
  summary(error.RF)
  library(e1071)
  set.seed(563)
  error.SVM <- numeric(10)
  for (i in 1:10) error.SVM[i] <-
    errorest(type ~ ., data = fgl,
               model = svm, cost = 10, gamma = 1.5)$error
  summary(error.SVM)

  ## USe random forest importance measures to identify the important predictors
   par(mfrow = c(2, 2))
   for (i in 1:4)
     plot(sort(fgl.rf$importance[,i], dec = TRUE),
            type = "h", main = paste("Measure", i))
  
  ### Random Forest Regression
    data(Boston)
    set.seed(1341)
    BH.rf <- randomForest(medv ~ ., Boston)
    print(BH.rf)
    
    