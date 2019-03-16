#Setting the environment a the lOADING Libraries
setwd("~/PML Project")
library(caret)
library(knitr)
library(rattle)
library(doParallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
#Loading files
test <-read.csv("pml-testing.csv")
train <-read.csv("pml-training.csv")
dim(test)  # 20 observation, 160 variable not used in creaeting the model
dim(train) # 19622 observation, 160 variable used for buiding the model
str(train)
set.seed(10000)
#Cleaning data and preprocessing: done both to the training and the testing set
 ##Removing the nearzeros
   nztest<- nearZeroVar(test)
   nztrain<- nearZeroVar(train)
   ctest <- test[, -nztest]
   train  <- train[, -nztrain]
   dim(test)    # 20 observation, 59 variables
   dim(train)   # 19622 observation, 100 variables
 ##Removing Variables with too many NAs (50%>)
   EmCol <- which(colSums(is.na(train))>0.5*dim(train)[1]) 
   ctrain <- train[,-EmCol]
   ##Checking for any variables with still missing data
   sum(!complete.cases(ctest))
   sum(!complete.cases(ctrain))
 ##Removing user identefiers and and timestamps as they are not relevent to the model
   ctrain<- ctrain[, -(1:7)]
   #ctest<- ctest[, -(1:7)]
   dim(ctest)   # 20 observation, 52 variables  
   dim(ctrain)  # 19622 observation, 52 variables
#Creating the Model
 ##Train data partitioning with 60% to train and 40% to test set
   inTrain<-createDataPartition(y=ctrain$classe, p=0.6 , list= FALSE)
   training<-ctrain[inTrain,]; testing<-ctrain[-inTrain,]
 ##Buid prediction Models: a number of models will be tested (including an ensemblled model of the top 3 models)
 ###LDA model
   modlda<-train(classe~ ., data =training, method= "lda")
   modlda
   confusionMatrix.train(modlda)  #accuracy:0.6921
 ###RAndom Force using parllel and crossvalidation as suggested by one of the mentor in the foroum to save computing time 
   fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
   modrf<-train(classe~., data=training, method="rf",trControl = fitControl)
   modrf
   modrf$resample
   confusionMatrix.train(modrf) #accuracy 0.9884
 ###GBM model
   modgbm <- train(classe~ ., method="gbm",data=training,verbose=FALSE)
   confusionMatrix.train(modgbm) #accuracy 0.9558
  ##Testing the prediction Models:
  gbm.pred <- predict(modgbm,testing)
  gbmMat <- confusionMatrix(testing$classe,gbm.pred)
  gbmMat$table
  gbmMat$overall[1]
  lda.pred <- predict(modlda,testing)
  ldaMat <- confusionMatrix(testing$classe,lda.pred)
  ldaMat$table
  ldaMat$overall[1] ##accuracy: 0.6893959
  rf.pred <- predict(modrf,testing)
  rfMat <- confusionMatrix(testing$classe,rf.pred)
  rfMat$table
  rfMat$overall[1] ##accuracy: 0.990441
  ##selecting the highest model accuracy to perform on the test set
  Smodel<-modrf
  smodel.pred <- predict(Smodel,ctest)
  smodel.pred
  ##The results on the test using this selected model resulted in 100% in the results of the quiz
    
  
 stopCluster(cluster)
 registerDoSEQ()
   
   
