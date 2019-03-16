#Setting the environment a the lOADING Libraries
setwd("G:/Folders/Master/Fungal-NIR/Work/R")
library(caret)
library(knitr)
library(rattle)
library(doParallel)
library(doParallel)
select(dataFrameTable, -var1)
library(dplyr)
library("data.table")
library("hyperSpec")

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
#Loading files
train <-read.csv("2016.August merged (ratioed).csv")
spc<-train
spczV <-as.data.frame(which(colSums(spc<10 & spc>-10 )<0.5*dim(spc)[1])) 
cspc<- select(spc, c(row.names(spczV)))
spc<-cspc[,-(1:5)]
names(spc)<-gsub("x","",names(spc))
wl<- 
Edata<-train[,(1:5)]
cbind(Edata,cspc)
hspc <- new ("hyperSpec", spc = spc, wavelength = as.numeric(wl), data = Edata)
plot(hspc)
set.seed(10000)
modlda<-train(Edata$C1~spc, data =hspc$., preProcess=c("pca","center"), method= "lda")
pca <- prcomp (~ spc, data = hspc, center = TRUE)
scores <- decomposition (hspc, pca$x, label.wavelength = "PC", label.spc = "score / a.u.")
qda <- qda(spc~., scores)
zspc<-nearZeroVar(spc)
plot(zspc)

#Creating the Model
 ##Train data partitioning with 60% to train and 40% to test set
   inTrain<-createDataPartition(y=whole$C1, p=0.9 , list= FALSE)
   training<-whole[inTrain,]; testing<-whole[-inTrain,]
 ##Buid prediction Models: a number of models will be tested (including an ensemblled model of the top 3 models)
 ###LDA model
   modlda<-train(C1~., data =training, preProcess=c("pca","center"), method= "lda")
   modlda
   modqda<-train(C1~., data =training, preProcess=c("pca","center"), method= "qda")
   confusionMatrix.train(modlda)  #accuracy:0.64
   confusionMatrix.train(modqda)
 ###RAndom Force using parllel and crossvalidation as suggested by one of the mentor to save computing time 
   fitControl <- trainControl(method = "cv",  allowParallel = TRUE)
   modrf<-train(C1~., data=training, method="rf", preProcess=c("pca","center"), trControl = fitControl)
  
   modrf
   modrf$resample
   confusionMatrix.train(modrf) #accuracy 0.7097
 ###GBM model
   modgbm <- train(C1~ .,  preProcess=c("pca","center", "scale") , method="gbm",data=training,verbose=FALSE)
 ###Ensembled model
  # cluster <- makeCluster(detectCores() - 1)
   #registerDoParallel(cluster)
   #fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
   
   #modgbm <- train(classe~ ., method="gbm",data=training,verbose=FALSE)
   #confusionMatrix.train(modgbm)
  ##Testing the prediction Models:
  gbm.pred <- predict(modgbm,testing)
  gbmMat <- confusionMatrix(testing$classe,gbm.pred)
  gbmMat$table
  gbmMat$overall[1]
  lda.pred <- predict(modlda,testing)
  ldaMat <- confusionMatrix(testing$classe,lda.pred)
  ldaMat$table
  ldaMat$overall[1]
  rf.pred <- predict(modrf,testing)
  rfMat <- confusionMatrix(testing$classe,rf.pred)
  rfMat$table
  rfMat$overall[1]
  ##selecting the highest model accuracy to perform on the test set
  Smodel<-modrf
  smodel.pred <- predict(Smodel,ctest)
  smodel.pred
  library(ggplot2)
  qplot(age,wage,colour=education,data=training)+geom_smooth(method='lm',formula=y~x)
 stopCluster(cluster)
 registerDoSEQ()
   
 
 preProcess=c("center", "scale")
   
