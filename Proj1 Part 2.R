
library(rgl)
library(tree)
library(GGally)
library(ISLR)
library(caret)
library(rattle)
library(readr)
library(dplyr)

#read in file
a = read.csv(file.choose())

#reorganize for MSRP first column
a <- a %>%
  select(MSRP, everything())


fitControl<-trainControl(method="repeatedcv",number=10,repeats=10) #number is the k in k-fold


#As discussed in class, while we will perform feature selection via CV, it is still important
#to have a validation set.  Caret has a nice function for data splitting.

set.seed(1234)
trainIndex<-createDataPartition(a$MSRP,p=.8,list=F)  #p: proportion of data in train

training<-a[trainIndex,]
validate<-a[-trainIndex,]


###################################################
#Tree Model while choosing complexity parameter via 10 fold CV
#
set.seed(1234)
tree.fit<-train(MSRP ~ .,
                    data=training,
                    method="rpart",minsplit=5,
                    trControl=fitControl,
                tuneGrid=data.frame(cp=c(.005,.0008,.01,.015,.02,.025,.03,.035,.04,.05,.06,.07,.08,.09,.25,.4))
)

#Lets look at the CV result
tree.fit

#If we want the final model tree
plot(tree.fit$finalModel)
text(tree.fit$finalModel)

#prettier tree
fancyRpartPlot(tree.fit$finalModel)


#Making predictions on the validation set
tree.pred<-predict(tree.fit,validate)

#Computing Errror Metrics
tree.validate<-postResample(pred=tree.pred,obs=validate$Salary)
tree.validate

plot(tree.pred,validate$Salary)
lines(0:2000,0:2000)

#Ranking predictors
varImp(tree.fit)
plot(varImp(tree.fit))


###################################################
# k-nn Model  10 fold CV to choose k
#
#Remember only continuous predictors
a2 <- select_if(a, is.numeric)  
a2


#recreating test/validate with knn for only numeric

set.seed(1234)
trainIndex<-createDataPartition(a2$MSRP,p=.8,list=F)  #p: proportion of data in train

training2<-a2[trainIndex,]
validate2<-a2[-trainIndex,]

#selecting only continues
set.seed(1234)
knn.fit<-train(MSRP~ .,
                data=training2,
                method="knn",preProcess = c("center","scale"),
                trControl=fitControl,
                tuneGrid=data.frame(k=c(1:10,15,20,25,30))
)

#CV result
knn.fit


plot(knn.fit)


#Making predictions on the validation set
knn.pred<-predict(knn.fit,validate)

#Computing Errror Metrics
knn.validate<-postResample(pred=knn.pred,obs=validate$MSRP)
knn.validate

plot(knn.pred,validate$MSRP)
lines(0:2000,0:2000)

#Ranking predictors
varImp(knn.fit)
plot(varImp(knn.fit))


