install.packages("plyr")
install.packages('e1071')
install.packages('corrplot')
install.packages('FSelector')

library(randomForest)
library(FSelector)
library(plyr)
library(e1071)
library(class)
library(ggplot2)
library(corrplot)
library(leaps)
library(MASS)
options(scipen = 999)

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

energy <- import.csv('energy_consumption.csv')
household <- import.csv('household.csv')
house <- import.csv('house.csv')

#house <- house[,-(2:5)]
#household <- household[,-c(2:5,7:9,24:27)]
#energy <- energy[,-(2:3)]
#submerge <- merge(energy[,c(1:3,5)],house[,c(1,3:27)],by='DOEID')
#final.merge <- merge(submerge,household,by='DOEID')



### COnvert to factors
## This part is not applicable to PCA analysis. In PCA we would follow the dummy variable approach.
house[2:5] <- as.data.frame(apply(house[2:5],2,as.factor))
household[c(2:5,7:9,24:27)]<-as.data.frame(apply(household[c(2:5,7:9,24:27)],2,as.factor))
energy[2:3]<-as.data.frame(apply(energy[,2:3],2,as.factor))


## Merge to make one dataset
submerge <- merge(energy[,c(1:5,7)],house[,c(1,3:5,7:31)],by='DOEID')
final.merge <- merge(submerge,household[,c(1:10,12:14,23:28)],by='DOEID')

## Remove the first row of DOEID and the error row 2187
final.merge <- final.merge[-2187,-1] 



plot(final.merge$BTUEL,type='h')
ggplot(final.merge,aes(y = BTUEL, x = TOTSQFT*1.1)) + geom_point() + geom_smooth() +
  ggtitle("Electric Energy against Area of the House")+labs(x = "Area", y= "Energy in BTU") 



ggplot(final.merge,aes(y = BTUEL, x = TYPEHUQ)) + geom_histogram(stat = "identity") + geom_smooth() +
  ggtitle("Electric Energy against AC Area of the House")+labs(x = "Area", y= "Energy in BTU") +
  geom_errorbar(aes(ymin = BTUEL-100000000, ymax = BTUEL+1))




do_cv <- function(df,output,k,model){
  set.seed(10)
  #df <- df[sample(nrow(df)),] # Shuffle(randomize) the data row wise
  df.intr <- df[, -which(names(df) %in% output)] # Find output column and move it to the last 
  df.intr <- cbind(df.intr,df[output])
  #interval <- nrow(df.intr) / k
  

  # A vector of size = nrow of data frame, with numbers 1:k, which helps to fold our data frame.
  k.fold <- cut(seq(1,nrow(df.intr)),breaks=k,labels=FALSE)
  df.act <- data.frame()
  df.act.final <- data.frame()
  mse.vector = vector()
  rmse.vector = vector()
  r.squared = vector()
  for(i in (1:k)){
    test <- df.intr[which(k.fold==i,arr.ind=TRUE),] #extracting test data
    train <- df.intr[-which(k.fold==i,arr.ind=TRUE),] #extracting training data
    
    pred <- model(train,test) # call model function(dots or lr or default)
    error <- df.intr[which(k.fold==i,arr.ind=TRUE) ,output] - pred
    actual <- df.intr[which(k.fold==i,arr.ind=TRUE) ,output]
    r.squared[i] <- 1 - (sum(error* error)/sum((actual-mean(actual))^2))
    mse.vector[i] <- mean(error*error,na.rm = TRUE) # Calculate MSE for the error
    rmse.vector[i] <- sqrt(mean(error*error,na.rm = TRUE))
    df.act <- as.data.frame(cbind(df.intr[which(k.fold==i,arr.ind=TRUE),output],  pred)) 
    df.act.final <- as.data.frame(rbind(df.act.final,df.act))
  }
  print(mean(r.squared))
  print(mean(mse.vector))
  print(mean(rmse.vector))
  #return(df.act.final)
}

# Linear model
# Assumes the last column of data is the output dimension
get_pred_lr <- function(train,test){
  nf <- ncol(train) # find total columns in Training data
  model <- lm(paste(names(train)[nf], '~ .'),data = train) # linear regression on the last column using all the other ones
  #print(summary(model)$r.squared)
  return(predict.lm(model,newdata = test)) # predicting the values of test data using the model generated above
  
}


#TOP 40
#CD65+LRGSTATE+HD65+AGEHHMEMY+TOTCSQFT+RHMCSQFT+RHMUCSQFT+TOTSQFT+TOTHSQFT+TOTRHMSQFT+AGEHHMEM2+TYPEHUQ+NHSLDMEM+RHMHSQFT+TOTBASESQFT+HHAGE+TOTUCSQFT+SPOUSE+BASUCSQFT+WALLTYPE+MONEYPY+URBRUR + SDESCENT + GARUSQFT + RETIREPY + BASHSQFT + TOTUSQFT + 
#ATTUSQFT + NCASHBEN + TOTATTCSQFT+ AGEHHMEM3+ GARUCSQFT+ BASCSQFT+ATTUCSQFT+WORKPAY+HHSEX+BASUSQFT+TOTGARGSQFT+RHMUSQFT+AGEHHMEM4


get_pred_rf<-function(train,test){
  set.seed(10)
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  #rf = randomForest(out ~ CD65+LRGSTATE+HD65+AGEHHMEMY+TOTCSQFT+RHMCSQFT+TYPEHUQ+TOTRHMSQFT+AGEHHMEM2+TOTSQFT+RHMHSQFT+RHMUCSQFT+TOTHSQFT+NHSLDMEM+HHAGE+WALLTYPE+TOTUCSQFT+TOTUSQFT+TOTBASESQFT+RHMUSQFT ,data=train, mtry=10, importance =TRUE, na.rm=TRUE,ntree = 400)
  #
  rf = randomForest(out ~ .  ,data=train,nodesize = 16,mtry = 17,importance =TRUE, na.rm=TRUE,ntree = 101)
  pred = predict(rf, newdata=test_val)
  return(pred)
  
} 


do_cv(final.merge,'BTUEL',10,get_pred_rf)

colnames(linear_new) <- c('actual','predicted')
ggplot(linear_new,aes(y = predicted, x = actual)) + geom_point() +geom_smooth() + 
  ggtitle("BTUEL -> Top 40 + LR")+labs(x = "Actual Values", y= "Predicted Values")


colnames(rf_new) <- c('actual','predicted')
ggplot(rf_new,aes(y = predicted, x = actual)) + geom_point() +geom_smooth() + 
  ggtitle("BTUEL -> TOP 40 + RF")+labs(x = "Actual Values", y= "Predicted Values")




nf <- ncol(final.merge) # find total columns in Training data
model <- lm(BTUEL ~ .,data = final.merge)

colnames(final.merge) <- c('State','Type.of.Home', 'Heat.DegDays','Cold.DegDays','BTUEL',
                           'Wall.Type','Year.Made','Neighborhood','Total.Area','Basement.Area','Attic.Area',
                           'Garage.Area','Total.Other.Area','Total.Heated.Area','Total.Unheated.Area','Basement.Heated.Area',
                           'Basement.Unheated.Area','Garage.Heated.Area','Garage.Unheated.Area','Attic.Heated.Area',
                           'Attic.Unheated.Area','Rest.Heated.HU','Rest.Unheated.HU','Total.AC.Area','Total.NonAC.Area',
                           'Basement.AC.Area','Basement.NonAC.Area','Garage.AC.Area','Garage.NonAC.Area','Attic.AC.Area','Attic.NonAC.Area',
                           'Rest.AC.HU','Rest.NonAC.HU','Home.Business','Other.Work.at.Home','People.Home.Weekday','Gender',
                           'Householder.Age','Employment.Status','Spouse','Race','People.Living.in.House','Age.Second.Oldest',
                           'Age.Third.Oldest','Age.Fourth.Oldest','Age.Youngest','Income','Retirement.Income','Cash.Benifits',
                           'Noncash.Benefits','TOtal.Income.Last.Year')
head(final.merge
     )

rf = randomForest(BTUEL ~ .  ,data=final.merge,nodesize = 16,mtry = 17,importance =TRUE, na.rm=TRUE,ntree = 101)
varImpPlot(rf,n.var = 30)

# Default predictor model
# Assumes the last column of data is the output dimension
get_pred_default <- function(train,test){
  set.seed(10)
  nf <- ncol(train)
  return(rep(mean(train[,nf],na.rm = TRUE),nrow(test))) # returning mean of last column of test data in as many rows as test data has.
}


set.seed(10)
do_cv(final.merge,'BTUEL',10,get_pred_default)
do_cv(final.merge,'BTUEL',10,get_pred_rf)

mse.vector <- vector()
test_val<-subset(final.merge,select = -c(BTUEL))
for(i in 15:30){
  rf = randomForest(BTUEL ~ . ,data=final.merge,nodesize = 16, mtry = i,importance =TRUE, na.rm=TRUE,ntree = 101)
  pred = predict(rf, newdata=test_val)
  error <- final.merge$BTUEL - pred
  mse.vector[i] <- mean(error*error,na.rm = TRUE)
}







ggplot(test, aes(y=pred, x= test[,1])) + geom_abline() + geom_smooth(method = 'lm')
ggplot(test, aes(y=test[,48], x= test[,1])) + geom_point() + geom_smooth(method = 'lm')


ggplot(final.acc, aes(y=accuracy, x= 1:nrow(final.acc))) + geom_point() +  ggtitle("Accuracy for kNN")+labs(x = "Value")
ggplot(final.acc, aes(y=1-accuracy, x= 1:nrow(final.acc))) + geom_point() + ggtitle("Error for kNN")+labs(x = "Value", y= "error") 




drops <- c('TYPEHUQ')
final.merge <- final.merge[ , !(names(final.merge) %in% drops)]

final <- cbind(house[c(2:5,7)],household[c(2:5,7,24:28)],energy[c(4:5,7)])


class(house[,3])

summary(regsubsets(BTUEL~.,data=final.merge,nbest=3,really.big = T))

model.energy <- lm(BTUEL~.,data=final.merge)
step.back <- stepAIC(model.energy, direction="both")
step.back$anova



summary(lm(BTUEL~.,data=final.merge))







dim(energy)

which(energy[,9] == '')

yy <- energy[-which(energy[,9] == 9999999 | energy[,9] == ''),9]
xx1 <- energy[-which(energy[,9] == 9999999 | energy[,9] == ''),3]
xx2 <- energy[-which(energy[,9] == 9999999 | energy[,9] == ''),4]
xx3 <- energy[-which(energy[,9] == 9999999 | energy[,9] == ''),5]

database = data.frame()
database <- cbind(xx1,yy,xx2,xx3)
database <- as.data.frame(database)

ggplot(data = database, aes(x = xx2 + xx3, y = yy)) + geom_point() + geom_smooth()

boxplot(yy ~ xx1, data = database,xlab = "Type of House", ylab= "NG Energy in BTU", main = "NG Energy consumption on the basis of house types")

plot(lm(yy ~ xx1 ))

hist()
plot(density(xx1))  
summary(lm(yy ~ xx1 + xx2 + xx3))






house <- import.csv("house.csv")
house <- house[,c(1,3:5,7:31)]

energy <- import.csv("energy_consumption.csv")
energy <- energy[,c(1:5,7)]

household <- import.csv("household.csv")
household <- household[,c(1:28)]

eh <- merge(house, energy, by.x = "DOEID", by.y = "DOEID")
ehh <- merge(eh, household, by.x = "DOEID", by.y = "DOEID")

ehh <- ehh[,-1]

install.packages("randomForest")
library(randomForest)
#number of trees = 200
#if there is NA value, just omit it

draft1 <- final.merge[,c("HD65","LRGSTATE","BTUEL",
                         "TYPEHUQ","RHMHSQFT","TOTSQFT",
                         "RHMUCSQFT","TOTUCSQFT","AGEHHMEM1","AGEHHMEM2","NHSLDMEM",
                        "TOTHSQFT", "URBRUR","HHAGE","MONEYPY", "BASHSQFT","SPOUSE",
                        "TOTBASESQFT","ATTUCSQFT","SDESCENT","WORKPAY","BASUCSQFT",
                        "TOTATTCSQFT","RETIREPY","TOTUSQFT","YEARMADE","TOTGARGSQFT","RHMUSQFT",
                        "BASCSQFT" , "GARCSQFT","ATTCSQFT","HBUSNESS" , "OTHWORK","ATHOME", "CASHBEN")]

draft2 <- final.merge[,c("BTUEL","LRGSTATE","TYPEHUQ","HD65","YEARMADE","URBRUR","TOTSQFT",
"TOTBASESQFT","TOTATTCSQFT","TOTGARGSQFT","TOTUSQFT","RHMHSQFT","RHMUSQFT","TOTUCSQFT",
"BASCSQFT","GARCSQFT","ATTCSQFT","ATTUCSQFT","RHMUCSQFT","HBUSNESS","OTHWORK",
"ATHOME","HHSEX","SDESCENT","NHSLDMEM","AGEHHMEM2","AGEHHMEM3","AGEHHMEM4","AGEHHMEM5",
"AGEHHMEM6","AGEHHMEM9","AGEHHMEM10","WORKPAY","RETIREPY","CASHBEN","NCASHBEN","MONEYPY")]


draft3 <- final.merge[,c("LRGSTATE","TYPEHUQ","HD65","YEARMADE","URBRUR","TOTSQFT",
               "TOTBASESQFT","TOTATTCSQFT","TOTGARGSQFT","TOTUSQFT","RHMHSQFT",
               "RHMUSQFT","TOTUCSQFT","BASCSQFT","GARCSQFT","ATTCSQFT","ATTUCSQFT",
               "RHMUCSQFT","HBUSNESS","OTHWORK","ATHOME","HHSEX","SPOUSE",
               "SDESCENT","NHSLDMEM","WORKPAY","RETIREPY","CASHBEN","NCASHBEN","MONEYPY","BTUEL")]



draft4 <- final.merge[,c("LRGSTATE","TYPEHUQ","HD65","YEARMADE","URBRUR","TOTSQFT","TOTBASESQFT","TOTATTCSQFT",
"TOTGARGSQFT","TOTUSQFT","RHMHSQFT","RHMUSQFT","TOTUCSQFT","BASCSQFT","GARCSQFT",
"ATTCSQFT","ATTUCSQFT","RHMUCSQFT","HBUSNESS","OTHWORK","ATHOME","HHSEX","SPOUSE",
"SDESCENT","NHSLDMEM","AGEHHMEMY","WORKPAY","CASHBEN",
"NCASHBEN","MONEYPY","BTUEL")]

URBRUR + SDESCENT + GARUSQFT + RETIREPY + BASHSQFT + TOTUSQFT + 
  ATTUSQFT + NCASHBEN + TOTATTCSQFT + AGEHHMEM3 + GARUCSQFT + 
  BASCSQFT + ATTUCSQFT + WORKPAY + HHSEX + BASUSQFT + TOTGARGSQFT + RHMUSQFT + AGEHHMEM4 +
  YEARMADE + 


do_cv(draft1,'BTUEL',100,get_pred_lr)
##"RHMCSQFT","AGEHHMEMY","TOTCSQFT","TOTRHMSQFT","WALLTYPE",



#
# , , 
#  , "HHSEX", "NCASHBEN"
##AGEHHMEM3 + AGEHHMEM4 + AGEHHMEM5 + 
##AGEHHMEM6 + AGEHHMEM9 + AGEHHMEM10

set.seed(10)
fit <- randomForest(BTUEL ~ .,  data=final.merge, ntree = 350) 
which.min(fit$mse)

tune.rpart(BTUEL ~ .,  data=final.merge)
tune.randomForest(BTUEL ~ .,  data=final.merge)

vec <- numeric()
for(i in 5:15){
  ab <- tuneRF(final.merge[,-5],final.merge[,5] , ntreeTry=20*i, plot = TRUE)
  vec <- rbind(vec, as.numeric(ab[which.min(ab[,2]),]['OOBError']))
}
      
plot(vec,type = 'l')
ab <- tuneRF(final.merge[,-5],final.merge[,5] , ntreeTry=10, plot = TRUE)

install.packages("mlbench")
install.packages("caret")
library(randomForest)
library(mlbench)
library(caret)
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#seed <- 7
metric <- "Accuracy"
#set.seed(seed)
mtry <- sqrt(ncol(final.merge))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(BTUEL~., data=final.merge[1:100,], method="rf", tuneGrid=tunegrid)
print(rf_default)






fit
varImpPlot(fit)


plot(density(final.merge$BTUEL))


abc = information.gain(BTUEL~.,final.merge)









set_cv <- makeResampleDesc("CV",iters = 3L)











ntreeTry <- c(seq(400, 500, 30))


do_cv <- function(df,output,k,func,n)
{
  total_row<-nrow(df)
  set.seed(10)
  #Shuffles the data
  df <- df[sample(total_row),]
  #Creating folds when k is passed
  fold<-cut(seq(1,nrow(df)),breaks=k, labels=FALSE)
  nf<-ncol(df)
  num<-which(names(df)==toString(trimws(output)))
  #makes the output column as the last column of the dataframe 
  df<- df[, c(1:num-1,(num+1):(nf),num)]   
  MSE<-rep(NA,k)
  #nf_test<-ncol(test)
  #divides data frame into k parts and one part is used in each iteration for testing and MSE is calculated
  for (i in 1:k)
  { 
    index <- which(fold==i,arr.ind=TRUE)     #Source : http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
    test  <- df[index, ] 
    train <- df[-index, ]
    nf <- ncol(train)
    numt <- n
    result<-func(train,test,numt)
    MSE[i]<- mean((result- (test[[output]]))^2 , na.rm = TRUE)
  }
  
  return (MSE)
}

get_pred_fr<-function(train,test,num){
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  # algorithm chose mtry = 10
  rf = randomForest(out ~., data=train, ntree=num, 
                    importance =TRUE, na.rm=TRUE)
  pred = predict(rf, newdata=test_val)
  return(pred)
}

result <- vector()
for (i in ntreeTry ){
  MSEi<- do_cv(final.merge,"BTUEL",10,get_pred_fr,i)
  result <- rbind(result,MSEi)
}
MSE_for_ntrees <- cbind(ntreeTry,result)
mean(MSE_for_ntrees[1,])
mean(MSE_for_ntrees[2,])
mean(MSE_for_ntrees[3,])
mean(MSE_for_ntrees[4,])

rf = randomForest(BTUEL ~., data=final.merge, ntree=401, 
                  importance =TRUE, na.rm=TRUE)

varImpPlot(rf)



get_pred_fr<-function(train,test){
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  #tune.rf = tuneRF(x=train, y=train[,nf], ntreetry=100, mtryStart=10, 
                   #stepFactor=20, nodesize=5)
  # algorithm chose mtry = 10
  rf = randomForest(out ~., data=train, mtry=10, 
                    importance =TRUE, na.rm=TRUE,ntree = 400)
  pred = predict(rf, newdata=test_val)
  return(pred)
}   

do_cv(final.merge,'BTUEL',10,get_pred_fr)


do_cv <- function(df,output,k,func)
{
  total_row<-nrow(df)
  set.seed(10)
  #Shuffles the data
  df <- df[sample(total_row),]
  #Creating folds when k is passed
  fold<-cut(seq(1,nrow(df)),breaks=k, labels=FALSE)
  nf<-ncol(df)
  num<-which(names(df)==toString(trimws(output)))
  #makes the output column as the last column of the dataframe 
  df<- df[, c(1:num-1,(num+1):(nf),num)]   
  MSE<-rep(NA,k)
  #nf_test<-ncol(test)
  #divides data frame into k parts and one part is used in each iteration for testing and MSE is calculated
  for (i in 1:k)
  { 
    index <- which(fold==i,arr.ind=TRUE)     #Source : http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
    test  <- df[index, ] 
    train <- df[-index, ]
    nf <- ncol(train)
    result<-func(train,test)
    MSE[i]<- mean((result- (test[[output]]))^2 , na.rm = TRUE)
  }
  
  return (MSE)
}


library(randomForest)
library(party)
library(ROCR)
library(gplots)
do_cv_class<-function(df,num_folds,model,n){
  # randomize data
  set.seed(1234)
  df <- df[sample(nrow(df)),]
  kk <- num_folds
  folds <- cut(seq(1,nrow(df)),breaks=kk,labels=FALSE)
  finalresult <- vector()
  # sparse the modedl name if it is knn 
  auc.score<-rep(NA,kk)
  for (i in 1:kk) {
    indexes <- which(folds==i,arr.ind=TRUE)
    itest <- df[indexes,]
    itrain <- df[-indexes,]
    numt <- n
    total <- model(itrain, itest,numt)
    finalresult <- rbind(finalresult,total)
    pred <- finalresult$pred
    true <- finalresult$true
    scores <- prediction(pred,true)
    # get the auc score which is  0.6312271
    scores<-performance(scores,"auc")
    auc.score[i] <-scores@y.values[[1]]
  }
  return(auc.score)
}



get_pred_fr<-function(train,test,num){
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  # algorithm chose mtry = 10
  train$out <- as.factor(train$out)
  rf = randomForest(out ~., data=train, ntree=num, 
                    importance =TRUE, na.rm=TRUE, type="classification")
  pred = predict(rf, newdata=test_val,type="prob")
  pred = data.frame(pred)$X1
  true <- test[,nf]
  result <- data.frame(pred,true)
  return(result)
}

ntreeTry <- c(seq(1,200,20))
result <- vector()
for (i in ntreeTry ){
  AUCi<- do_cv_class(final.merge,10,get_pred_fr,i)
  result <- rbind(result,AUCi)
}
AUC_for_ntrees <- cbind(ntreeTry,result)
AUC_for_ntrees

for (i in 1:nrow(AUC_for_ntrees)){
  print(mean(AUC_for_ntrees[i,2:11]))
}



