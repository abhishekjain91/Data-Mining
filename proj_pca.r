
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


### COnvert to factors
## This part is not applicable to PCA analysis. In PCA we would follow the dummy variable approach.
#house[2:5] <- as.data.frame(apply(house[2:5],2,as.factor))
#household[c(2:5,7:9,24:27)]<-as.data.frame(apply(household[c(2:5,7:9,24:27)],2,as.factor))
#energy[2:3]<-as.data.frame(apply(energy[,2:3],2,as.factor))

## Merge to make one dataset
submerge <- merge(energy[,c(1:5,7)],house[,c(1,3:5,7:31)],by='DOEID')
final.merge <- merge(submerge,household[,c(1:10,12:14,23:28)],by='DOEID')

## remove the first row of DOEID and the error row 2187
final.merge <- final.merge[-2187,-1]



##########Dummy variable creation for PCA analysis########################
####Energy Table Starts######
state.other <- ifelse(final.merge$LRGSTATE == 0,1,0)
state.ny <- ifelse(final.merge$LRGSTATE == 1,1,0)
state.ca <- ifelse(final.merge$LRGSTATE == 2,1,0)
state.tx <- ifelse(final.merge$LRGSTATE == 3,1,0)
state.fl <- ifelse(final.merge$LRGSTATE == 4,1,0)


home.ty.mob <- ifelse(final.merge$TYPEHUQ == 1,1,0)
home.ty.det <- ifelse(final.merge$TYPEHUQ == 2,1,0)
home.ty.att <- ifelse(final.merge$TYPEHUQ == 3,1,0)
home.ty.2to4 <- ifelse(final.merge$TYPEHUQ == 4,1,0)
home.ty.5plus <- ifelse(final.merge$TYPEHUQ == 5,1,0)

pca.energy.df <- cbind(state.other,state.ny,state.ca,state.tx,state.fl,
                home.ty.mob,home.ty.det,home.ty.att,home.ty.2to4,home.ty.5plus)




####Energy Table Ends######


####House Table Starts######

wall.brick <- ifelse(final.merge$WALLTYPE == 1,1,0)
wall.wood <- ifelse(final.merge$WALLTYPE == 2,1,0)
wall.siding <- ifelse(final.merge$WALLTYPE == 3,1,0)
wall.stucco <- ifelse(final.merge$WALLTYPE == 4,1,0)
wall.shingle <- ifelse(final.merge$WALLTYPE == 5,1,0)
wall.stone <- ifelse(final.merge$WALLTYPE == 6,1,0)
wall.concrete <- ifelse(final.merge$WALLTYPE == 7,1,0)
wall.glass <- ifelse(final.merge$WALLTYPE == 8,1,0)
wall.other <- ifelse(final.merge$WALLTYPE == 9,1,0)
wall.indesc <- ifelse(final.merge$WALLTYPE == 10,1,0)


yearmade.b1940 <- ifelse(final.merge$YEARMADE == 1,1,0)
yearmade.1940plus <- ifelse(final.merge$YEARMADE == 2,1,0)
yearmade.1950plus <- ifelse(final.merge$YEARMADE == 3,1,0)
yearmade.1960plus <- ifelse(final.merge$YEARMADE == 4,1,0)
yearmade.1970plus <- ifelse(final.merge$YEARMADE == 5,1,0)
yearmade.1980plus <- ifelse(final.merge$YEARMADE == 6,1,0)
yearmade.1985plus <- ifelse(final.merge$YEARMADE == 7,1,0)
yearmade.1990plus <- ifelse(final.merge$YEARMADE == 8,1,0)
yearmade.1995plus <- ifelse(final.merge$YEARMADE == 9,1,0)
yearmade.2000plus <- ifelse(final.merge$YEARMADE == 10,1,0)
yearmade.2003 <- ifelse(final.merge$YEARMADE == 11,1,0)
yearmade.2004 <- ifelse(final.merge$YEARMADE == 12,1,0)
yearmade.2005 <- ifelse(final.merge$YEARMADE == 13,1,0)


neigh.city <- ifelse(final.merge$URBRUR == 1,1,0)
neigh.town <- ifelse(final.merge$URBRUR == 2,1,0)
neigh.suburbs <- ifelse(final.merge$URBRUR == 3,1,0)
neigh.rural <- ifelse(final.merge$URBRUR == 4,1,0)


pca.house.df <- cbind(wall.brick,wall.wood,wall.siding,wall.stucco,wall.shingle,wall.stone,
                      wall.concrete,wall.glass,wall.other,wall.indesc,
                      yearmade.b1940,yearmade.1940plus,yearmade.1950plus,yearmade.1960plus,yearmade.1970plus,
                      yearmade.1980plus,yearmade.1985plus,yearmade.1990plus,yearmade.1995plus,
                      yearmade.2000plus,yearmade.2003,yearmade.2004,yearmade.2005,
                      neigh.city,neigh.town,neigh.suburbs,neigh.rural)

####House Table Ends######

####Household Table Starts######

homebusiness.yes <- ifelse(final.merge$HBUSNESS == 1,1,0)
homebusiness.no <- ifelse(final.merge$HBUSNESS == 0,1,0)

otherwork.yes <- ifelse(final.merge$OTHWORK == 1,1,0)
otherwork.no <- ifelse(final.merge$OTHWORK == 0,1,0)

athome.yes <- ifelse(final.merge$ATHOME == 1,1,0)
athome.no <- ifelse(final.merge$ATHOME == 0,1,0)



gender.male <- ifelse(final.merge$HHSEX == 2,1,0)
gender.female <- ifelse(final.merge$HHSEX == 1,1,0)

employment.unemp <- ifelse(final.merge$EMPLOYHH == 0,1,0)
employment.empft <- ifelse(final.merge$EMPLOYHH == 1,1,0)
employment.emppt <- ifelse(final.merge$EMPLOYHH == 2,1,0)


spouse.yes <- ifelse(final.merge$SPOUSE == 1,1,0)
spouse.no <- ifelse(final.merge$SPOUSE == 0,1,0)

racehisp.yes <- ifelse(final.merge$SDESCENT == 1,1,0)
racehisp.no <- ifelse(final.merge$SDESCENT == 0,1,0)

workpay.yes <- ifelse(final.merge$WORKPAY == 1,1,0)
workpay.no <- ifelse(final.merge$WORKPAY == 0,1,0)

retirepay.yes <- ifelse(final.merge$RETIREPY == 1,1,0)
retirepay.no <- ifelse(final.merge$RETIREPY == 0,1,0)

cashben.yes <- ifelse(final.merge$CASHBEN == 1,1,0)
cashben.no <- ifelse(final.merge$CASHBEN == 0,1,0)

ncashben.yes <- ifelse(final.merge$NCASHBEN == 1,1,0)
ncashben.no <- ifelse(final.merge$NCASHBEN == 0,1,0)

moneypy.l2500 <- ifelse(final.merge$MONEYPY == 1,1,0)
moneypy.2500plus <- ifelse(final.merge$MONEYPY == 2,1,0)
moneypy.5000plus <- ifelse(final.merge$MONEYPY == 3,1,0)
moneypy.7500plus <- ifelse(final.merge$MONEYPY == 4,1,0)
moneypy.10000plus <- ifelse(final.merge$MONEYPY == 5,1,0)
moneypy.15000plus <- ifelse(final.merge$MONEYPY == 6,1,0)
moneypy.20000plus <- ifelse(final.merge$MONEYPY == 7,1,0)
moneypy.25000plus <- ifelse(final.merge$MONEYPY == 8,1,0)
moneypy.30000plus <- ifelse(final.merge$MONEYPY == 9,1,0)
moneypy.35000plus <- ifelse(final.merge$MONEYPY == 10,1,0)
moneypy.40000plus <- ifelse(final.merge$MONEYPY == 11,1,0)
moneypy.45000plus <- ifelse(final.merge$MONEYPY == 12,1,0)
moneypy.50000plus <- ifelse(final.merge$MONEYPY == 13,1,0)
moneypy.55000plus <- ifelse(final.merge$MONEYPY == 14,1,0)
moneypy.60000plus <- ifelse(final.merge$MONEYPY == 15,1,0)
moneypy.65000plus <- ifelse(final.merge$MONEYPY == 16,1,0)
moneypy.70000plus <- ifelse(final.merge$MONEYPY == 17,1,0)
moneypy.75000plus <- ifelse(final.merge$MONEYPY == 18,1,0)
moneypy.80000plus <- ifelse(final.merge$MONEYPY == 19,1,0)
moneypy.85000plus <- ifelse(final.merge$MONEYPY == 20,1,0)
moneypy.90000plus <- ifelse(final.merge$MONEYPY == 21,1,0)
moneypy.95000plus <- ifelse(final.merge$MONEYPY == 22,1,0)
moneypy.100000plus <- ifelse(final.merge$MONEYPY == 23,1,0)
moneypy.120000plus <- ifelse(final.merge$MONEYPY == 24,1,0)


pca.household.df <- cbind(homebusiness.yes,homebusiness.no,otherwork.yes,otherwork.no,
                          athome.yes,athome.no,gender.male,gender.female,
                          employment.unemp,employment.empft,employment.emppt,
                          spouse.yes,spouse.no,racehisp.yes,racehisp.no,workpay.yes,workpay.no,
                          retirepay.yes,retirepay.no,cashben.yes,cashben.no,ncashben.yes,ncashben.no,
                          moneypy.l2500,moneypy.2500plus,moneypy.5000plus,moneypy.7500plus,
                          moneypy.10000plus,moneypy.15000plus,moneypy.20000plus,moneypy.25000plus,
                          moneypy.30000plus,moneypy.35000plus,moneypy.40000plus,moneypy.45000plus,
                          moneypy.50000plus,moneypy.55000plus,moneypy.60000plus,moneypy.65000plus,
                          moneypy.70000plus,moneypy.75000plus,moneypy.80000plus,moneypy.85000plus,
                          moneypy.90000plus,moneypy.95000plus,moneypy.100000plus,moneypy.120000plus)

####Household Table Ends######

pca.df <- cbind.data.frame(pca.energy.df,pca.house.df,pca.household.df)

final.merge <- cbind.data.frame(final.merge,pca.df)


final.merge <- subset(final.merge, select = -c(LRGSTATE,TYPEHUQ,WALLTYPE,YEARMADE,URBRUR,HBUSNESS,OTHWORK,ATHOME,HHSEX,
                                        EMPLOYHH,SPOUSE,SDESCENT,WORKPAY,RETIREPY,CASHBEN,NCASHBEN,MONEYPY))





corr.column <- c("AGEHHMEM2",	
                 "AGEHHMEM3",
                 "AGEHHMEMY",
                 "athome.no",
                 "athome.yes",
                 "ATTHSQFT",
                 "ATTUCSQFT",
                 "ATTUSQFT",
                 "BASHSQFT",
                 "BASUCSQFT",
                 "BASUSQFT",
                 "cashben.no",
                 "cashben.yes",
                 "CD65",
                 "employment.empft",
                 "employment.emppt",
                 "GARCSQFT",
                 "GARUCSQFT",
                 "GARUSQFT",
                 "gender.female",
                 "gender.male",
                 "HD65",
                 "HHAGE",
                 "home.ty.5plus",
                 "home.ty.att",
                 "home.ty.det",
                 "home.ty.mob",
                 "homebusiness.no",
                 "NHSLDMEM",
                 "otherwork.no",
                 "otherwork.yes",
                 "racehisp.no",
                 "racehisp.yes",
                 "retirepay.no",
                 "retirepay.yes",
                 "RHMHSQFT",
                 "RHMUCSQFT",
                 "RHMUSQFT",
                 "spouse.no",
                 "spouse.yes",
                 "state.ca",
                 "state.fl",
                 "state.ny",
                 "state.other",
                 "state.tx",
                 "TOTATTCSQFT",
                 "TOTBASESQFT",
                 "TOTCSQFT",
                 "TOTGARGSQFT",
                 "TOTHSQFT",
                 "TOTRHMSQFT",
                 "TOTSQFT",
                 "TOTUCSQFT",
                 "TOTUSQFT",
                 "wall.brick",
                 "wall.concrete",
                 "wall.glass",
                 "wall.shingle",
                 "wall.stucco",
                 "workpay.no",
                 "workpay.yes")
pc.merge <- final.merge[, names(final.merge) %in% corr.column]
pc.result <- prcomp(pc.merge, retx = TRUE, center = TRUE, scale = FALSE)
#screeplot(pc.result, type = "lines")
#View(pc.result$x)

df.new <- final.merge[, !names(final.merge) %in% corr.column]
df.new <- cbind(df.new, pc.result$x[,1:39])
final.pca1 <- do_pca(df.new,'BTUEL',10,get_pred_rf)



M <- cor(final.merge)
correlation <- cor(final.merge, use = "complete.obs", method="pearson")
corrplot(M)
corrplot.mixed(M)

Models<-c('Linear Regression','Random Forest','RF + Linear Regression','RF+RF','PCA + Linear Regression','PCA + RF','Default')
RMSE<- c(19761.77,19688.59,19908.89,19631.40,19706.16,19916.23,25638.44)
df <- cbind.data.frame(Models,RMSE)

df <-df[order(df$Models),]

ggplot(data = df, aes(reorder(Models, RMSE), RMSE))+geom_bar(stat = "identity",width = 0.6,fill="#9999EE")+ xlab("Models") + ylab("RMSE(MBTU)") +
  theme(axis.text.x = element_text(hjust = 1 ,angle = 30)) + ggtitle("Root Mean Squared Error in Million British Thermal Units") + geom_text(aes(label=RMSE),vjust=-1,size =4,color="navy")
  #coord_cartesian(ylim=c(0.6,0.9)) + geom_text(aes(label=RMSE), vjust=-1,size =5,color="navy") +
  #theme(axis.text=element_text(size=10),axis.title  =element_text(size=10),
   #     plot.title = element_text(hjust = 0.5,size=10)) + 



# Linear model
# Assumes the last column of data is the output dimension
get_pred_lr <- function(train,test){
  set.seed(10)
  nf <- ncol(train) # find total columns in Training data
  model <- lm(paste(names(train)[nf], '~ .'),data = train) # linear regression on the last column using all the other ones
  return(predict.lm(model,newdata = test)) # predicting the values of test data using the model generated above
  
}

get_pred_rf<-function(train,test){
  nf <- ncol(train)
  test_val<-as.data.frame(test[,-nf])
  names(train)[nf]<-"out"
  rf = randomForest(out ~ .  ,data=train,nodesize = 16,mtry = 17,importance =TRUE, na.rm=TRUE,ntree = 101)
  pred = predict(rf, newdata=test_val)
  return(pred)
} 


do_pca <- function(df,output,k,model){
  set.seed(10)
  df.intr <- df[, -which(names(df) %in% output)] # Find output column and move it to the last 
  df.intr <- cbind(df.intr,df[output])
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
  print(mean(mse.vector))
  print(mean(rmse.vector))
  print(mean(r.squared))
  return(df.act.final)
}



####################PCA STUFF####################################################
out.pca <-prcomp(final.merge[,-5],retx = TRUE, center = TRUE, scale = FALSE)
screeplot(out.pca, type = 'lines')

variances <- out.pca$sdev * out.pca$sdev
tot.var <- sum(variances)

df.final <- as.data.frame(cbind(out.pca$x[,1:100],final.merge[,5]))
colnames(df.final)[101] <- 'BTUEL'

final.pca1 <- do_pca(df.new,'BTUEL',10,get_pred_lr)
final.pca <- do_pca(final.merge,'BTUEL',10,get_pred_rf)

  colnames(final.pca) <- c('actual','predicted')
  colnames(final.pca1) <- c('actual','predicted')
pca.rf <- ggplot(final.pca,aes(y = predicted, x = actual)) + geom_point() +geom_smooth() + 
  ggtitle("BTUEL -> PCA + RF")+labs(x = "Actual Values", y= "Predicted Values") 

##########################################################################################

## Now create a dataframe to store percentage values for all principal components
var.per <- cbind(cumsum(variances)/tot.var, (1:length(variances)))

#plotting top PC
top <-as.data.frame(var.per)
colnames(top) <- c('Variance_Percent','PC_Index')

ggplot(top, aes(x = PC_Index, y = Variance_Percent * 100)) + geom_point() +xlab("Principal Component") +
  ylab("Cumulative Varience Percent") +
  ggtitle("Cumulative Varience")

# plotting just the top 5 which gives almost 92% cummulatiive varience

top.five <- top[1:5,]
ggplot(top.five, aes(x = PC_Index, y = Variance_Percent * 100)) + geom_line() +xlab("Principal Component") +
  ylab("Cumulative Varience Percent") +
  ggtitle("Cumulative Varience")



