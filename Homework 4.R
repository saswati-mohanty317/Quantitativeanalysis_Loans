
#Read data 
df <- read.csv("~/R/credit3 (1).csv")
df$PROFITABLE <- ifelse(df$NPV>0,1,0)
library("fastDummies")
df[,1] <- NULL
df$AMOUNT_REQUESTED <- lapply(df$AMOUNT_REQUESTED,gsub,pattern = ",",fixed = TRUE,replacement="")
df$AMOUNT_REQUESTED <- as.numeric(as.character(df$AMOUNT_REQUESTED))
df$CREDIT_EXTENDED <- lapply(df$AMOUNT_REQUESTED,gsub,pattern = ",",fixed = TRUE,replacement="")
df$CREDIT_EXTENDED <- as.numeric(as.character(df$AMOUNT_REQUESTED))

df$CREDIT_EXTENDED <- NULL

#Creating dummy variables 
df <-dummy_cols(df,select_columns=c("CHK_ACCT","SAV_ACCT","HISTORY","JOB","TYPE"))
df[,2] <- NULL
df[,2]<- NULL
df[,4] <- NULL
df[,6] <- NULL
df[,15] <- NULL
df$NPV <- NULL
df
#Normalisation function
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
} 
df[,1:15] <- apply(df[,1:15], 2, fun)
df[,17:41] <- apply(df[,17:41],2,fun)

#setting seed and splitting the data into train and validation
set.seed(12345)
train <- sample(nrow(df), 0.7*nrow(df))
dftrain <- df[train,] 
dfvalidation <- df[-train,]

train_input <- as.matrix(dftrain[,-16])
train_output <- as.vector(dftrain[,16])
validation_input <- as.matrix(dfvalidation[,-16])

library(class)

# KNN algorithm for classification, testing all values of k from 1 to 15
kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output, k=i)
  prediction2 <- knn(train_input, validation_input,train_output, k=i)
  #
  # The confusion matrix for training data is:
  CM1 <- table(prediction, dftrain$PROFITABLE)
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(prediction2, dfvalidation$PROFITABLE)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}
#Graph of error rate of training and validation sample
plot(c(1,kmax),c(0,0.5),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(9, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)

#Error rate minimized at a certain k value
cat("Minimum Validation Error k:", z)

####
# Error rate calculated at k=14
prediction <- knn(train_input, train_input,train_output, k=14)
prediction2 <- knn(train_input, validation_input,train_output, k=14)
# The confusion matrix for validation data is: 
CM2 <- table(prediction2, dfvalidation$PROFITABLE)
ER2 <- (CM2[1,2]+CM2[2,1])/sum(CM2)
ER2
CM2
#
Sensi<-(CM2[2,2]/(CM2[2,1]+CM2[2,2]))
Speci<-(CM2[1,1]/(CM2[1,1]+CM2[1,2]))
Sensi
Speci

#Classifying a specific applicant given in the qts.
new_applicant <- data.frame("AGE"=27,"NUM_CREDITS"=1,"DURATION"=12,"PRESENT_RESIDENT"=1,"EMPLOYMENT"=1,"NUM_DEPENDENTS"=0,"RENT"=1,"INSTALL_RATE"=3,"GUARANTOR"=0,"OTHER_INSTALL"=0,"OWN_RES"=0,"TELEPHONE"=1,"FOREIGN"=0,"REAL_ESTATE"=0,"AMOUNT_REQUESTED"=4500,"CHK_ACCT_0"=0,"CHK_ACCT_1"=1,"CHK_ACCT_2"=0,"CHK_ACCT_3"=0,"SAV_ACCT_0"=0,"SAV_ACCT_1"=0,"SAV_ACCT_2"=0,"SAV_ACCT_3"=0,"SAV_ACCT_4"=1,"HISTORY_0"=0,"HISTORY_1"=1,"HISTORY_2"=0,"HISTORY_3"=0,"HISTORY_4"=0,"JOB_0"=0,"JOB_1"=0,"JOB_2"=1,"JOB_3"=0,"TYPE_0"=0,"TYPE_1"=0,"TYPE_2"=1,"TYPE_3"=0,"TYPE_4"=0,"TYPE_5"=0,"TYPE_6"=0)
new_applicant
validation_input_new <- as.matrix(new_applicant)
validation_input_new
prediction_new <- knn(train_input, validation_input_new,train_output, k=14)
prediction_new
predicted.probability <- attr(prediction_new, "prob")
predicted.probability


#Predicted probability that account is profitable.



#Computing lift chart for best case senario i.e. k=14 
####

prediction3 <- knn(train_input, validation_input, train_output, k=14, prob=T)
#
predicted.probability <- attr(prediction3, "prob")
# 
# This (unfortunately returns the proportion of votes for the winning class - P(Success))
#
predicted.probability.knn <- ifelse(prediction3 ==1, predicted.probability, 1-predicted.probability)
#
df1 <- data.frame(prediction3, predicted.probability.knn,dfvalidation$PROFITABLE)
# When prediction is 1, we will use predicted.probability; else use 1-predicted.probability
df1S <- df1[order(-predicted.probability.knn),]
df1S$Gains <- cumsum(df1S$dfvalidation.PROFITABLE)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$dfvalidation.PROFITABLE)/nrow(df1S),lty = 2, col="red")

#finding for new applicant
new_df=data.frame(
  "OBS."=1001,
  "AGE"=27,
  "CHK_ACCT"=1,
  "SAV_ACCT"=4,
  "NUM_CREDITS"=1,
  "DURATION"=12,
  "HISTORY"=1,
  "PRESENT_RESIDENT"=1,
  "EMPLOYMENT"=1,
  "JOB"=2,
  "NUM_DEPENDENTS"=1,
  "RENT"=1,
  "INSTALL_RATE"=3,
  "GUARANTOR"=0,
  "OTHER_INSTALL"=0,
  "OWN_RES"=0,
  "TELEPHONE"=1,
  "FOREIGN"=0,
  "REAL_ESTATE"=0,
  "TYPE"=2,
  "AMOUNT_REQUESTED"=4500,
  "CREDIT_EXTENDED"=9999,
  "NPV"=0
  
)





### Naive Bayes

df <- read.csv("credit3.csv")
df$PROFITABLE <- ifelse(df$NPV>0,1,0)
df[,1] <- NULL
df$AMOUNT_REQUESTED <- lapply(df$AMOUNT_REQUESTED,gsub,pattern = ",",fixed = TRUE,replacement="")
df$AMOUNT_REQUESTED <- as.numeric(as.character(df$AMOUNT_REQUESTED))
df$CREDIT_EXTENDED <- lapply(df$AMOUNT_REQUESTED,gsub,pattern = ",",fixed = TRUE,replacement="")
df$CREDIT_EXTENDED <- as.numeric(as.character(df$AMOUNT_REQUESTED))


df$CHK_ACCT <- as.factor(df$CHK_ACCT)
df$SAV_ACCT <- as.factor(df$SAV_ACCT)
df$NUM_CREDITS <- as.factor(df$NUM_CREDITS)
df$HISTORY <- as.factor(df$HISTORY)
df$PRESENT_RESIDENT <- as.factor(df$PRESENT_RESIDENT)
df$EMPLOYMENT <- as.factor(df$EMPLOYMENT)
df$JOB <- as.factor(df$JOB)
df$NUM_DEPENDENTS <- as.factor(df$NUM_DEPENDENTS)
df$RENT <- as.factor(df$RENT)
df$INSTALL_RATE <- as.factor(df$INSTALL_RATE)
df$GUARANTOR <- as.factor(df$GUARANTOR)
df$OTHER_INSTALL <- as.factor(df$OTHER_INSTALL)
df$OWN_RES <- as.factor(df$OWN_RES)
df$TELEPHONE <- as.factor(df$TELEPHONE)
df$FOREIGN <- as.factor(df$FOREIGN)
df$REAL_ESTATE <- as.factor(df$REAL_ESTATE)
df$TYPE <- as.factor(df$TYPE)
df$CREDIT_EXTENDED <- as.factor(df$CREDIT_EXTENDED)
df$PROFITABLE <- as.factor(df$PROFITABLE)
df$NPV <- NULL

set.seed(12345)
train <- sample(nrow(df), 0.7*nrow(df))
dftrain <- df[train,] 
dfvalidation <- df[-train,]

library(e1071)

model <- naiveBayes(PROFITABLE~., data=dftrain)
model
prediction <- predict(model, newdata = dfvalidation[,-22])
table(dfvalidation$PROFITABLE,prediction,dnn=list('actual','predicted'))
model$apriori

####

predicted.probability <- predict(model, newdata = dfvalidation[,-22], type="raw")
predicted.probability.NB <- predicted.probability[,2]

#
# The first column is class 0, the second is class 1
PL <- as.numeric(dfvalidation$PROFITABLE)-1
prob <- predicted.probability[,2]
df1 <- data.frame(prediction, PL, prob)
#
#Lift chart for naive bayes
df1S <- df1[order(-prob),]
df1S$Gains <- cumsum(df1S$PL)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success")
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$PL)/nrow(df1S),lty = 2, col="red")



### Logistic regression

df <- read.csv("~/R/credit3.csv")
df[,1] <- NULL
df$CREDIT_EXTENDED <- NULL
df$PROFITABLE<-ifelse(df$NPV>0,1,0)
df$NPV <- NULL

df$CHK_ACCT<-as.factor(df$CHK_ACCT)
df$SAV_ACCT<-as.factor(df$SAV_ACCT)
df$HISTORY<-as.factor(df$HISTORY)
df$JOB<-as.factor(df$JOB)
df$TYPE<-as.factor(df$TYPE)

set.seed(12345)
train <- sample(nrow(df), 0.7*nrow(df))
dftrain <- df[train,] 
dftest <- df[-train,]

logisticmodel<-glm(PROFITABLE~.,data=dftrain,family = "binomial")

predicted.probability.train <- predict(logisticmodel, type = "response")

predicted.probability.test <- predict(logisticmodel, type = "response",newdata = dftest) 

predictedclass_test <- ifelse(predicted.probability.test>0.5,1,0)

table(dftest$PROFITABLE,predictedclass_test,dnn=list('actual','predicted'))


# ROC Curves
par(pty="s")
library(pROC)
roc_rose <- plot(roc(dftest$PROFITABLE, predicted.probability.knn), 
                 print.auc = TRUE, col = "blue",legacy.axes=T)


roc_rose <- plot(roc(dftest$PROFITABLE, predicted.probability.NB), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE, legacy.axes=T)

roc_rose <- plot(roc(dftest$PROFITABLE, predicted.probability.test), print.auc = TRUE, 
                 col = "red", print.auc.y = .3, add = TRUE, legacy.axes=T)

legend(9, 0.1, c("KNN","Naive Baiyes","logistic reg"),lty=c(1,1), col=c("blue","green","red"))

