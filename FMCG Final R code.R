campaigndetails <- read.csv("C:/Users/Smita Gavandi/Documents/FMCG data/Campaign Details.csv")


length(unique(campaigndetails$CustomerID))
head(campaigndetails)
dim(campaigndetails)
str(campaigndetails)


#Use file Campaign Response data_changed
campresponse <- read.csv("C:/Users/Smita Gavandi/Documents/FMCG data/Campaign Response data_changed.csv")
length(unique(campresponse$CustomerID))

head(campresponse)
dim(campresponse)
str(campresponse)

#Use file Transactions

transactionsdata <- read.csv("C:/Users/Smita Gavandi/Documents/FMCG data/Transactions.csv")

head(transactionsdata)
dim(transactionsdata)
str(transactionsdata)

#Use Master Lookup

region <- read.csv("C:/Users/Smita Gavandi/Documents/FMCG data/MasterLookUp.csv")

head(region)
dim(region)
str(region)


library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(ROCR)
library(caret)
library(e1071)
library(randomForest)

##############Files Merging########################################################


Master_FMCG <-  NULL

#There are some duplicate ids ,so transforming them to single row by agrgregating:
campresponse_agg <- aggregate(cbind(response,n_comp,loyalty,portal,rewards,nps,n_yrs)~CustomerID ,data = campresponse,FUN=mean)

dim(campresponse_agg)

#####Rounding off all variables in a datframe to 0 excluding the first column i.e CustomerID

campresponse_agg[,-1] <- round(campresponse_agg[,-1],0)

View(campresponse_agg)

names(campresponse_agg)
table(campresponse_agg$response)
table(campresponse_agg$n_comp)
table(campresponse_agg$loyalty)
table(campresponse_agg$portal)
table(campresponse_agg$rewards)
table(campresponse_agg$nps)
table(campresponse_agg$n_yrs)



Master_FMCG<- merge(campresponse_agg,region,by=c("CustomerID"),all.x = T)

head(Master_FMCG)
dim(Master_FMCG)


#There are some duplicate ids in campaigndetails data file ,so transforming them to single row by agrgregating:


campaigndetails_agg <- aggregate(cbind(email,sms,call)~CustomerID ,data = campaigndetails,FUN=mean)

dim(campaigndetails_agg)
View(campaigndetails_agg)

#####Rounding off all variables in a datframe to 0 excluding the first column i.e CustomerID

campaigndetails_agg[,-1] <- round(campaigndetails_agg[,-1],0)


table(campaigndetails_agg$email)
table(campaigndetails_agg$sms)
table(campaigndetails_agg$call)

table(campaigndetails$email)
table(campaigndetails$sms)
table(campaigndetails$call)


Master_FMCG<- merge(Master_FMCG,campaigndetails_agg,by=c("CustomerID"),all.x = T)

head(Master_FMCG)
dim(Master_FMCG)



#############Deriving new variables#########################################

#transactionsdata_n <- NULL
#transactionsdata_n <- transactionsdata %>% filter(Month >=10 & Year==2014)
#head(transactionsdata_n)
#dim(transactionsdata_n)

head(transactionsdata)

transactionsdata_n1 <- NULL


transactionsdata_n1 <- aggregate(Sales~CustomerID,data=transactionsdata_n,FUN=sum)

head(transactionsdata_n1)


transactionsdata_n2 <- transactionsdata %>% filter(Year==2014) %>% group_by(CustomerID) %>% summarize(Annualsales=sum(Sales))

head(transactionsdata_n2)


transactionsdata_n3 <- transactionsdata %>% filter(Year==2014 & Brand=="B1") %>% group_by(CustomerID) %>% summarize(Sales_B1=sum(Sales))

head(transactionsdata_n3)

transactionsdata_n4 <- transactionsdata %>% filter(Year==2014) %>% group_by(CustomerID) %>% summarize(buyingfreq=length(CustomerID))

head(transactionsdata_n4)

transactionsdata_n5 <- transactionsdata %>% filter(Year==2014 & Brand=="B1") %>% group_by(CustomerID) %>% summarize(buyingfreq_B1=length(CustomerID))

head(transactionsdata_n5)

transactionsdata_n6 <- transactionsdata %>% filter(Year==2014) %>% group_by(CustomerID) %>% summarize(brandengagement=length(unique(Brand)))

head(transactionsdata_n6)

transactionsdata8 <- transactionsdata %>% filter(Month >=10 & Year==2014) %>% group_by(CustomerID) %>% summarize(sales_Q4=sum(Sales))

head(transactionsdata8)

Master_FMCG<- merge(Master_FMCG,transactionsdata_n1,by=c("CustomerID"),all.x = T)
Master_FMCG<- merge(Master_FMCG,transactionsdata_n2,by=c("CustomerID"),all.x = T)
Master_FMCG<- merge(Master_FMCG,transactionsdata_n3,by=c("CustomerID"),all.x = T)

Master_FMCG$Pcontri_B1 <- round((Master_FMCG$Sales_B1/Master_FMCG$Annualsales) * 100,2)

View(Master_FMCG)
dim(Master_FMCG)

Master_FMCG<- merge(Master_FMCG,transactionsdata_n4,by=c("CustomerID"),all.x = T)

View(Master_FMCG)


Master_FMCG<- merge(Master_FMCG,transactionsdata_n5,by=c("CustomerID"),all.x = T)

View(Master_FMCG)

Master_FMCG<- merge(Master_FMCG,transactionsdata_n6,by=c("CustomerID"),all.x = T)

View(Master_FMCG)

Master_FMCG<- merge(Master_FMCG,transactionsdata8,by=c("CustomerID"),all.x = T)

View(Master_FMCG)
dim(Master_FMCG)

####To calculate variable 'growth' 

transaction7 <- aggregate(Sales~CustomerID+Year,data=transactionsdata,FUN=sum)


transaction7<-transaction7[order(transaction7$CustomerID),] #To bring two years values of single customer back to back

View(transaction7)

changedf <- data.frame(Change=c(NA,diff(transaction7$Sales))) #New dataframe with differenced values - Yt minus Yt-1


View(changedf)

transaction7new <- cbind(transaction7,changedf) #Bind transaction data with new dataframe

View(transaction7new)

transaction7final <- subset(transaction7new,Year==2014,select = c(-Year, -Sales)) #2014 subset has the correct difference values

head(transaction7final)

setnames(transaction7final,"Change","growth")

Master_FMCG<- merge(Master_FMCG,transaction7final,by=c("CustomerID"),all.x = T)

View(Master_FMCG)
dim(Master_FMCG)




###Removal of NA's from the master file

View(summary(Master_FMCG))

View(Master_FMCG)
dim(Master_FMCG)

sum(is.na(Master_FMCG))
Master_FMCG[is.na(Master_FMCG)] <- 0
sum(is.na(Master_FMCG))

summary(Master_FMCG)
View(Master_FMCG)
table(Master_FMCG$response)


###########Variables under study

str(Master_FMCG)

#Dependant Variable
response 

#Independant Variables
Annual Sales - "Annualsales"
Sales in Q4 2014 - "sales_q4"
Sales in Brand1 - "Sales_B1"
%Contribution of brand1 - "Pcontri_B1"
Association with the client - "n_yrs"
Buying frequency - "buyingfreq"
Buying frequency for B1 - "buyingfreq_B1"
Region - "Region"
Net promotion score - "nps"
Loyalty - "loyalty"
Portal membership - "portal"
Satisfaction level(Number of complaiants) - "n_comp"
Communication Channels - "email","sms","call"
Brandengagement - "brandengagement"
Growth - "growth"


write.csv(Master_FMCG,file="Masterfile.csv")


###################Exploratory Analysis##################################################

###Boxplots

boxplot(Master_FMCG$n_comp,col="green",main="Number of Complaints")

boxplot(Master_FMCG$buyingfreq,col="blue",main="Buying Frequency")

boxplot(Master_FMCG$buyingfreq_B1,col="blue",main="Buying Frequency for B1")

boxplot(Master_FMCG$Annualsales,col="red",main="Annual Sales")

boxplot(Master_FMCG$growth,col="grey",main="Growth")

###Pie chart for Region


regiontable <-table(Master_FMCG$Region)

C2<-prop.table(regiontable)

round(C2,2)

labs <- paste("(",names(C2),")","\n",round(C2,2)*100,"%",sep="")

pie(regiontable,labels=labs,col=rainbow(length(C2)),explode = 0.2,cex=0.8,main="Distribution Of Customers By Region")

#Association with the Client by Region

agg_years <- aggregate(n_yrs~Region,Master_FMCG,FUN = mean)

agg_years$n_yrs <- round(agg_years$n_yrs,2)

agg_years

ggplot(agg_years,aes(x=Region,y=n_yrs))+
  geom_bar(stat="identity",fill="purple")+
  scale_y_continuous(limits = c(0,10))+
  geom_text(aes(label=n_yrs),vjust=-0.5,colour="black",size=4)+
  labs(x="Region",y="Average Number Of Years",title="Association with the Client By Region")+
  theme_bw()



#Distribution of Buying Frequency of Brand1 by Number of Association years

buyingfreqB1byyears <- aggregate(buyingfreq_B1~n_yrs,Master_FMCG,FUN=sum)

#Buying Frequnecy for Brand1 seems to be higher for the customers who have 4 and 5 years of association. Frequency is lower in the initial years of collaboration.

ggplot(buyingfreqB1byyears,aes(x=n_yrs,y=buyingfreq_B1))+
geom_bar(stat="identity",fill="light green")+
labs(x="Numbers of Years Of Association",y="Buying Frequency",title="Distribution of Buying Frequency by Association years")+
theme_bw()



####Chart for Regionwise contribution for B1

contriB1 <- Master_FMCG %>% group_by(Region) %>% summarize(percent=(sum(Sales_B1)/sum(Annualsales))*100)
contriB1_table <- as.data.frame(contriB1)
contriB1_table$percent <- round(contriB1_table$percent,2)


ggplot(contriB1_table,aes(x=Region,y=percent))+
geom_bar(stat="identity",fill="hotpink2")+
labs(x="Region",y="Percent Contribution for B1",title="Percent Contribution of Brand1 by Region")+
theme_bw()


#Distribution of the different Communication Channels by Number of Customers /Distribution of Communication Channels

X<-aggregate(CustomerID ~ sms+call+email,data=Master_FMCG,FUN=length)

X2<-aggregate(CustomerID ~ sms+call+email+response,data=Master_FMCG,FUN=length)

X3 <- X2 %>% filter(response==1)

CrossTable(master$sms,master$response)
CrossTable(master$call,master$response)
CrossTable(master$email,master$response)





##Brandwise Annual Sales per Month

transactionsdata_2014 <- transactionsdata %>% filter(Year==2014) 

head(transactionsdata_2014)

Month <- aggregate(Sales~Month+Brand,data=transactionsdata_2014,FUN=mean)

View(Month)

ggplot(Month,aes(x=Month,y=Sales,col=Brand))+
  geom_line(size=1)+
  geom_point(size=2)+
  labs(y="Average Sales",title="Brandwise Monthly Sales trend - Year 2014")+
  theme_bw()


#######glm model

#Global Testing :

null <- glm(response~1,data=Master_FMCG,family=binomial)

anova(null,fmcg_glmmodel,test="Chisq")


#Predicting Probabilities
Master_FMCG$predprob <- round(fitted(fmcg_glmmodel),2)

head(Master_FMCG)




###n_yrs,nps,email,sms,call are significant variables since the pvalues are <0.05. Rerun the model using significant predictors

fmcg_glmmodel <- glm(response ~ n_yrs+nps+email+sms+call,data=Master_FMCG,family=binomial)
summary(fmcg_glmmodel)

#Predicting Probabilities on revised model
Master_FMCG$predprob <- round(fitted(fmcg_glmmodel),2)

View(Master_FMCG)



###Boxplots for Significant predictors nps, n_yrs

boxplot(n_yrs~response,data=Master_FMCG,col=c("darkred","darkblue"),xlab="Response to the Campaign",ylab="Number of Years Of Association with the Company")


boxplot(nps~response,data=Master_FMCG,col=c("lightgreen","dark green"),xlab="Response to the Campaign",ylab="NPS Score")

ggplot(Master_FMCG,aes(x=nps))+
  geom_bar(aes(fill=response),position="dodge")+
  labs(x="NPS Score",y="Number of Customers")


#Classification table

table(Master_FMCG$response)

Cutoff=477/1228=0.39

###Classification table for 0.5 cutoff

table(Master_FMCG$response,fitted(fmcg_glmmodel)>0.5)

FALSE TRUE
0   630  121
1   275  202

misclassification rate= 121+275/1228=32%

Sensitivity=42%
Specificity=83%

###Classification table for 0.4 cutoff

table(Master_FMCG$response,fitted(fmcg_glmmodel)>0.4)

FALSE TRUE
0   502   249
1   191   286

Sensitivity=60%
Specificity=67%


###Classification table for 0.39 cutoff

table(Master_FMCG$response,fitted(fmcg_glmmodel)>0.39)

FALSE TRUE
0   491  260
1   181  296

Sensitivity=62%
Specificity=65%



###Classification table for 0.37 cutoff

table(Master_FMCG$response,fitted(fmcg_glmmodel)>0.37)

FALSE TRUE
0   461  290
1   162  315

Sensitivity=66%
Specificity=61%

#Optimum cut off value is 0.39

#ROC

library(ROCR)
Master_FMCG$predprob <- fitted(fmcg_glmmodel)
pred <- prediction(Master_FMCG$predprob,Master_FMCG$response)
perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(0,1)

auc<-performance(pred,"auc")
auc@y.values


###Odds Ratio

coef(fmcg_glmmodel)

exp(coef(fmcg_glmmodel))

cbind(odds_ratio = exp(coef(fmcg_glmmodel)),exp(confint(fmcg_glmmodel)))

#Hold Out cross-validation



library(caret)
index <- createDataPartition(Master_FMCG$response,p=0.8,list=F)

head(index)

traindata <- Master_FMCG[index,]
testdata <- Master_FMCG[-index,]

dim(traindata)
dim(testdata)

traindata$predprob <- predict(fmcg_glmmodel,traindata,type='response')
traindata$predY <- ifelse(traindata$predprob>0.39,1,0)

confusionMatrix(traindata$predY,traindata$response,positive = "1")

#ROC Curve for training data

traindata$predprob <- predict(fmcg_glmmodel,traindata,type='response')
pred <- prediction(traindata$predprob,traindata$response)

perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(0,1)

auc <- performance(pred,"auc")

auc@y.values

0.7032%

#ROC curve on testing data

testdata$predprob <- predict(fmcg_glmmodel,testdata,type='response')

pred <- prediction(testdata$predprob,testdata$response)

perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(0,1)

auc <- performance(pred,"auc")

auc@y.values

0.6723%

#Kfold Cross validation- Binary Logistic

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

traindata$response <- as.factor(traindata$response)


str(traindata)


glmmod_fit <- train(response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                        buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                        sms+call+rewards+brandengagement+growth,data=traindata, method="glm", family="binomial",
                      trControl = ctrl, tuneLength = 5)


predg <- predict(glmmod_fit, newdata=testdata)

head(predg)

traindata$predprob <- fitted(glmmod_fit)

head(traindata)
confusionMatrix(predg, testdata$response)
pred <- prediction(traindata$predprob,traindata$response)

perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc <- performance(pred,"auc")
auc@y.values


#NAIVE Bayes


naivemodel <- naiveBayes(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                           buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                           sms+call+rewards+brandengagement+growth,data=master)



naivemodel

#Predicted Probabilities and ROC Curve

prednb <- predict(naivemodel,master,type='raw')

prednb
pred <- prediction(prednb[,2],master$response)

perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(0,1)

auc <- performance(pred,"auc")

auc@y.values

Model=71%
Traindata=71%
Testdata=69%


#Train Data


naivemodel_train <- naiveBayes(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                                 buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                                 sms+call+rewards+brandengagement+growth,data=traindata)


dim(traindata)
naivemodel_train

#Predicted Probabilities and ROC Curve

prednb <- predict(naivemodel_train,traindata,type='raw')

prednb
pred <- prediction(prednb[,2],traindata$response)

perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(0,1)

auc <- performance(pred,"auc")

auc@y.values

#Test Data

naivemodel_test <- naiveBayes(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                                buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                                sms+call+rewards+brandengagement+growth,data=testdata)


dim(testdata)
naivemodel_test

#Predicted Probabilities and ROC Curve

prednb <- predict(naivemodel_test,testdata,type='raw')

prednb
pred <- prediction(prednb[,2],testdata$response)

perf <- performance(pred,"tpr","fpr")

plot(perf)
abline(0,1)

auc <- performance(pred,"auc")

auc@y.values

# Kfold Cross Validation -Naive Bayes


# Convert 'response' to factor

master$response <- as.factor(master$response)
master$response <- as.numeric(master$response)



# Remove irrelevant variables from the data


testdata$predprob<-NULL

testdata$predY<-NULL



# Create k-folds

kfolds<-trainControl(method="cv",number=10)



naivemodel <- train(response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                      
                      buyingfreq+buyingfreq_B1+nps+loyalty+portal+n_comp+email+
                      
                      sms+call+rewards+brandengagement+growth,data=traindata,
                    
                    method="nb",trControl=kfolds)



# Predicted Probabilities

predk_n<-as.data.frame(predict(naivemodel$finalModel,testdata))


# Check the object format

head(predk)
dim(predk)

#Confusion Matrix

testdata$predY_n <- ifelse(predk_n[,3]>0.39,1,0) 

confusionMatrix(testdata$predY_n,testdata$response)


# ROC Curve and Area Under Curve

library(ROCR)

pred<-prediction(predk_n[,3],testdata$response)

perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc<-performance(pred,"auc")
auc@y.values



#RANDOM FOREST


library("randomForest")


testdata$response <- as.factor(master$response)
class(testdata$response)


forestmodel <- randomForest(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                              buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                              sms+call+rewards+brandengagement+growth,data=master,
                            mtry=4,ntree=500,importance=T,cutoff=c(0.61,0.39))



forestmodel
plot(forestmodel)

forestmodel$importance

varImpPlot(forestmodel,col="blue")


pred <- prediction(forestmodel$votes[,2],master$response)
perf <-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc <- performance(pred,"auc")
auc@y.values


#Train Data

traindata$response <- as.factor(traindata$response)
forestmodel_train <- randomForest(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                                    buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                                    sms+call+rewards+brandengagement+growth,data=traindata,
                                  mtry=4,ntree=500,importance=T,cutoff=c(0.61,0.39))


forestmodel_train


predtree <- predict(forestmodel_train,traindata,type="prob")

pred <- prediction(forestmodel_train$votes[,2],traindata$response)
perf <-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc <- performance(pred,"auc")
auc@y.values


#Test Data


testdata$response <- as.factor(testdata$response)
forestmodel_test <- randomForest(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                                   buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                                   sms+call+rewards+brandengagement+growth,data=testdata,
                                 mtry=4,ntree=500,importance=T,cutoff=c(0.61,0.39))


forestmodel_test

predtree <- predict(forestmodel_test,testdata,type="prob")

pred <- prediction(forestmodel_test$votes[,2],testdata$response)
perf <-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc <- performance(pred,"auc")
auc@y.values




####K fold Cross Validation for Random Forest 


trControl <- trainControl(method="cv",number=10)

set.seed(1234)


fit_rf_train <- train(response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                        
                        buyingfreq+buyingfreq_B1+nps+loyalty+portal+n_comp+email+
                        
                        sms+call+rewards+brandengagement+growth,data=traindata,
                      
                      method="rf",metric="Accuracy",trControl=trControl,ntree=300,importance=T)

summary(fit_rf_train)

predk <- NULL
predk <- predict(fit_rf_train,testdata,type="prob")

dim(predk)
testdata$predprob <- NULL
testdata$predYn<- NULL
testdata$predY <- NULL
testdata$predYr<-NULL

#How accuartely our model is working?

testdata$predYr <- ifelse(predk[,2]>0.39,1,0) 

confusionMatrix(testdata$predYr,testdata$response)

varImp(fit_rf_train)

####ROC Curve

pred<-prediction(predk[,2],testdata$response)

perf<-performance(pred,"tpr","fpr")

plot(perf)

abline(0,1)

auc<-performance(pred,"auc")

auc@y.values

#####SVM Method

model_svm <- svm(formula= response ~ Annualsales+sales_Q4+Sales_B1+Pcontri_B1+n_yrs+
                   buyingfreq+buyingfreq_B1+Region+nps+loyalty+portal+n_comp+email+
                   sms+call+rewards+brandengagement+growth,data=traindata,type='C',
                 probability=TRUE,kernel='linear')

model_svm

predsvm <- NULL

predsvm <- predict(model_svm,testdata)

confusionMatrix(predsvm,testdata$response)

##ROC Curve


pred1 <- predict(model_svm,testdata,probability=T)

pred2 <- attr(pred1,"probabilities")[,1]

dim(pred2)

str(master)
head(attr(pred1,"probabilities"))
library(ROCR)
pred <- prediction(pred2,testdata$response)


perf<-performance(pred,"tpr","fpr")

plot(perf)

abline(0,1)

auc<-performance(pred,"auc")

auc@y.values
