rm(list=ls())
mydata<-read.table("~/Documents/University of Colorado Denver/Predictive Analytics/Project/train_dat1.txt",sep=",",header=TRUE,na.strings = c("","NA"))
str(mydata)
head(mydata)
names(mydata)<- c("rowno","cusid","shoppts","rcdtyp","day","time","state","location","grpsize","homeowner","carage","carval","rf","ageoldest","ageyoungest","married","cpre","durpre","cost")
names(mydata)
str(mydata)

#Imputation for Risk Factor, C_Previous, Duration Previous and Car Value
colSums(is.na(mydata)) #List number of entries column-wise with NA values
mydata$rf<-ifelse(is.na(mydata$rf),5,mydata$rf) # Replacement of NAs in risk factor with 5
mydata$cpre<-ifelse(is.na(mydata$cpre),0,mydata$cpre) # Replacement of NAs in C_Previous with 0
mydata$durpre<-ifelse(is.na(mydata$durpre),0,mydata$durpre) # Replacement of NAs in Duration_Pre with 0
mydata$carval<-ifelse(is.na(as.character(mydata$carval)),"e",as.character(mydata$carval)) #Assignment of missing values in Car Value to "e"
colSums(is.na(mydata)) #All missing values imputed

#Data Structure:
mydata$shoppts<-as.factor(mydata$shoppts)
mydata$rcdtyp<-as.factor(mydata$rcdtyp)
mydata$day<-as.factor(mydata$day)
mydata$location<-as.factor(mydata$location)
mydata$grpsize<-as.integer(mydata$grpsize)
mydata$homeowner<-as.factor(mydata$homeowner)
mydata$carval<-as.factor(mydata$carval)
mydata$rf<-as.factor(mydata$rf)
mydata$married<-as.factor(mydata$married)
mydata$cpre<-as.factor(mydata$cpre)

#Conversion of time from factor to a continous and also as a categorical variable
time_formatted<- strptime(mydata$time, "%H:%M")
time_cont<-round(as.numeric(format(time_formatted,"%H"))+as.numeric(format(time_formatted,"%M"))/60,2)
time_cat<-round(as.numeric(format(time_formatted,"%H"))+as.numeric(format(time_formatted,"%M"))/60,0)
str(time_cont)
myproject<-cbind(mydata,time_cont,time_cat)
replace(myproject$time_cat,24,0) #replacement of 24th hours to 0 hours
rm(time_cat,time_cont,mydata,time_formatted)

myproject$time_cont<-as.numeric(myproject$time_cont) #time in hours as a continous variable (2 decimals)
myproject$time_cat<-as.factor(myproject$time_cat) #time in hours as a factor

attach(myproject)
head(myproject)

#EDA
cor(myproject[,c(19,9,11,14,15,18,20)], use="pairwise")
plot(age_oldest~age_youngest)

#From the pairwise correlation matrix above, we note that ageoldest and age youngest has a high correlation with each other and this is a case of multicollinearity. Therfore, we would be using only one of them preferably ageyoungest with a higher correlation to cost for our model.

boxplot(cost,horizontal= TRUE)
boxplot(cost~rf,horizontal=TRUE,notch=1,boxwex=.3,las=1,whiskltyp=1,col = "orange",outpch=16,outcol = "gray") 






#Regression line models

#Baseline model creation, cross-validation and its RMSE
blmodel<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf+ageoldest+ageyoungest+married+cpre+durpre+time_cat+time_cont) #Baseline Model with 14 variable
summary(blmodel)
blrmse<-sqrt(cv.glm(data=myproject,blmodel,K=10)$delta[1]) #Baseline Model's RMSE after CV

#Based on the summary of the Baseline Model, every variable except time_cat and time_cont seem to have a high significance. Therefore, we drop both time variables from the model and we would select the rest of the 12 variables


# We now create different regression models starting 1 variable and adding one variable to it each time. 
reg1<-glm(cost~day)
reg2<-glm(cost~day+state)
reg3<-glm(cost~day+state+grpsize)
reg4<-glm(cost~day+state+grpsize+homeowner)
reg5<-glm(cost~day+state+grpsize+homeowner+carage)
reg6<-glm(cost~day+state+grpsize+homeowner+carage+carval)
reg7<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf)
reg8<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf+ageoldest)
reg9<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf+ageoldest+ageyoungest)
reg10<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf+ageoldest+ageyoungest+married)
reg11<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf+ageoldest+ageyoungest+married+cpre)
reg12<-glm(cost~day+state+grpsize+homeowner+carage+carval+rf+ageoldest+ageyoungest+married+cpre+durpre)


# We now do cross Validation and compute MSE for each of the above 12 regression models
require(boot)
mse=NULL
mse[1]<-cv.glm(data=myproject,reg1,K=10)$delta[1]
mse[2]<-cv.glm(data=myproject,reg2,K=10)$delta[1]
mse[3]<-cv.glm(data=myproject,reg3,K=10)$delta[1]
mse[4]<-cv.glm(data=myproject,reg4,K=10)$delta[1]
mse[5]<-cv.glm(data=myproject,reg5,K=10)$delta[1]
mse[6]<-cv.glm(data=myproject,reg6,K=10)$delta[1]
mse[7]<-cv.glm(data=myproject,reg7,K=10)$delta[1]
mse[8]<-cv.glm(data=myproject,reg8,K=10)$delta[1]
mse[9]<-cv.glm(data=myproject,reg9,K=10)$delta[1]
mse[10]<-cv.glm(data=myproject,reg10,K=10)$delta[1]
mse[11]<-cv.glm(data=myproject,reg11,K=10)$delta[1]
mse[12]<-cv.glm(data=myproject,reg12,K=10)$delta[1]

#Plot all the RMSEs from all above 12 models and compare against the baseline model
plot(c(1:12),sqrt(mse),type="b",col="darkblue",pch=16, ylab="RMSE", xlab = "Regression Models\n with n variables",ylim = c(36,46), main="RMSE vs Regression Models and Baseline Model\n(10-fold cross-validated values)",las=1)
abline(h=blrmse,col="darkred",lwd=2)
legend("topright","Baseline RMSE",lty=1,col="darkred")


#From the plot it is clear that Reg Model 3 doesnt reduce RMSE any further from Reg Model 2. Therefore, the variable 'grpsize' that was added in Reg Model 3 is dropped from the final model in order to avoid overfitting.

#Reg model with 'grpsize' variable dropped for reg12 model:
reg13<-glm(cost~day+state+homeowner+carage+carval+rf+ageoldest+ageyoungest+married+cpre+durpre)
mse[13]<-cv.glm(data=myproject,reg13,K=10)$delta[1]

#Checking RMSE plot for reg13 model
plot(c(1:13),sqrt(mse),type="b",col="darkblue",pch=16, ylab="RMSE", xlab = "Regression Models\n with n variables",ylim = c(36,46), main="RMSE vs Regression Models and Baseline Model\n (10-fold cross-validated values)",las=1)
abline(h=blrmse,col="darkred",lwd=2)
legend("topright","Baseline RMSE",lty=1,col="darkred")

#Variable 'married' added in Reg model 10 also doesnot seem to reduce RMSE. To check this, we remove 'married' variable from the model 13 and check it on the plot
reg14<-glm(cost~day+state+homeowner+carage+carval+rf+ageoldest+ageyoungest+cpre+durpre)
mse[14]<-cv.glm(data=myproject,reg14,K=10)$delta[1]
plot(c(1:14),sqrt(mse),type="b",col="darkblue",pch=16, ylab="RMSE", xlab = "Regression Models\n with n variables",ylim = c(36,46), main="RMSE vs Regression Models and Baseline Model\n (10-fold cross-validated values)",las=1)
abline(h=blrmse,col="darkred",lwd=2)
legend("topright","Baseline RMSE",lty=1,col="darkred")
#Contrary to our belief, the RMSE has increased when we dropped 'married' variable in reg model 14, so we go go back to reg model 13 with 11 variables that gives the same MSE as our baseline model.


reg15<-glm(cost~day+state+homeowner+carage+carval+rf+I(ageyoungest/ageoldest)+married+cpre+durpre)
mse[15]<-cv.glm(myproject,reg15,K=10)$delta[1]
mse[15]
sqrt(mse[15])
summary(reg15)

plot(c(1:15),sqrt(mse),type="b",col="darkblue",pch=16, ylab="RMSE", xlab = "Regression Models\n with n variables",ylim = c(36,46), main="RMSE vs Regression Models and Baseline Model\n (10-fold cross-validated values)",las=1)
abline(h=blrmse,col="darkred",lwd=2)
legend("topright","Baseline RMSE",lty=1,col="darkred")



#MAPE of training set based on reg12
pred<-predict(reg12,myproject)
head(pred)
mean(abs((myproject$cost-pred)/myproject$cost))






#Clean up on test
mytest<-read.csv("~/Documents/University of Colorado Denver/Predictive Analytics/Project/test_dat1_no_y.csv",na.strings = c("","NA"))
head(mytest)
names(mytest)<- c("rowno","cusid","shoppts","rcdtyp","day","time","state","location","grpsize","homeowner","carage","carval","rf","ageoldest","ageyoungest","married","cpre","durpre")
names(mytest)
str(mytest)

#Imputation for Risk Factor, C_Previous, Duration Previous and Car Value
colSums(is.na(mytest)) #List number of entries column-wise with NA values
mytest$rf<-ifelse(is.na(mytest$rf),5,mytest$rf) # Replacement of NAs in risk factor with 5
mytest$cpre<-ifelse(is.na(mytest$cpre),0,mytest$cpre) # Replacement of NAs in C_Previous with 0
mytest$durpre<-ifelse(is.na(mytest$durpre),0,mytest$durpre) # Replacement of NAs in Duration_Pre with 0
mytest$carval<-ifelse(is.na(as.character(mytest$carval)),"e",as.character(mytest$carval)) #Assignment of missing values in Car Value to "e"
colSums(is.na(mytest)) #All missing values imputed

#Data Structure:
mytest$shoppts<-as.factor(mytest$shoppts)
mytest$rcdtyp<-as.factor(mytest$rcdtyp)
mytest$day<-as.factor(mytest$day)
mytest$location<-as.factor(mytest$location)
mytest$grpsize<-as.integer(mytest$grpsize)
mytest$homeowner<-as.factor(mytest$homeowner)
mytest$carval<-as.factor(mytest$carval)
mytest$rf<-as.factor(mytest$rf)
mytest$married<-as.factor(mytest$married)
mytest$cpre<-as.factor(mytest$cpre)

#Conversion of time from factor to a continous and also as a categorical variable
time_formatted<- strptime(mytest$time, "%H:%M")
time_cont<-round(as.numeric(format(time_formatted,"%H"))+as.numeric(format(time_formatted,"%M"))/60,2)
time_cat<-round(as.numeric(format(time_formatted,"%H"))+as.numeric(format(time_formatted,"%M"))/60,0)
str(time_cont)
mytest<-cbind(mytest,time_cont,time_cat)
replace(mytest$time_cat,24,0) #replacement of 24th hours to 0 hours
rm(time_cat,time_cont,time_formatted)

mytest$time_cont<-as.numeric(mytest$time_cont) #time in hours as a continous variable (2 decimals)
mytest$time_cat<-as.factor(mytest$time_cat) #time in hours as a factor



summary(reg12)
summary(mytest$pred.test)



#Risk factor re-imputation
head(rf)

data_norf<-myproject[rf=="5",]
dim(data_norf)
data_rf<-myproject[!rf=="5",]
dim(data_rf)
data_rf$rf<-as.numeric(data_rf$rf)
data_norf$rf<-as.numeric(data_norf$rf)
reg.rf<-lm(rf~state+grpsize+homeowner+ageyoungest+married,data=data_rf)
head(myproject)
reg.rf
rf.pred<-predict(reg.rf,data_norf)

data_test_norf<-mytest[rf=="5",]
data_test_rf<-mytest[!rf=="5",]
dim(data_test_rf)
dim(data_test_norf)
data_test_rf$rf<-as.numeric(data_test_rf$rf)
data_test_norf$rf<-as.numeric(data_test_norf$rf)
rf.test.pred<-predict(reg.rf,data_test_rf)
rf.test.pred<-ifelse(rf.test.pred<3,floor(rf.test.pred),round(rf.test.pred))
rf.check.pred<-ifelse(rf.check.pred<3,floor(predict(reg.rf,data_rf)),round(rf.test.pred))

mean(abs((data_test_rf$rf-rf.test.pred)/data_test_rf$rf))
mean(abs((data_rf$rf-rf.check.pred)/data_rf$rf))
summary(rf.check.pred)
rf.check.pred
