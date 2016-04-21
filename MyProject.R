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

boxplot(cost,horizontal= TRUE)
boxplot(cost~rf,horizontal=TRUE,notch=1,boxwex=.5,las=1,whiskltyp=1,col = "orange",outpch=16,outcol = "gray") 


model1<-lm(cost~days+date+grpsize+homeowner+)