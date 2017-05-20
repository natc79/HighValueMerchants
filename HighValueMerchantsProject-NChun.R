# Created by:  Natalie Chun (21 April 2017)
# Purpose:  This code was intended to analyze hypothetical merchant transaction data.


#I.  Read in initial libraries
#-------------------------------------------------------
#(to install packages:  install.packages("packagename")
library(rio)
library(chron)
library(data.table)
library(dplyr)
library(ggplot2) 
library(gtable)
library(grid)
library(stats)
library(class)
library(TTR)
library(plyr)
library(dplyr)
library(zoo)
library(foreign)
library(rpart)
library(randomForest)
library(miscTools)
library(plm)
library(stargazer)
library(psych)
library(gmodels)


#II. Declare important functions for analysis
#-------------------------------------------------------

# predictionscx: automates the fit and train models for cross-sectional data
# after being provided with a dataset.  It calculates r-squared and 
# MSE for (i) linear regression (ii) random tree regressor (iii) random forest regressor

predictionscx <- function(modeldata) {
  
  ind <- sample(2, nrow(modeldata), replace=TRUE, prob=c(0.67, 0.33))
  
  #set-seeds for model train and test datasets                                                                                                              collapse = " + "), sep = " ~ "))
  set.seed(2)
  modeldata.train <- modeldata[ind==1,1:ncol(modeldata)]
  modeldata.test <- modeldata[ind==2,1:ncol(modeldata)]
  modeldata.testLabels <- modeldata[ind==2,1]
  show(head(modeldata.train))
  show(head(modeldata.test))
  
  #Code that creates a formula that can be called based on data
  frmla <- as.formula(paste(colnames(modeldata)[1], paste(colnames(modeldata)[2:ncol(modeldata)], sep = "", 
                                                          collapse = " + "), sep = " ~ "))
  show(frmla)
  lm.train <- lm(frmla,data=modeldata.train,na.action=na.exclude)
  show(summary(lm.train))
  pr.test <- predict(lm.train, newdata=modeldata.test)
  
  lm.test <- lm(frmla,data=modeldata.test,na.action=na.exclude)
  show(summary(lm.test))
  
  SS.total      <- sum((modeldata.testLabels - mean(modeldata.testLabels))^2)
  SS.residual   <- sum((modeldata.testLabels - pr.test)^2)
  SS.regression <- sum((pr.test - mean(modeldata.testLabels))^2)
  SS.total - (SS.regression+SS.residual)
  message("SSR:")
  show(SS.residual)
  message("SST:")
  show(SS.total)
  
  #R-squared
  test.rsq <- 1 - SS.residual/SS.total  
  message("R-squared (linear regression):")
  show(test.rsq)
  
  # fraction of variability explained by the model
  SS.regression/SS.total 
  
  #Mean-squared error
  mse <- mean((modeldata.testLabels - pr.test)^2)
  message("MSE (linear regression):")
  show(mse)
  
  #Random tree regressor
  rt.train<-rpart(frmla,data=modeldata.train,method="anova",control=)  #anova refers to regression tree (as opposed to classification tree)
  show(printcp(rt.train)) # display the results 
  show(plotcp(rt.train)) # visualize cross-validation results 
  show(summary(rt.train)) # detailed summary of splits
  test.rsq <- rSquared(modeldata.test[,1], modeldata.test[,1] - predict(rt.train, modeldata.test[,2:ncol(modeldata)]))
  mse <- mean((modeldata.test[,1] - predict(rt.train, modeldata.test[,2:ncol(modeldata)]))^2)
  message("R-squared (random tree):")
  show(test.rsq)
  message("MSE (random tree):")
  show(mse)
  
  
  #Random forest regressor
  rf.train <- randomForest(frmla, data=modeldata.train, ntree=20, nodesize=5, mtry=6)
  test.rsq <- rSquared(modeldata.test[,1], modeldata.test[,1] - predict(rf.train, modeldata.test[,2:ncol(modeldata)]))
  mse <- mean((modeldata.test[,1] - predict(rf.train, modeldata.test[,2:ncol(modeldata)]))^2)
  message("R-squared (random forest):")
  show(test.rsq)
  message("MSE (random forest):")
  show(mse)
  
  
  return(list(modeldata.train,modeldata.test))
}

#normalize:  normalizes data before taking it to the test and train sets
# useful when there is a huge range of values in x and y
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

#III.  READ IN DATA AND CLEAN IT UP
#--------------------------------------------------------

#Read in raw data
casedata <- read.csv("C:\\JobApplications\\Stripe\\case_final.csv", header=TRUE, sep=",")
casedata$userid <- group_indices(casedata,user)
head(casedata)
sapply(casedata,typeof)

#Convert time data into a usable form
newtime <-as.character(casedata$time)
head(newtime)
#Split the time data on the period
timesplit <- strsplit(newtime,"[.]")
head(timesplit)

#get the first value of the timesplit function
timeval1<-lapply(timesplit, `[`, 1)
head(timeval1)
typeof(timeval1)

#Apply the strptime function across multiple values
timeval2 <- strptime(array(timeval1), format='%Y-%m-%dT%H:%M:%S')
#now put these values back in the dataframe
casedata$fdatetime <- timeval2
casedata$date <- format(timeval2,'%Y-%m-%d')
casedata$year <- format(timeval2,'%Y')
casedata$month <- format(timeval2,'%m')
casedata$day <- format(timeval2,'%d')
head(format(timeval2,'%Y'))
head(casedata$date)

#Sum over rows to get the aggregates
casedata$amt <- casedata$amount_in_cents/100
casedata$cnt <- 1

#The below statement starts to compute daily statistics on transaction per day
#aggregate function is too slow to execute use other functions
#txncnt_perday <- aggregate(casedata, by=list(casedata$user, casedata$date), FUN=sum)
casedata$fdate <- as.Date(casedata$date)
casedata1 <- data.table(casedata)
txncnt_perday<-casedata1[,list(amtperday=sum(amt),txnperday=sum(cnt),amtpertxn=mean(amt)), by='user,fdate']
head(txncnt_perday)
nrow(txncnt_perday)
ncol(txncnt_perday)
newdf <- data.frame(txnamt_perday,txncnt_perday)
head(txncnt_perday,10)
nrow(newdf)
nrow(data_perday)
head(casedata1,10)

#Manipulate data to fill-in missing date values
fdate=as.Date(casedata$date)
min_date = min(fdate)
max_date = max(fdate)
all_dates=seq(min_date,by=1,to=max_date) 
uniqueuser <- unique(casedata$user, incomparables=FALSE)
user_dates <- CJ(user=uniqueuser,fdate=all_dates)
colnames(user_dates)<-c("user","fdate")
minmaxdates <- casedata1[,list(min_date=min(fdate), max_date=max(fdate)),by='user']
minmaxdates$totaldays <- minmaxdates$max_date-minmaxdates$min_date
#flag users if they have truncated data (i.e. we can't observe 365 days)
minmaxdates$truncated <- minmaxdates$min_date + 365 > max_date
summary(minmaxdates$truncated)
head(minmaxdates)
nrow(casedata)
ncol(casedata)
nrow(user_dates)
ncol(user_dates)
head(casedata)
head(user_dates)

#fill-in all of the lines of data between min and max dates
casedata2 <- merge(user_dates, minmaxdates, by=c("user"), all=TRUE)
casedata2 <- subset(casedata2, min_date <= fdate & fdate <= max_date)
nrow(casedata2)
head(casedata2)

#now merge back in the data with the txnperday data and replace missing values with zero
txnperday <- merge(txncnt_perday,casedata2,by=c("user","fdate"), all=TRUE)
#since we have a data.table we want to convert back to dataframe
txnperday1 <- as.data.frame(txnperday)
#before we replace values with zero lets count all of the merchants with at least 30 days of positive transactions
txnperday1$daystxn <- is.na(txnperday1$amtperday) == 0
txnperday1 <- replace(txnperday1,is.na(txnperday1),0)
head(txnperday1)
nrow(txnperday1)
txnperday1$totaldays <- txnperday1$max_date - txnperday1$min_date

#Create simple growth values per day using this data
txnperday1$gr_amtperday <- with(txnperday1, ave(amtperday,user,
                      FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))

#only replace the NaN values.  The prevalence of NaN and Inf may suggest better to 
#use weekly aggregates.
txnperday1$gr_amtperday[is.nan(txnperday1$gr_amtperday)] <- 0
head(txnperday1,20)

# IV.  ADDITIONAL FORMATTING TO OBTAIN DAILY AND WEEKLY TRANSACTION DATA
#---------------------------------------------------------------------------------------------------

#Collapse the transaction data further so that there is only one per user
txnperday2 <- data.table(txnperday1)
head(txnperday2)
txnperday3 <- txnperday2[txnperday2$gr_amtperday != 'Inf' & txnperday2$gr_amtperday != 'NA']
head(txnperday3)
userstats<-txnperday3[,list(amtpertxn=mean(amtpertxn), amtperday=mean(amtperday),txnperday=mean(txnperday),totalposdays=sum(daystxn),totaldays=max(totaldays),totalamt=sum(amtperday), gr_amtperday=mean(gr_amtperday)), by='user']
userstats$amtpertxn_txnperday <- userstats$amtpertxn*userstats$txnperday
dim(userstats)
#drop all data where there is greater than 30 days (of positive usage)
userstats<-userstats[totaldays>30,]
head(userstats)
dim(userstats)

#Collapse the data into weeks instead because we have problems with lots of 0's when using
#the daily data
sapply(txnperday1,typeof)
txnperday1$week <- week(txnperday1$fdate)
head(txnperday1$week,25)
head(txnperday1)
#convert back into other data
txnperday2 <- data.table(txnperday1)
head(txnperday2)
#Now aggregate the data by week instead and then recompute growth values
txnperweek <- txnperday2[,list(fdate=min(fdate), amtpertxn=mean(amtpertxn), amtperweek=sum(amtperday), txnperweek=sum(txnperday),weekstxn=sum(daystxn),totaldays=max(totaldays),truncated=max(truncated)),by=list(user,week)]

#Create simple growth values per day using this data
txnperweek$gr_amtperweek <- with(txnperweek, ave(amtperweek,user,
                                                 FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
txnperweek1 <- as.data.frame(txnperweek)
txnperweek1$gr_amtperweek[is.nan(txnperweek1$gr_amtperweek)] <- 0
#txnperweek1 <- txnperweek1[txnperweek1$gr_amtperweek != 'Inf' & txnperweek1$gr_amtperweek != 'NA']

txnperweek1 <- replace(txnperweek1,is.na(txnperweek1),0)
head(txnperweek1)
txnperweek2 <- data.table(txnperweek1)
txnperweek3 <- txnperweek2[txnperweek2$gr_amtperweek != 'Inf' & txnperweek2$gr_amtperweek != 'NA']
txnperweek4 <- as.data.frame(txnperweek3)
head(txnperweek4)
class(txnperweek4)
summary(txnperweek4)
str(txnperweek4)

#take the data and normalize the values
txnperweeknorm <- as.data.frame(lapply(txnperweek4[c('amtpertxn','amtperweek','txnperweek','gr_amtperweek')], normalize))
year.f = factor(year)
summary(txnperweeknorm)

txnperweek4$month <- format(txnperweek4$fdate,'%m')
month <- format(txnperweek4$fdate,'%m')
month.f <- factor(month)
head(month.f)
dummies <- model.matrix(~month.f)
head(dummies)
txnperweek5<-cbind(txnperweek4,dummies)
head(txnperweek5)

# Define the lag function
lg <- function(x)c(NA, x[1:(length(x)-1)])
txnperweek6 <- txnperweek5 %>% group_by(user) %>% mutate(lamtperweek = lag(amtperweek, 1), l2amtperweek = lag(amtperweek, 2))
help("%>%")
head(txnperweek6)

#obtain average for prior month of data (not sure if correct - recheck?)
txnperweek6$mavg.4wkamt <- ave(txnperweek6$lamtperweek, txnperweek6$user, FUN = function(x) rollmeanr(x, k=min(NROW(x), 4), fill = NA, na.pad = TRUE))
txnperweek6$mavg.4wksdamt <- ave(txnperweek6$lamtperweek, txnperweek6$user, FUN= function(x) rollapply(x,width=min(NROW(x), 4),FUN=sd,fill=NA, align="right"))
#Get moving average first and then take lags
txnperweek6$mavg.4wktxn <- ave(txnperweek6$txnperweek, txnperweek6$user, FUN = function(x) rollmeanr(x, k=min(NROW(x), 4), fill = NA, na.pad = TRUE))
txnperweek6$mavg.4wkgramt <- ave(txnperweek6$gr_amtperweek, txnperweek6$user, FUN = function(x) rollmeanr(x, k=min(NROW(x), 4), fill = NA, na.pad = TRUE))
#To get the standard deviation
txnperweek6$mavg.4wksdtxn <- ave(txnperweek6$txnperweek, txnperweek6$user, FUN= function(x) rollapply(x,width=min(NROW(x), 4),FUN=sd,fill=NA, align="right"))
txnperweek6$mavg.4wksdgramt <- ave(txnperweek6$gr_amtperweek, txnperweek6$user, FUN= function(x) rollapply(x,width=min(NROW(x), 4),FUN=sd,fill=NA, align="right"))

#for now lets compress the data into weekly transactions per user
txnperweek7 <- data.table(txnperweek6)
head(txnperweek7)
txnperweek8 <- txnperweek7[txnperweek7$gr_amtperweek != 'Inf' & txnperweek7$gr_amtperweek != 'NA' & txnperweek6$mavg.4wkamt != 'NA' & txnperweek7$mavg.4wksdtxn != 'NA']
head(txnperweek8)
userstatsperweek<-txnperweek8[,list(amtpertxn=mean(amtpertxn), amtperweek=mean(amtperweek),txnperweek=mean(txnperweek),totalposdays=sum(weekstxn),totaldays=max(totaldays),totalamt=sum(amtperweek), gr_amtperweek=mean(gr_amtperweek)), by='user']
head(userstatsperweek)



#IV.  GENERATE SOME BASIC GRAPHS
#-------------------------------------------------------

#Create histogram of the number of days using payment platform
totaldays <- as.numeric(minmaxdates$totaldays)
bins = seq(0,370,by=5)
h <-hist(totaldays, plot=F, breaks=bins)
h$counts <- h$counts/sum(h$counts) 
plot(h, col="Purple", xlim=c(0,366), xlab="Usage Days", main="Histogram of Usage Days")
axis(1, at=c(0,370), labels=c("",""), lwd.ticks=0)
axis(1, at=seq(0 , 370, by=100), lwd=0, lwd.ticks=1)
dev.copy(png,'C:\\JobApplications\\Stripe\\totalusagedays.png')
dev.off()
length(totaldays)

# Create histogram of the number of days using Stripe (dropping those with < 30 days of usage)
tempdata <- subset(minmaxdates,as.numeric(minmaxdates$totaldays) > 30 & minmaxdates$truncated ==0)
head(tempdata)
totaldays1 <- as.numeric(tempdata$totaldays)
head(totaldays1)
length(totaldays1)
h1 <-hist(totaldays1, plot=F, breaks=bins)
h1$counts <- h1$counts/sum(h1$counts) 
plot(h1, freq=TRUE, ylab="Relative Frequency", col="Blue", main="Histogram of Usage Days", xlab="Usage Days")
axis(1, at=c(0,370), labels=c("",""), lwd.ticks=0)
axis(1, at=seq(0 , 370, by=100), lwd=0, lwd.ticks=1)
#mtext("Note: excludes users < 30 days and users where the data is truncated",side=1,line=4, at=c(25,350))
#ggsave("FILE.png",width=mywidth, height=myheight, p1, dpi=90)
dev.copy(png,'C:\\JobApplications\\Stripe\\totalusagedays_selected.png')
dev.off()

#Create histogram of days of positive usage (i.e. non-zero values)
h2 <-hist(userstats$totalposdays, plot=F, breaks=bins)
h2$counts <- h2$counts/sum(h2$counts) 
plot(h2, freq=TRUE, ylab="Relative Frequency", col="Red", main="Histogram of Total Positive Usage Days", xlab="Usage Days")
axis(1, at=c(0,370), labels=c("",""), lwd.ticks=0)
axis(1, at=seq(0 , 370, by=100), lwd=0, lwd.ticks=1)
#mtext("Note: excludes users < 30 days and users where the data is truncated",side=1,line=4, at=c(25,350))
#ggsave("FILE.png",width=mywidth, height=myheight, p1, dpi=90)
dev.copy(png,'C:\\JobApplications\\Stripe\\totalposusagedays_selected.png')
dev.off()

#V. ANALYZE DAILY TRANSACTION DATA (CX)
#--------------------------------------------------------

#Perform basic regression analysis on daily averages
#have to first exclude infinite, NA values
col2[which(!is.finite(col2))] = NA 
data.lm <- lm(gr_amtperday~txnperday+amtpertxn+amtpertxn_txnperday,data=userstats,na.action=na.exclude)
summary(data.lm)

# Note the data is pretty badly fit between test and train models (model very inaccurate), but the linear regression performs best
userstats$txnperday_amtpertxn <- userstats$txnperday*userstats$amtpertxn
userstats$amtpertxn2 <- with(userstats, amtpertxn^2)
userstats$txnperday2 <- with(userstats, txnperday^2)
modeldata1 <- data.frame(userstats)[,c('gr_amtperday','amtpertxn','amtpertxn2','txnperday','txnperday2','txnperday_amtpertxn','totalposdays','totaldays')]
results<-predictionslm(modeldata1)


#V. ANALYZE WEEKLY TRANSACTION DATA (CX)
#--------------------------------------------------------

# Note the data is pretty badly fit between test and train models (model very inaccurate)
userstatsperweek$amtpertxn_txnperweek <- userstatsperweek$txnperweek*userstatsperweek$amtpertxn
userstatsperweek$amtpertxn2 <- with(userstatsperweek, amtpertxn^2)
userstatsperweek$amtpertxn3 <- with(userstatsperweek, amtpertxn^3)
userstatsperweek$txnperweek2 <- with(userstatsperweek, txnperweek^2)
userstatsperweek$txnperweek3 <- with(userstatsperweek, txnperweek^3)

data.lm <- lm(gr_amtperweek~txnperweek+txnperweek2+txnperweek3+amtpertxn+amtpertxn2+amtpertxn3+amtpertxn_txnperweek,data=userstatsperweek,na.action=na.exclude)
summary(data.lm)
stargazer(data.lm, title="Results", type="text", dep.var.labels=c("Growth in Weekly Revenues"),
          covariate.labels=c("Txn per week","Txn per week Sq.","Txn per week Cu.","Amt Per Txn", "Amt Per Txn Sq.","Amt Per Txn Cu.",
                             "Amt Per Txn*Txn per Week"), align=TRUE,no.space=TRUE)

amtpertxn<-seq(0,50,by=1)
newdf<-as.data.frame(amtpertxn)

#Use the predictions above to graph out some values
newdf$ds1 <- with(newdf, data.lm$coefficients[1]+data.lm$coefficients[2]*1+data.lm$coefficients[3]*1+data.lm$coefficients[4]*1
                  +data.lm$coefficients[5]*amtpertxn+data.lm$coefficients[6]*amtpertxn^2+data.lm$coefficients[7]*amtpertxn^3
                  +data.lm$coefficients[8]*amtpertxn*1)

newdf$ds3 <- with(newdf, data.lm$coefficients[1]+data.lm$coefficients[2]*3+data.lm$coefficients[3]*9+data.lm$coefficients[4]*27
                  +data.lm$coefficients[5]*amtpertxn+data.lm$coefficients[6]*amtpertxn^2+data.lm$coefficients[7]*amtpertxn^3
                  +data.lm$coefficients[8]*amtpertxn*3)

newdf$ds5 <- with(newdf, data.lm$coefficients[1]+data.lm$coefficients[2]*5+data.lm$coefficients[3]*25+data.lm$coefficients[4]*125
                  +data.lm$coefficients[5]*amtpertxn+data.lm$coefficients[6]*amtpertxn^2+data.lm$coefficients[7]*amtpertxn^3
                  +data.lm$coefficients[8]*amtpertxn*5)


plot(c(0,50),c(-0,5),type="n",xlab="Amount per Transaction",ylab="Revenue Growth (%)",main="Revenue Growth")
lines(newdf$amtpertxn, newdf$ds1, col="blue",lwd=2.5)
lines(newdf$amtpertxn, newdf$ds3, col="green",lwd=2.5)
lines(newdf$amtpertxn, newdf$ds5, col="pink",lwd=2.5)
legend(0,5,c("F=1","F=3","F=5"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("blue","green","pink"))
dev.copy(png,'C:\\JobApplications\\Stripe\\revenue_growth_week.png')
dev.off()


# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(newdf, aes(seq,value)) + geom_line(aes(colour = series))
dev.copy(png,'C:\\JobApplications\\Stripe\\totalusagedays_selected.png')
dev.off()


ggplot(newdf$seq,newdf$ds1,type="l",col="red")
lines(rangeamtpertxn,ds2,col="green")
lines(rangeamtpertxn,ds3,col="blue")

modeldata2 <- data.frame(userstatsperweek)[,c('gr_amtperweek','amtperweek','amtpertxn','amtpertxn2','amtpertxn3','txnperweek','txnperweek2','txnperweek3','amtpertxn_txnperweek','totaldays','totalposdays')]
results<-predictionslm(modeldata2)



#VI. ANALYZE WEEKLY TRANSACTION DATA (PANEL)
#--------------------------------------------------------


#fixed effects model
fixedeff <- lm(gr_amtperweek~amtpertxn+txnperweek+mavg.4wkamt+mavg.4wksdamt+mavg.4wktxn+mavg.4wksdtxn+factor(week)+factor(month), data=txnperweek6)
summary(fixedeff)

# random effects model
randomeff <- plm(gr_amtperweek~amtpertxn+txnperweek+mavg.4wkamt+mavg.4wksdamt+mavg.4wktxn+mavg.4wksdtxn+factor(month), data=txnperweek6, index=c("user","week"), model="random")
summary(randomeff)

#Between effects model
betweeneff <- plm(gr_amtperweek~amtpertxn+txnperweek+mavg.4wkamt+mavg.4wksdamt+mavg.4wktxn+mavg.4wksdtxn+factor(month), data=txnperweek6, index=c("user","week"), model="within")
summary(randomeff)

fixedeff1<-fixedeff$coefficients[1:7]
print(fixedeff$coefficients)
print(fixedeff1)
betweeneff1<-betweeneff$coefficients[1:7]
randomeff1<-randomeff$coefficients[1:7]

stargazer(fixedeff1, randomeff1, betweeneff1, title="Results", align=TRUE)
stargazer(fixedeff1, randomeff1, betweeneff1, title="Results", type="text",dep.var.labels=c("Growth in Transaction Amount"),align=TRUE,no.space=TRUE)

stargazer(fixedeff, randomeff, betweeneff, title="Results", type="text",dep.var.labels=c("Growth in Transaction Amount"),align=TRUE,no.space=TRUE,omit="week")


#XI. OUTPUT DESRIPTIVE STATISTICS (MORE SIMILAR TO WHAT WAS IN STATA)
#------------------------------------------------------

#create some grouped variables for the day values
summary(userstats)
userstats$grptxnperday <- ifelse(userstats$txnperday < 0.15, 1, ifelse((userstats$txnperday >= 0.15) & (userstats$txnperday <= 1), 2, 3))
userstats$grpamtpertxn <- ifelse(userstats$amtpertxn < 1, 1, ifelse((userstats$amtpertxn >= 1) & (userstats$amtpertxn <= 9.99), 2, 3))

ddply(userstats, .(grptxnperday,grpamtpertxn), summarize,  amtperday=mean(amtperday), gr_amtperday=mean(gr_amtperday), cntamtperday=length(grptxnperday))

summary(userstatsperweek)
userstatsperweek$grptxnperweek <- ifelse(userstatsperweek$txnperweek < 0.4, 1, ifelse((userstatsperweek$txnperweek >= 0.4) & (userstatsperweek$txnperweek <= 2.3), 2, 3))
userstatsperweek$grpamtpertxn <- ifelse(userstatsperweek$amtpertxn < 1, 1, ifelse((userstatsperweek$amtpertxn >= 1) & (userstatsperweek$amtpertxn <= 9.99), 2, 3))

ddply(userstatsperweek, .(grptxnperweek,grpamtpertxn), summarize,  amtperweek=mean(amtperweek), gr_amtperweek=mean(gr_amtperweek), cntamtperweek=length(grptxnperweek))


