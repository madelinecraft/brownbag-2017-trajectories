*Exploratory Analysis*
mydata = read.table("/Users/madelinecraft/Desktop/204B/dental.txt", header = FALSE)
#dataset I'm borrowing from 204D
names(mydata) = c("ID", "Gender", "Time1", "Time2", "Time3", "Time4")
#adding variable labels
head(mydata)
#longitudinal dataset with repeated measurements of dental growth for 27 children
#reshaping from wide to long
install.packages('lattice')
library('lattice')
xyplot(Response~Time | ID, data=long.mydata, 
	prepanel=function(x,y) prepanel.loess(x,y,
	family="gaussian"), 
	xlab="Time", ylab="Dental Growth",
	panel=function(x,y) {
	panel.xyplot(x,y) 
	panel.loess(x,y,family="gaussian")}, 								ylim=c(15,35), as.table=T)
#empirical growth plot intructions from http://stats.idre.ucla.edu/r/examples/alda/r-applied-longitudinal-data-analysis-ch-2/
indivOLS<-by(long.mydata, long.mydata$ID, function(x) summary(lm(Response~Time,data=x)))
indivOLS
#individual OLS trajectories
int<-by(long.mydata, long.mydata$ID, function(data) coefficients(lm(Response~Time,data=data))[[1]])
int<-unlist(int)
names(int)<-NULL
summary(int)
stem(int,scale=2)
slope<-by(long.mydata, long.mydata$ID, function(data) coefficients(lm(Response~Time,data=data))[[2]])
slope<-unlist(slope)
names(slope)<-NULL
summary(slope)
stem(slope,scale=2)
#stem and leaf plots for individual intercepts and slopes
rsq<-by(long.mydata, long.mydata$ID, function(data) summary(lm(Response~Time,data=data))$r.squared)
rsq<-unlist(rsq)
names(rsq)<-NULL
summary(rsq)
stem(rsq,scale=2)
#stem and leaf plot for r squared
xyplot(Response~Time | ID, data=long.mydata, 
	panel=function(x,y) {
	panel.xyplot(x,y) 
	panel.lmline(x,y)}, 								ylim=c(15,35), as.table=T)
#OLS trajectories fitted to each individual's data
mean(int)
sd(int)
mean(slope)
sd(slope)
cor(int,slope)
#summary stats on intercepts and slopes
head(long.mydata)
interaction.plot(long.mydata$Time, long.mydata$ID, long.mydata$Response, xlab='Time', ylab='Dental Growth', legend=FALSE)
#interaction plot of individual trajectories

as.factor(long.mydata$Gender)
dentalm<-long.mydata[long.mydata$Gender=='M' , ]
dentalm
dentalf<-long.mydata[long.mydata$Gender=='F' , ]
dentalf
par(mfrow=c(1,2))
interaction.plot(dentalm$Time, dentalm$ID, dentalm$Response, 
xlab='Time', ylab='Dental Growth', legend=FALSE, ylim=c(16,32))
title(main="Males")	
interaction.plot(dentalf$Time, dentalf$ID, dentalf$Response,
xlab='Time', ylab='Dental Growth', legend=FALSE, ylim=c(16,32))
title(main="Females")
table(long.mydata$Gender)


*Analysis*
OLSm1<-lm(Response~Time+Gender, data=long.mydata)
summary(OLSm1)
library(nlme)
*Ask Tim why this does not give me more info on intercept and slope separated by gender and other interpretations...
mlm1<-lme(Response~Time+Gender, random=~1+Time|ID, method="REML", data=long.mydata)
summary(mlm1)
AIC(OLSm1);BIC(OLSm1)


