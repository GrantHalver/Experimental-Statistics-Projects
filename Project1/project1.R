#Creating table to store data
library(readr)
data <- read_delim("~/STP429/Project1/Compressed Mortality, 1999-2015.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
cols.dont.want <- c("Notes", "Crude Rate", "Year Code", "Age Group Code")
data <- data[, ! names(data) %in% cols.dont.want, drop = F]
data=subset(data, !( Population=="Not Applicable" | "Age Group"=="Not Stated") )

#Creating HIV column w/ 0/1 in each cell
data$HIV<-data$'ICD Sub-Chapter Code'
hiv=rep(0,length(data$HIV))
hiv[data$'ICD Sub-Chapter Code'=="B20-B24"]=1
data$HIV=hiv

#All ages group from 1-20 years have similar rates (for both hiv and non-hiv related), so they will be combined into 1 category
for(i in 2:27996){
  if(data$'Age Group'[i]=="1-4 years" || data$'Age Group'[i]=="5-9 years" || data$'Age Group'[i]=="10-14 years" || data$'Age Group'[i]=="15-19 years")
    data$'Age Group'[i]="1-20 years"
}

#Creating aggregate data for deaths and population
data1<-aggregate(data$Deaths, c(list(data$'Age Group'), list(data$Year), list(data$HIV)), FUN=sum)
data2<-aggregate(as.numeric(paste(data$Population)), c(list(data$'Age Group'), list(data$Year), list(data$HIV)), FUN=sum)

#Generating variable rate
rate=(data1$x/data2$x)*100000

#Creating hiv-related graph 1
plot(c(1999:2015), c(1:17), ylim=c(0,15), type="n", ann=FALSE)
title(main="Deaths realated to HIV", font.main=4)
title(xlab="Year")
title(ylab="Deaths per 100,000")

for(i in 1:5){
  rate2=rate[data1$Group.3==1 & data1$Group.1==data1$Group.1[i]]
  year1=data1$Group.2[data1$Group.3==1 & data1$Group.1==data1$Group.1[i]]
  lines(year1, rate2, type="b", col=i)
}

legend(2013, 15, data1$Group.1[1:5], cex=0.8, col=c(1:5), pch=21:22, lty=1:2)
title(sub="Source: CDC: Compressed Mortality, 1999-2015", line = 5.5)


#Creating hiv-related graph 2
plot(c(1999:2015), c(1:17), ylim=c(0,15), type="n", ann=FALSE)
title(main="Deaths related to HIV", font.main=4)
title(xlab="Year")
title(ylab="Deaths per 100,000")

for(i in 6:10){
  rate2=rate[data1$Group.3==1 & data1$Group.1==data1$Group.1[i]]
  year1=data1$Group.2[data1$Group.3==1 & data1$Group.1==data1$Group.1[i]]
  lines(year1, rate2, type="b", col=i-5)
}

legend(2013, 15, data1$Group.1[6:10], cex=0.8, col=1:5, pch=21:22, lty=1:2)
title(sub="Source: CDC: Compressed Mortality, 1999-2015", line = 5.5)

#Creating non-hiv-related graph 1
plot(c(1999:2015), c(1:17), ylim=c(0,50), type="n", ann=FALSE)
title(main="Deaths not related to HIV", font.main=4)
title(xlab="Year")
title(ylab="Deaths per 100,000")

for(i in 1:5){
  rate2=rate[data1$Group.3==0 & data1$Group.1==data1$Group.1[i]]
  year1=data1$Group.2[data1$Group.3==0 & data1$Group.1==data1$Group.1[i]]
  lines(year1, rate2, type="b", col=i)
}

legend(2013, 50, data1$Group.1[1:5], cex=0.8, col=1:5, pch=21:22, lty=1:2)
title(sub="Source: CDC: Compressed Mortality, 1999-2015", line = 5.5)

#Creating non-hiv-related graph 2
plot(c(1999:2015), c(1:17), ylim=c(0,50), type="n", ann=FALSE)
title(main="Deaths not related to HIV", font.main=4)
title(xlab="Year")
title(ylab="Deaths per 100,000")

for(i in 6:10){
  rate2=rate[data1$Group.3==0 & data1$Group.1==data1$Group.1[i]]
  year1=data1$Group.2[data1$Group.3==0 & data1$Group.1==data1$Group.1[i]]
  lines(year1, rate2, type="b", col=i-5)
}

legend(2013, 50, data1$Group.1[6:10], cex=0.8, col=1:5, pch=21:22, lty=1:2)
title(sub="Source: CDC: Compressed Mortality, 1999-2015", line = 5.5)

#Creating model for the data
data1$Group.1<-factor(data1$Group.1)
model=lm(rate[data1$Group.3==1]~data1$Group.2[data1$Group.3==1]+data1$Group.1[data1$Group.3==1])
plot(model)

#Durbin-Watson Test
install.packages('car')
library('car')
dw<-durbinWatsonTest(model)
dw

#Non-Constant Variance Test
ncv<-ncvTest(model)
ncv

#Applying log transformation to model
model1=lm(log(rate[data1$Group.3==1])~data1$Group.2[data1$Group.3==1]+data1$Group.1[data1$Group.3==1])
plot(model1)

#Durbin-Watson Test
dw<-durbinWatsonTest(model1)
dw

#Non-Constant Variance Test
ncv<-ncvTest(model1)
ncv

#Plotting with confidence intervals 1
plot(rate[data1$Group.3==1]~data1$Group.2[data1$Group.3==1], ylim=c(-3,14), ann=FALSE)
title(main="Regression Lines for Deaths not Related to HIV", font.main=4)
mtext("With 95% Confidence Intervals")
title(xlab="Year")
title(ylab="Deaths per 100,000")
b=coef(model1[1])

abline(b[1],b[2],col=1,lwd=2)
abline(b[1]+b[3],b[2],col=2,lwd=2)
abline(b[1]+b[4],b[2],col=3,lwd=2)
abline(b[1]+b[5],b[2],col=4,lwd=2)
abline(b[1]+b[6],b[2],col=5,lwd=2)

my.conf<-predict(model1, interval="confidence")

for(i in 1:5){
  a1=my.conf[,1][data1$Group.1[data1$Group.3==1]==data1$Group.1[i]] #predict
  a2=my.conf[,2][data1$Group.1[data1$Group.3==1]==data1$Group.1[i]] #uppper
  a3=my.conf[,3][data1$Group.1[data1$Group.3==1]==data1$Group.1[i]] #lower
  
  Year=data1$Group.2[data1$Group.3==1 & data1$Group.1==data1$Group.1[i]]
  
  lines(Year,a1, col=i, lty=2)
  lines(Year,a2, col=i, lty=2)
  lines(Year,a3, col=i, lty=2)
}

legend(2013, 14, data1$Group.1[1:5], cex=0.8, col=1:5, pch=21:22, lty=1:2)
title(sub="Source: CDC: Compressed Mortality, 1999-2015", line = 5.5)

#Plotting with confidence intervals 2
plot(rate[data1$Group.3==1]~data1$Group.2[data1$Group.3==1], ylim=c(-3,14), ann=FALSE)
title(main="Regression Lines for Deaths not Related to HIV: With 95% Confidence Intervals", font.main=4)
title(xlab="Year")
title(ylab="Deaths per 100,000")
b=coef(model1[1])

abline(b[1]+b[7],b[2],col=1,lwd=2)
abline(b[1]+b[8],b[2],col=2,lwd=2)
abline(b[1]+b[9],b[2],col=3,lwd=2)
abline(b[1]+b[10],b[2],col=4,lwd=2)
abline(b[1]+b[11],b[2],col=5,lwd=2)

my.conf<-predict(model1, interval="confidence")

for(i in 6:10){
  a1=my.conf[,1][data1$Group.1[data1$Group.3==1]==data1$Group.1[i]] #predict
  a2=my.conf[,2][data1$Group.1[data1$Group.3==1]==data1$Group.1[i]] #uppper
  a3=my.conf[,3][data1$Group.1[data1$Group.3==1]==data1$Group.1[i]] #lower
  
  Year=data1$Group.2[data1$Group.3==1 & data1$Group.1==data1$Group.1[i]]
  
  lines(Year,a1, col=i-5, lty=2)
  lines(Year,a2, col=i-5, lty=2)
  lines(Year,a3, col=i-5, lty=2)
}

legend(2013, 14, data1$Group.1[6:10], cex=0.8, col=1:5, pch=21:22, lty=1:2)
title(sub="Source: CDC: Compressed Mortality, 1999-2015", line = 5.5)