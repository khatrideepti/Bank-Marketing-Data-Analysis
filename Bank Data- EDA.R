bank <- read.csv("~/Spring 19 Sem/Multi Analysis/bank-additional/bank-additional-full.csv", sep=";")


boxplot(bank$age, main="Age Box plot",yaxt="n", xlab="Age", horizontal=TRUE)
boxplot(bank$euribor3m, main="Euribor3m Box plot",yaxt="n", xlab="euribor3m", horizontal=TRUE)
boxplot(bank$duration, main="Duration Box plot",yaxt="n", xlab="Duration", horizontal=TRUE)

library(MVA)
#Chiplot
mlab = "Age of the Customer"
plab = "Duration"
with(bank, plot(age, duration, xlab = mlab, ylab = plab, cex.lab = 0.9))
with(bank, chiplot(age, duration))

#bvplot
bank_age_dur=data.frame(bank$age, bank$duration)
bvbox(bank_age_dur, mtitle = "", xlab = mlab, ylab = plab)

y_int=ifelse(bank$y=='no', 0, 1)

plot(bank$age, bank$duration, pch=c(1,16)[y_int],xlab="Age",ylab="Duration")
plot(bank$age, bank$campaign, pch=c(1,16)[y_int],xlab="Age", ylab="Campaign")

#3d scatterplot
library(scatterplot3d)
s3d <- scatterplot3d(bank$age,bank$duration,bank$campaign,pch=c(1,16)[as.numeric(bank$y)],xlab="Age", ylab="Duration", angle=45,zlab="Campaign", lty.hide=2,type="h",y.margin.add=0.1,font.axis=2,font.lab=2)

     
library(ggplot2)
ggplot(bank,aes(x=bank$age,fill=bank$y)) + geom_histogram(binwidth=1) +
  labs(y= "Count", x="Age", title = "Age")
ggplot(bank, aes(x=bank$job,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Job", title = "Job")
ggplot(bank, aes(x=bank$marital,fill=bank$y)) + geom_bar() +
  labs(y= "Count", x="Marital", title = "Marital")
ggplot(bank, aes(x=bank$education,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Education", title = "Education")
ggplot(bank, aes(x=bank$default,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Contact", title = "Contact")
ggplot(bank, aes(x=bank$housing,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Contact", title = "Contact")
ggplot(bank, aes(x=bank$contact,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Contact", title = "Contact")
ggplot(bank, aes(x=bank$loan,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Laon", title = "Loan")
ggplot(bank, aes(x=bank$month,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Month", title = "Month")
ggplot(bank, aes(x=bank$day_of_week,fill=bank$y)) + geom_bar()+
  labs(y= "Count", x="Day of week", title = "Day of week")
ggplot(bank, aes(x=bank$poutcome,fill=bank$y)) + geom_bar() +
  labs(y= "Count", x="poutcome", title = "Poutcome")

#correlation analysis
bank_int=bank[c(1,11:14)]
pairs(bank_int, labels=c("age","Duration","Campaign","pdays","previous"),pch=c(1,16)[y_int],font.labels=2)

#Diagonal boxplot
library(SciViews)
pairs(bank_int, diag.panel = panel.boxplot, labels=c("Duration","Campaign","pdays","previous"),pch=c(1,16)[y_int], font.labels=2)

library(car)
scatterplotMatrix(~age+duration+campaign+pdays+previous | bank$y, data=bank_int, var.labels=c("age","Duration","Campaign","pdays","previous"),cex.labels=0.7, diagonal="boxplot",smooth=FALSE,reg.line=FALSE,pch=c(1,16),col=rep("black",2), legend.plot=FALSE)

#Instead of using splom using psych library it includes splom , and give better correlation for factor features
library(psych)
pairs.panels(bank[,c(1:8,21)])
pairs.panels(bank[,c(9:21)])





