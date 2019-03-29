bank <- read.csv("~/Spring 19 Sem/Multi Analysis/bank-full.csv", sep=";")
str(bank)
attach(bank)

library(ggplot2)
ggplot(bank, aes(y=age,x=education, fill=y)) + geom_boxplot()
ggplot(bank, aes(y=age,x=housing, fill=y)) + geom_boxplot()
ggplot(bank, aes(y=age,x=marital, fill=y)) + geom_boxplot()
ggplot(bank, aes(y=balance,x=job, fill=y)) + geom_boxplot()

boxplot(age ~ job)
unique(bank$default)
default_dummy=ifelse(bank$default=='yes',1,0) 

unique(bank$housing)
housing_dummy=ifelse(bank$housing=='yes',1,0) 

unique(bank$loan)
loan_dummy=ifelse(bank$loan=='yes',1,0) 

unique(bank$contact)
cell_dummy=ifelse(bank$contact=='cellular',1,0)

unique(bank$marital)
married_dummy=ifelse(bank$marital=='married',1,0)
divorced_dummy=ifelse(bank$marital=='divorced',1,0)

admin_dummy=ifelse(bank$job== 'admin.',1,0)
bluecollar_dummy=ifelse(bank$job== 'blue-collar',1,0)
technician_dummy=ifelse(bank$job== 'technician',1,0)
services_dummy=ifelse(bank$job=='services',1,0)
management_dummy=ifelse(bank$job=='management',1,0)
retired_dummy=ifelse(bank$job=='retired',1,0)
entrepreneur_dummy=ifelse(bank$job=='entrepreneur',1,0)
selfemployed_dummy=ifelse(bank$job=='self-employed',1,0)
housemaid_dummy=ifelse(bank$job=='housemaid',1,0)
unemployed_dummy=ifelse(bank$job=='unemployed',1,0)
student_dummy=ifelse(bank$job=='student',1,0)

bank1=data.frame(age,balance,day,duration,campaign,pdays,previous,default_dummy,housing_dummy,loan_dummy,cell_dummy,married_dummy,divorced_dummy,
                 admin_dummy,bluecollar_dummy,technician_dummy,services_dummy,management_dummy,retired_dummy,
                 entrepreneur_dummy,selfemployed_dummy,housemaid_dummy,unemployed_dummy,student_dummy)

# Computing Correlation Matrix
corrm.job <- cor(bank1)
corrm.job
plot(corrm.job)
bank_pca <- prcomp(bank1, scale=TRUE)
bank_pca
summary(bank_pca)

plot(bank_pca)
(eigen_bank <- bank_pca$sdev^2) 
names(eigen_bank) <- paste("PC",1:24,sep="")
eigen_bank
head(bank_pca$x)

dim(bank_pca$x)

# Identifying the scores by their fixed deposit status
jobtyp_pca <- data.frame((bank$y),bank_pca$x)
head(jobtyp_pca)
# Means of scores for all the PC's classified by Survival status
tabmeansPC <- aggregate(jobtyp_pca[,2:25],by=list(y=bank$y),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$y)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans
# Standard deviations of scores for all the PC's classified by Survival status
tabsdsPC <- aggregate(jobtyp_pca[,2:25],by=list(y=bank$y),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

plot(bank_pca)
summary(bank_pca)

plot(eigen_bank, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Can take till PC5 i.e. here 7 features reduced to 5

library(psych)
vss(bank1)
#therefore choosing 8 Factors 

#Oblique rotation 
fit.pc <- fa(bank1, nfactors=8, rotate="oblimin")

fit.pc

#what are factors for each variable , what is affectin and
#Fit based upon off diagonal values = 0.96 higher the better
#residual should be less
round(fit.pc$values, 3)
fit.pc$loadings
#View(fit.pc)
# Loadings with more digits
for (i in c(1,2,3,4,5,6,7,8)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
fit.pc$scores
# Play with FA utilities

fa.parallel(bank1) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship




