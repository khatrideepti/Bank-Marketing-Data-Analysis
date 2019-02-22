bank <- read.csv("~/Spring 19 Sem/Multi Analysis/bank-additional/bank-additional-full.csv", sep=";")
head(bank)
str(bank)

bank_modified=bank
unique(bank$job)
summary(bank$job)
#unknown = 0 ,student=1, unemployed=2, housemaid=3
#self-employed 4, entrepreneur 5, retired 6, management 7,services 8 , technician 9
# admin 11   blue-collar    10               
bank_job= ifelse(bank$job== 'admin.', 11,
                 ifelse(bank$job=='blue-collar', 10,
                    ifelse(bank$job=='technician',9,
                          ifelse(bank$job=='services',8,
                              ifelse(bank$job=='management',7,
                                 ifelse(bank$job=='retired',6,
                                     ifelse(bank$job=='entrepreneur',5,
                                            ifelse(bank$job=='self-employed',4,
                                                   ifelse(bank$job=='housemaid',3,
                                                          ifelse(bank$job=='unemployed',2,
                                                                 ifelse(bank$job=='student',1,0)))))))))))

#added column in new dataframe bank_modified
bank_modified=cbind(bank_modified,bank_job)
#head(bank_modified[,c('education','bank_education')],30)

#month from factor to numeric
unique(bank$month)
#may jun jul aug oct nov dec mar apr sep
bank_month=ifelse(bank$month=='mar',3,
                  ifelse(bank$month=='apr',4,
                         ifelse(bank$month=='may',5,
                                ifelse(bank$month=='jun',6,
                                       ifelse(bank$month=='jul',7,
                                              ifelse(bank$month=='aug',8,
                                                     ifelse(bank$month=='sep',9,
                                                            ifelse(bank$month=='oct',10,
                                                                   ifelse(bank$month=='nov',11,
                                                                          ifelse(bank$month=='dec',12,0))))))))))


#adding it to data frame bank_modified
bank_modified=cbind(bank_modified,bank_month)


#changing day of the week to numeric
#mon tue wed thu fri
bank_days=ifelse(bank$day_of_week=='mon',1,
                 ifelse(bank$day_of_week=='tue',2,
                        ifelse(bank$day_of_week=='wed',3,
                               ifelse(bank$day_of_week=='thu',4,
                                      ifelse(bank$day_of_week=='fri',5,0)))))


bank_modified=cbind(bank_modified, bank_days)

#loan from factor to numric 

bank_loan= ifelse(bank$loan=='yes',1,0)
bank_modified=cbind(bank_modified,bank_loan) 

#default from factor to numric  
bank_default= ifelse(bank$default=='yes',1,0)
bank_modified=cbind(bank_modified,bank_default) 

#education from factor to numeric in the order of highest count: higher count get the highest number 
bank_education=ifelse(bank$education=='illiterate',1,
                      ifelse(bank$education=='basic.6y',2,
                             ifelse(bank$education=='basic.4y',3,
                                    ifelse(bank$education=='professional.course',4,
                                           ifelse(bank$education=='basic.9y',5,
                                                  ifelse(bank$education=='high.school',6,
                                                         ifelse(bank$education=='university.degree',7,0) ))))))

bank_modified=cbind(bank_modified,bank_education)


bank_contact=ifelse(bank$contact=='cellular',2,1)
bank_modified=cbind(bank_modified,bank_contact)

#changing marital from factor to integer 
#married 3, single 2, divorced 1 and unknown 0
bank_marital=ifelse(bank$marital=='married',3,
                    ifelse(bank$marital=='single',2,
                           ifelse(bank$marital=='divorced',1,0)))

bank_modified=cbind(bank_modified,bank_marital)


#Housing from factor to numeric
bank_housing= ifelse(bank$housing=='yes',1,0)
 
bank_modified=cbind(bank_modified,bank_housing) 
head(bank_modified)

bank_y=ifelse(bank$y=='yes',1,0)
bank_modified=cbind(bank_modified,bank_y)
str(bank_modified)
bank_int = bank_modified[,c('age','duration','campaign','pdays','previous','emp.var.rate','cons.price.idx','cons.conf.idx','euribor3m',
                            'nr.employed','bank_housing','bank_loan','bank_job','bank_education','bank_month',
                            'bank_days','bank_contact','bank_marital','bank_y')]

#x <- dist(scale(bank_int, center = FALSE))
#x
#as.dist(round(as.matrix(x), 2)[1:12, 1:12])

cm <- colMeans(bank_int)
S <- cov(bank_int) #diagonals are variances 
d <- apply(bank_int, MARGIN = 1, function(bank_int)t(bank_int - cm) %*% solve(S) %*% (bank_int - cm))
d
S
cm
str(bank_int)
qqnorm(bank_int[,"age"], main = "age")
#how nomal looks like - univariate normalization
qqline(bank_int[,"age"])

qqnorm(bank_int[,"duration"], main = "duration")
#how nomal looks like - univariate normalization
qqline(bank_int[,"duration"])


qqnorm(bank_int[,"campaign"], main = "campaign")
#how nomal looks like - univariate normalization
qqline(bank_int[,"campaign"])

qqnorm(bank_int[,"pdays"], main = "pdays")
#how nomal looks like - univariate normalization
qqline(bank_int[,"pdays"])

qqnorm(bank_int[,"bank_housing"], main = "bank_housing")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_housing"])

qqnorm(bank_int[,"bank_job"], main = "bank_job")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_job"])

qqnorm(bank_int[,"bank_education"], main = "bank_education")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_education"])

qqnorm(bank_int[,"bank_month"], main = "bank_month")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_month"])

qqnorm(bank_int[,"bank_days"], main = "bank_days")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_days"])

qqnorm(bank_int[,"bank_marital"], main = "bank_marital")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_marital"])

qqnorm(bank_int[,"bank_contact"], main = "bank_contact")
#how nomal looks like - univariate normalization
qqline(bank_int[,"bank_contact"])

#individually they had outliers 
#all of them together or how they interact with each other 
#they look they are normally multivariate
plot(qchisq((1:nrow(bank_int) - 1/2) / nrow(bank_int), df = 3), sort(d),
     xlab = expression(paste(chi[3]^2, " Quantile")),
     ylab = "Ordered distances")
abline(a = 0, b = 1)



t.test(bank$age[bank$y=="yes"],bank$age[bank$y=="no"],var.equal=TRUE)
#Age is not significant

t.test(bank$duration[bank$y=="no"],bank$duration[bank$y=="yes"],var.equal=TRUE)
#Duration is Significant

t.test(bank_modified$bank_job[bank_modified$y=='yes'],bank_modified$bank_job[bank_modified$y=='no'],var.equal=TRUE)
#Job is significant

t.test(bank_modified$bank_housing[bank_modified$y=='yes'],bank_modified$bank_housing[bank_modified$y=='no'],var.equal=TRUE)
#housing is significant

t.test(bank_modified$bank_month[bank_modified$y=='yes'],bank_modified$bank_month[bank_modified$y=='no'],var.equal=TRUE)
#month is signficant 

t.test(bank_modified$bank_loan[bank_modified$y=='yes'],bank_modified$bank_laon[bank_modified$y=='no'],var.equal=TRUE)
#laon is significant

t.test(bank_modified$bank_days[bank_modified$y=='yes'],bank_modified$bank_days[bank_modified$y=='no'],var.equal=TRUE)
#Days of week is significant 

t.test(bank_modified$bank_default[bank_modified$y=='yes'],bank_modified$bank_default[bank_modified$y=='no'],var.equal=TRUE)
#Default is not significant

t.test(bank_modified$bank_month[bank_modified$y=='yes'],bank_modified$bank_month[bank_modified$y=='no'],var.equal=TRUE)
#month is signficant 

t.test(bank_modified$bank_education[bank_modified$y=='yes'],bank_modified$bank_education[bank_modified$y=='no'],var.equal=TRUE)
#Significant 

t.test(bank_modified$bank_contact[bank_modified$y=='yes'],bank_modified$bank_contact[bank_modified$y=='no'],var.equal=TRUE)
#significant

t.test(bank_modified$bank_marital[bank_modified$y=='yes'],bank_modified$bank_marital[bank_modified$y=='no'],var.equal=TRUE)
#significant 

t.test(bank$emp.var.rate[bank$y=="no"],bank$emp.var.rate[bank$y=="yes"],var.equal=TRUE)
#Significant 

t.test(bank$cons.price.idx[bank$y=="no"],bank$cons.price.idx[bank$y=="yes"],var.equal=TRUE)
#Significant 

t.test(bank$cons.conf.idx[bank$y=="no"],bank$cons.conf.idx[bank$y=="yes"],var.equal=TRUE)
#Significant 

t.test(bank$euribor3m[bank$y=="no"],bank$euribor3m[bank$y=="yes"],var.equal=TRUE)
#Significant 

t.test(bank$nr.employed[bank$y=="no"],bank$nr.employed[bank$y=="yes"],var.equal=TRUE)
#Significant 

t.test(bank$campaign[bank$y=="no"],bank$campaign[bank$y=="yes"],var.equal=TRUE)
#significant

library(Hotelling)

t2testbank_int <- hotelling.test( .  ~ bank_y, data=bank_int)
cat("T2 statistic =",t2testbank_int$stat[[1]],"\n")
print(t2testbank_int)
View(t2testbank_int)

#if we include bank_default it becomes singular 
#and it gives error Lapack routine dgesv: system is exactly singular: U[13,13] = 0

#


#no much info p should be less than .05
# testing Variation
# F-test for Total length (not recommended)


#close to 1 F=.7 is not helping 
var.test(bank_int$age[bank_int$bank_y=="1"],bank_int$age[bank_int$bank_y=="0"])

var.test(bank_int$duration[bank_int$bank_y=="1"],bank_int$duration[bank_int$bank_y=="0"])

var.test(bank_int$campaign[bank_int$bank_y=="1"],bank_int$campaign[bank_int$bank_y=="0"])

var.test(bank_int$pdays[bank_int$bank_y=="1"],bank_int$pdays[bank_int$bank_y=="0"])

var.test(bank_int$previous[bank_int$bank_y=="1"],bank_int$previous[bank_int$bank_y=="0"])

var.test(bank_int$emp.var.rate[bank_int$bank_y=="1"],bank_int$emp.var.rate[bank_int$bank_y=="0"])

var.test(bank_int$cons.price.idx[bank_int$bank_y=="1"],bank_int$cons.price.idx[bank_int$bank_y=="0"])

var.test(bank_int$cons.conf.idx[bank_int$bank_y=="1"],bank_int$cons.conf.idx[bank_int$bank_y=="0"])

var.test(bank_int$euribor3m[bank_int$bank_y=="1"],bank_int$euribor3m[bank_int$bank_y=="0"])

var.test(bank_int$nr.employed[bank_int$bank_y=="1"],bank_int$nr.employed[bank_int$bank_y=="0"])


var.test(bank_int$bank_housing[bank_int$bank_y=="1"],bank_int$bank_housing[bank_int$bank_y=="0"])


var.test(bank_int$bank_loan[bank_int$bank_y=="1"],bank_int$bank_loan[bank_int$bank_y=="0"])

var.test(bank_int$bank_job[bank_int$bank_y=="1"],bank_int$bank_job[bank_int$bank_y=="0"])

var.test(bank_int$bank_education[bank_int$bank_y=="1"],bank_int$bank_education[bank_int$bank_y=="0"])

var.test(bank_int$bank_month[bank_int$bank_y=="1"],bank_int$bank_month[bank_int$bank_y=="0"])

var.test(bank_int$bank_days[bank_int$bank_y=="1"],bank_int$bank_days[bank_int$bank_y=="0"])

var.test(bank_int$bank_contact[bank_int$bank_y=="1"],bank_int$bank_contact[bank_int$bank_y=="0"])

var.test(bank_int$bank_marital[bank_int$bank_y=="1"],bank_int$bank_marital[bank_int$bank_y=="0"])


# Leverne test is used to verify Homoscedasticity. It tests if the variance of two samples are # #equal. Levene's test is an inferential statistic used to assess the equality of variances for a #variable calculated for two or more groups.[1] Some common statistical procedures assume that #variances of the populations from which different samples are drawn are equal. Levene's test #assesses this assumption.

bank_int$y=bank$y
library(car)
leveneTest(age ~ y, data=bank_int)
#leveneTest() produces a two-sided test
leveneTest(duration ~ y, data=bank_int)
leveneTest(campaign ~ y, data=bank_int)
leveneTest(pdays ~ y, data=bank_int)
leveneTest(previous ~ y, data=bank_int)
leveneTest(emp.var.rate ~ y, data=bank_int)
leveneTest(cons.conf.idx ~ y, data=bank_int)
leveneTest(cons.price.idx ~ y, data=bank_int)
leveneTest(euribor3m ~ y, data=bank_int)
leveneTest(nr.employed ~ y, data=bank_int)
leveneTest(bank_housing ~ y, data=bank_int)
leveneTest(bank_loan ~ y, data=bank_int)
leveneTest(bank_job ~ y, data=bank_int)
leveneTest(bank_education ~ y, data=bank_int)
leveneTest(bank_month ~ y, data=bank_int)
leveneTest(bank_days ~ y, data=bank_int)
leveneTest(bank_contact ~ y, data=bank_int)
leveneTest(bank_marital ~ y, data=bank_int)


#variance are not playing much role , so standarization helped a little after we had humerous


