bank <- read.csv("~/Spring 19 Sem/Multi Analysis/bank-additional/bank-additional-full.csv", sep=";")
head(bank)
str(bank)

bank_pca_data=bank[,c(1,11:14,16:20)]
str(bank_pca_data)

summary(bank_pca_data)
cor(bank_pca_data)
plot(cor(bank_pca_data))

# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one

bank_pca <- prcomp(bank_pca_data,scale=TRUE)
plot(bank_pca)
summary(bank_pca)


#View(bank_pca)
# x has new values of data , after 
(eigen_bank <- bank_pca$sdev^2)  
names(eigen_bank) <- paste("PC",1:10,sep="")
eigen_bank
sumlambdas <- sum(eigen_bank)
sumlambdas
propvar <- eigen_bank/sumlambdas
propvar
cumvar_bank <- cumsum(propvar)
cumvar_bank
matlambdas <- rbind(eigen_bank,propvar,cumvar_bank)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(bank_pca)
bank_pca$rotation
print(bank_pca)
# Sample scores stored in bank_pca$x
head(bank_pca$x)

# Identifying the scores by their survival status
banktyp_pca <- cbind(data.frame(bank$y),bank_pca$x)
head(banktyp_pca)

# Means of scores for all the PC's classified by Cumstomer's response towards fixed deposit
tabmeansPC <- aggregate(banktyp_pca[,2:11],by=list(y=bank$y),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$y)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans
# Standard deviations of scores for all the PC's classified by Bank$y
tabsdsPC <- aggregate(banktyp_pca[,2:11],by=list(y=bank$y),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds


#t test on all the Principal Components 
t.test(PC1~bank$y,data=banktyp_pca)
t.test(PC2~bank$y,data=banktyp_pca)
t.test(PC3~bank$y,data=banktyp_pca)
t.test(PC4~bank$y,data=banktyp_pca)
t.test(PC5~bank$y,data=banktyp_pca)
t.test(PC6~bank$y,data=banktyp_pca)
t.test(PC7~bank$y,data=banktyp_pca)
t.test(PC8~bank$y,data=banktyp_pca)
t.test(PC9~bank$y,data=banktyp_pca)
t.test(PC10~bank$y,data=banktyp_pca)

# F ratio tests
var.test(PC1~bank$y,data=banktyp_pca)
var.test(PC2~bank$y,data=banktyp_pca)
var.test(PC3~bank$y,data=banktyp_pca)
var.test(PC4~bank$y,data=banktyp_pca)
var.test(PC5~bank$y,data=banktyp_pca)
var.test(PC6~bank$y,data=banktyp_pca)
var.test(PC7~bank$y,data=banktyp_pca)
var.test(PC8~bank$y,data=banktyp_pca)
var.test(PC9~bank$y,data=banktyp_pca)
var.test(PC10~bank$y,data=banktyp_pca)


# Levene's tests (one-sided)
library(car)

(LTPC1 <- leveneTest(PC1~bank$y,data=banktyp_pca))
(LTPC1 <- leveneTest(PC1~bank$y,data=banktyp_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~bank$y,data=banktyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~bank$y,data=banktyp_pca))
(p_PC3_1sided=LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(PC4~bank$y,data=banktyp_pca))
(p_PC4_1sided=LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(PC5~bank$y,data=banktyp_pca))
(p_PC5_1sided=LTPC5[[3]][1]/2)
(LTPC6 <- leveneTest(PC6~bank$y,data=banktyp_pca))
(p_PC6_1sided=LTPC6[[3]][1]/2)
(LTPC7 <- leveneTest(PC7~bank$y,data=banktyp_pca))
(p_PC7_1sided=LTPC7[[3]][1]/2)
(LTPC8 <- leveneTest(PC8~bank$y,data=banktyp_pca))
(p_PC8_1sided=LTPC8[[3]][1]/2)
(LTPC9 <- leveneTest(PC9~bank$y,data=banktyp_pca))
(p_PC9_1sided=LTPC9[[3]][1]/2)
(LTPC10 <- leveneTest(PC10~bank$y,data=banktyp_pca))
(p_PC10_1sided=LTPC10[[3]][1]/2)


# Plotting the scores for the first and second components
plot(banktyp_pca$PC1, banktyp_pca$PC2,pch=ifelse(banktyp_pca$bank.y == "yes",1,16),xlab="PC1", ylab="PC2", main="Customer Response against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("yes","no"), pch=c(1,16))


plot(eigen_bank, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
#where bending - chossing pC componeet or >.7
#6
plot(log(eigen_bank), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
#9 are good 
print(summary(bank_pca))
View(bank_pca)
diag(cov(bank_pca$x))
xlim <- range(bank_pca$x[,1])
head(bank_pca$x[,1])
head(bank_pca$x)
plot(bank_pca$x,xlim=xlim,ylim=xlim)
bank_pca$rotation[,1]
bank_pca$rotation[,2]
bank_pca$rotation[,3]
bank_pca$rotation
#get the original value of the data based on PCA
#center <- bank_pca$center
#scale <- bank_pca$scale
#new_bank <- as.matrix(bank_pca_data)
#head(new_bank)
#drop(scale(new_bank,center=center, scale=scale)%*%bank_pca$rotation[,1])
#drop(new_bank%*%bank_pca$rotation[,1])
#predict(bank_pca)[,1]
#scale it back up 
#The aboved two gives us the same thing. predict is a good function to know.


#Name these PC

sparrows_pca$rotation[,1]
#take 50% of length - generally speaking 

sparrows_pca$rotation[,2]
# this gives shape 
