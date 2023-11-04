# Chapter 3 Statistics

#figure 3.1
n_repl=1000
newlist0=replicate(n_repl,sample(0:9,10,replace=TRUE))
ybar0=colMeans(newlist0)
mean(ybar0)
hist(ybar0,scale="frequency")
sd(ybar0)
sd(ybar0)*sqrt(10)

CI1=c(mean(ybar0)-1*sd(ybar0),mean(ybar0)+1*sd(ybar0))
sum((ybar0>CI1[1])*(ybar0<CI1[2]))/length(ybar0)

CI2=c(mean(ybar0)-2*sd(ybar0),mean(ybar0)+2*sd(ybar0))
sum((ybar0>CI2[1])*(ybar0<CI2[2]))/length(ybar0)

## Cor between u_i and delta_i
cor(c(1,2,3,4),c(.1,.1,.4,.4))

##############

# section 3.4
library(readxl)
sleep <- read_excel("/Users/HerrNorb/Library/Mobile Documents/com~apple~CloudDocs/SP '21/STAT 3336 Sampling/R code and docs/sleep.xlsx")
View(sleep)

# The database is attached to the R search path. This means that the database is searched by R 
# when evaluating a variable, so objects in the database can be accessed by simply giving their names.
attach(sleep)

N=length(BrainWt)
hist(BrainWt,scale="frequency")
mean(BrainWt)

# Population standard deviation
sd(BrainWt)*sqrt((N-1)/N) 

######

n_repl=100
n=40
newlist1=replicate(n_repl,sample(BrainWt,n,replace=FALSE))
ybar1=colMeans(newlist1)
hist(ybar1,scale="frequency")

######

n_repl=1000
n=5
LBW=log(BrainWt)
newlist2=replicate(n_repl,sample(LBW,n,replace=FALSE))
ybar2=colMeans(newlist2)
hist(ybar2,scale="frequency",xlab = "Mean of the logarithm of brain weight",main = "Sampling distribution")
mean(ybar2)
sd(ybar2)

mean(LBW)
sd(LBW)*sqrt((N-1)/N)
sd(LBW)*sqrt((N-1)/N)/sqrt(n)

CI1=c(mean(ybar2)-1*sd(ybar2),mean(ybar2)+1*sd(ybar2))
sum((ybar2>CI1[1])*(ybar2<CI1[2]))/length(ybar2)

CI2=c(mean(ybar2)-2*sd(ybar2),mean(ybar2)+2*sd(ybar2))
sum((ybar2>CI2[1])*(ybar2<CI2[2]))/length(ybar2)

###########################
## Box-Cox transformation
library(MASS)
bc<-boxcox(BrainWt~1,plotit=TRUE)
lambdabc<-bc$x[which.max(bc$y)]

n_repl=1000
n=5
BCBW=((BrainWt)^lambdabc -1)/lambdabc
newlist3=replicate(n_repl,sample(BCBW,n,replace=FALSE))
ybar3=colMeans(newlist3)
hist(ybar3,scale="frequency",xlab = "Mean of the Box-Cox of brain weight",main = "Sampling distribution")


CI1=c(mean(ybar3)-1*sd(ybar3),mean(ybar3)+1*sd(ybar3))
sum((ybar3>CI1[1])*(ybar3<CI1[2]))/length(ybar3)

CI2=c(mean(ybar3)-2*sd(ybar3),mean(ybar3)+2*sd(ybar3))
sum((ybar3>CI2[1])*(ybar3<CI2[2]))/length(ybar3)

qqnorm(BCBW)
qqline(BCBW)

shapiro.test(BCBW)
