## Chapter 4 Simple Random Sampling

library(survey)

# Loads specified data sets, or list the available data sets.
# api = Student performance in California Schools(column within data sets)
data(api)
srs_design<-svydesign(id=~1,fpc=~fpc,data=apisrs)
srs_design

# Compute means, variances, ratios and totals for data from complex surveys.
svytotal(~enroll,srs_design)
svymean(~enroll,srs_design)
confint(svymean(~enroll,srs_design))

# We should have looked first at the histogram of enroll
summary(apisrs$enroll)
hist(apisrs$enroll)

# The confidence interval is not accurate but by the central limit theorem it will work
#
# The CLT states that, given a sufficiently large sample size from a population, the mean 
# of all samples from the same population will be approximately equal to the mean of the
# original population. It also states that as you increase the number of samples and the
# sample size, the distribution of all of the sample means will approximate a normal
# distribution (aka Gaussian distribution) — no matter what the population distribution is.
# This distribution is referred to as the “sampling distribution.”

# Functions and datasets to support Venables and Ripley,
# "Modern Applied Statistics with S" (4th edition, 2002).
library(MASS)

bc<-boxcox(apisrs$enroll~1,plotit=T)
lambdabc<-bc$x[which.max(bc$y)]
BCENROLL=((apisrs$enroll)^lambdabc-1)/lambdabc
hist(BCENROLL)

qqnorm(BCENROLL)
qqline(BCENROLL)

shapiro.test(BCENROLL)

BCSTAT=svymean(BCENROLL,srs_design)
BCSTAT=as.data.frame(BCSTAT)

LL=BCSTAT[[1]]-qnorm(.975)*BCSTAT[[2]]
UL=BCSTAT[[1]]+qnorm(.975)*BCSTAT[[2]]
c(LL,UL)


##### Select a new SRS from apipop
SRS<-sample(nrow(apipop),200,replace=F)
newSRS=apipop[SRS,]
srs_design2<-svydesign(id=~1,fpc=rep(nrow(apipop),200),data=newSRS)


### To use weights
srs_design3<-svydesign(id=~1,weights=~pw,data=apisrs)
svytotal(~enroll,srs_design3)
svymean(~enroll,srs_design3)
confint(svymean(~enroll,srs_design3))


##### Select a new SRS from apipop using weights for estimaion
SRS2<-sample(nrow(apipop),200,replace=F)
newSRS2=apipop[SRS2,]
pw=6194/200
srs_design2<-svydesign(id=~1,weight=rep(pw,200),data=newSRS2)

