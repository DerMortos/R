#### Lesson 5 Cluster Sampling
############
library(survey)
data(api) 
N=length(unique(apipop$dnum))   ## district numbers (some missing)
N
n=length(unique(apiclus1$dnum)) ## sample size
n
tab=table(apipop$dnum)          ## table showing schools in district
View(tab)
dclus1<-svydesign(id=~dnum,data=apiclus1,fpc=~fpc)  ## id = ~ dnum (all schools with each dnum fall under sampling unit)
svymean(~enroll,dclus1)


#### Sampling with a probability
P=c(1200,450,2100,860,2840,1910,290,3200)
sam=sample(8,3,replace=F,prob=P)
sam
P[sam]

#### Two-Phase Cluster sampling
dclus2<-svydesign(id=~dnum+snum,fpc=~fpc1+fpc2,data=apiclus2)
sm=svymean(~enroll,dclus2,na.rm=TRUE)
sm
confint(sm)
