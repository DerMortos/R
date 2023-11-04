## Chapter 5 Stratified Random Sampling
# install.packages("survey")
library(survey)
data(api)
strat_design<-svydesign(id=~1,strata=~stype,fpc=~fpc,data=apistrat)
strat_design
svytotal(~enroll,strat_design)
enrollmean=svymean(~enroll,strat_design)
confint(enrollmean)

##### Select a new Stratified sample from apipop
# Some prep
tab1=table(apipop$stype)
tab2=table(apistrat$stype)
tab1/tab2
names(tab1)
names(tab1)[1]
sum(tab1)
sum(tab2)
tab1[["E"]]
tab1[[names(tab1)[1]]]
tab=rbind(tab1,tab2)
tab

## Now the strat with proportional allocation
tab1=table(apipop$stype)
nsam=200
tabn<-round(nsam*tab1/sum(tab1))
tabn

s<-c()
for(i in 1:length(tab1)){
  s<-c(s,sample(which(apipop$stype==names(tab1)[i]),tabn[i],replace=F))
}

startsam=apipop[s,]
#assign fpc from tab1
startsam$fpc=tab1[startsam$stype]
#probability/weight
startsam$pw=tab1[startsam$stype]/tabn[startsam$stype]

strat_design<-svydesign(id=~1,strata=~stype,fpc=~fpc,data=startsam)
strat_design
svytotal(~enroll, na.rm=TRUE, strat_design)
enrollmean=svymean(~enroll, na.rm=TRUE, strat_design)
confint(enrollmean)

### Cumulative square root of frequency method
L=3 # to stratify by enroll

tab3=ftable(apipop$enroll)
View(tab3)

tab4= cumsum(sqrt(tab3))
View(tab4)

divider=c()
for(i in 1:(L-1)){divider[i]=which.min(abs(tab4 - tail(tab4,1)*i/L))}
divider
tab3=as.data.frame(tab3)
tab3[divider,1]
