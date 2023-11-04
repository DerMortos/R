### Ratio estimation
library(survey)
data(api)
## For SRS
srs_design<-svydesign(id=~1,fpc=~fpc,data=apisrs)
svyplot(api.stu~enroll,design=srs_design)
cor(apisrs$api.stu,apisrs$enroll)

resti=svyratio(~api.stu,~enroll,srs_design)
resti
confint(resti)

## Startified ratio estimator
strat_design<-svydesign(id=~1,strata=~stype,fpc=~fpc,data=apistrat)
svyplot(api.stu~enroll,design=strat_design,style = "bubble")
svyplot(api.stu~enroll,design=strat_design,style = "transparent",pch=19)
# Combined ratio estimator 
com<-svyratio(~api.stu,~enroll,strat_design)
com
confint(com)
pre=predict(com,total = 6194)
pre
# Separate ratio estimator 
sep<-svyratio(~api.stu,~enroll,strat_design,separate=TRUE)
sep

pre=predict(sep,total = list(E=181616,H=28282,M=27272))
pre

svyby(~api.stu,~stype,design=strat_design,denom=~enroll,svyratio)

### Box plots
svyboxplot(enroll~stype,design=srs_design)
svyboxplot(enroll~stype,design=strat_design)
