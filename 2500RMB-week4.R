length(unique(n$stnnum))
length(n$stnnum)/length(unique(n$stnnum))
reg1 <- lm(nitrates ~ gdp+gdpsq+meantemp+year,data=n)
summary(reg1)

new.gdp <- (n$gdp[n$year==4 & n$stnnum==1001])*1.03
reg1$coef[1] + reg1$coef[2]*new.gdp + reg1$coef[3]*new.gdp*new.gdp +
  reg1$coef[4]*n$meantemp[n$year==4 & n$stnnum==1001] + reg1$coef[5]*5

library(lme4)
re1 <- lmer(nitrates ~ gdp+gdpsq+meantemp+year+(1|stnnum),data=n)
summary(re1)

coef(re1)$stnnum[1,2]

m1 <- lmer(math ~ white + male + homework + (1|id), data=s)
summary(m1)
15.71/(15.71+70.39)
coef(m1)
coef(m1)$id[8,1] + coef(m1)$id[8,2] + coef(m1)$id[8,3] + coef(m1)$id[8,4]*6
m2 <- lmer(math ~ white + male + homework + parentcollege + (1|id), data=s)
summary(m2)
m3 <- lmer(math ~ white + male + homework + parentcollege + south + ratio
           + (1|id), data=s)
summary(m3)
m4 <- lmer(math ~ white + male + homework + parentcollege + south + ratio +
             (1 + homework + parentcollege|id), data=s)
summary(m4)
coef(m4)
library(arm)
se.ranef(m4)
coef(m4)$id[1,5]/se.ranef(m4)$id[1,3]
coef(m4)$id[1,4]/se.ranef(m4)$id[1,2]