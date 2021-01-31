mod1 <- glm(work ~ age + health + edu + exp + soccap + msrch,
            family=binomial(link='logit'),data = u)
exp(coef(mod1))
100*(exp(coef(mod1))[5]-1)
100*(exp(coef(mod1))[2]-1)

library(mfx)
logitmfx(mod1,data=u)

logitmfx(mod1,atmean=F,data=u)

library(arm)
library(boot)
set.seed(1)
regsims <-sim(mod1,n.sims=1000)
coefs <-coef(regsims)
values1 <- c(1,mean(u$age),
             median(u$health),
             median(u$edu),
             median(u$exp),
             median(u$soccap),
             min(u$msrch))
pred.outcomes1 <- inv.logit(values1 %*% t(coefs))
values2 <- c(1,mean(u$age),
             median(u$health),
             median(u$edu),
             median(u$exp),
             median(u$soccap),
             max(u$msrch))
pred.outcomes2 <- inv.logit(values2 %*% t(coefs))
pred.diff <- pred.outcomes2 - pred.outcomes1
mean(pred.diff)
quantile(pred.diff, c(0.025,0.975))


t$firstclass <- ifelse(t$thirdclass==0&t$secondclass==0&t$crew==0,1,0)
mod.t <- glm(survive~crew + thirdclass + firstclass + adult + male,
             family=binomial(link='logit'),data = t)
summary(mod.t)

set.seed(1)
titanicsims <-sim(mod.t,n.sims=1000)
coefs <-coef(titanicsims)
values1 <- c(1,
             crew=0,
             thirdclass=0,
             firstclass=1,
             adult=1,
             male=0)
pred.outcomes1 <- inv.logit(values1 %*% t(coefs))
values2 <- c(1,
             crew=0,
             thirdclass=1,
             firstclass=0,
             adult=1,
             male=0)
pred.outcomes2 <- inv.logit(values2 %*% t(coefs))
pred.diff <- pred.outcomes1 - pred.outcomes2
mean(pred.diff)
quantile(pred.diff, c(0.025,0.975))


# first class vs. second class
values1 <- c(1,
             crew=0,
             thirdclass=0,
             firstclass=1,
             adult=1,
             male=0)
pred.outcomes1 <- inv.logit(values1 %*% t(coefs))
values2 <- c(1,
             crew=0,
             thirdclass=0,
             firstclass=0,
             adult=1,
             male=0)
pred.outcomes2 <- inv.logit(values2 %*% t(coefs))
pred.diff1 <- pred.outcomes1 - pred.outcomes2
mean1 <- mean(pred.diff1)
###############
# crew vs. second class
values1 <- c(1,
             crew=1,
             thirdclass=0,
             firstclass=0,
             adult=1,
             male=0)
pred.outcomes1 <- inv.logit(values1 %*% t(coefs))
values2 <- c(1,
             crew=0,
             thirdclass=0,
             firstclass=0,
             adult=1,
             male=0)
pred.outcomes2 <- inv.logit(values2 %*% t(coefs))
pred.diff2 <- pred.outcomes1 - pred.outcomes2
mean2 <- mean(pred.diff2)
###############
#third class vs. second class
values1 <- c(1,
             crew=0,
             thirdclass=1,
             firstclass=0,
             adult=1,
             male=0)
pred.outcomes1 <- inv.logit(values1 %*% t(coefs))
values2 <- c(1,
             crew=0,
             thirdclass=0,
             firstclass=0,
             adult=1,
             male=0)
pred.outcomes2 <- inv.logit(values2 %*% t(coefs))
pred.diff3 <- pred.outcomes1 - pred.outcomes2
mean3 <- mean(pred.diff3)
cis=data.frame(matrix(NA,3,4))
colnames(cis) <- c("estimate", "ci_lo", "ci_hi","num")
cis[1,1] = mean1
cis[1,2] = quantile(pred.diff1, 0.025)
cis[1,3] = quantile(pred.diff1, 0.975)
cis[2,1] = mean2
cis[2,2] = quantile(pred.diff2, 0.025)
cis[2,3] = quantile(pred.diff2, 0.975)
cis[3,1] = mean3
cis[3,2] = quantile(pred.diff3, 0.025)
cis[3,3] = quantile(pred.diff3, 0.975)
cis[,4] <- c(1:3)
library(ggplot2)
ggplot(cis, aes(y=num, x=estimate)) +
  geom_errorbarh(aes(xmin=ci_lo, xmax=ci_hi), height=.1, col="red") +
  geom_point(aes(y=num, x=estimate), size=2, col="red", shape=21, fill="red") +
  geom_vline(xintercept=0) +
  scale_x_continuous(name="Change in Probability", limits=c(-0.4,0.4)) +
  scale_y_discrete(name="",
                   limits=c("First Class",
                            "Crew",
                            "Third Class"))

