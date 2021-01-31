mod1 <- lm(hrinc~sex+educ, data=data1)
summary(mod1)
mod1$coefficients[2]/mod1$coefficients[3]

mod2 <- lm(hrinc~sex*educ, data=data1)
summary(mod2)

library(arm)
sims <- sim(mod2,1000)                                          #蒙特卡洛算区间
values <- c(1,1,15,15)
coef <- coef(sims)
sampling.dist <- values %*% t(coef)
quantile(sampling.dist, c(0.025,0.975))

mod3 <- lm(hrinc~sex+educ*age, data=data1)
summary(mod3)

u$work <- ifelse(u$studytim<53,1,0)
mod1 <- lm(work~age + health + edu + exp + soccap + msrch, data = u)
summary(mod1)
pred.prob = mod1$coef[1] + mod1$coef[2]*55 + mod1$coef[3]*4 +
  mod1$coef[4]*5 + mod1$coef[5] + mod1$coef[6]
pred.prob
plot(u$age,fitted.values(mod1),                                 #预测值
     ylim=c(-0.2,1.2),
     ylab="Predicted Probability",
     xlab="Age",
     las=1)
abline(h=0,lty=3)
abline(h=1,lty=3)

mod2 <- glm(work~age + health + edu + exp + soccap + msrch,    #罗辑回归
            family=binomial(link="logit"), data = u)
summary(mod2)
summary(predict(mod2,type="link"))                             #Logit units
summary(predict(mod2,type="response"))                         #Predicted Probabilities
pred.logits <- mod2$coef[1] + mod2$coef[2]*55 + mod2$coef[3]*4 +
  mod2$coef[4]*5 + mod2$coef[5] + mod2$coef[6]
pred.logits
library(boot)
inv.logit(pred.logits)

set.seed(1)
training.rows <-sample(nrow(u),(nrow(u)/ 2))                   #抽样技术
training.data <- u[training.rows,]
test.data <- u[-training.rows,]
mod3 <- glm(work~age + health + edu + exp + soccap + msrch,
            family=binomial(link="logit"), data = training.data)
summary(mod3)
logit.probs.test <- predict(mod3,test.data, type="response")
logit.preds.test <- ifelse(logit.probs.test>0.5,1,0)
table(logit.preds.test,test.data$work)                        #先行后列
(13+21)/nrow(test.data)                                       #error rate
48/(48+13)                                                    #sensitivity
39/(21+39)                                                    #specificity

library(pROC)                                                 #ROC
rocplot <- roc(test.data$work,logit.probs.test)
rocplot$auc
plot(rocplot,legacy.axes=T)

mod4 <- glm(work~health + age*edu + age*exp + soccap + msrch,
            family=binomial(link="logit"), data = training.data)
summary(mod4)
logit.preds.test2 <- ifelse(predict(mod4,test.data, type="response")>0.5,1,0)
table(logit.preds.test2,test.data$work)
44/(44+17)
37/(23+37)
logit.probs.test2 <- predict(mod4,test.data, type="response")
rocplot2 <- roc(test.data$work,logit.probs.test2)
rocplot2$auc
plot(rocplot2,legacy.axes=T)


logit.probs.training.mod3 <- predict(mod3,type="response")
logit.preds.training.mod3 <- ifelse(logit.probs.training.mod3>0.5,1,0)
table(logit.preds.training.mod3,training.data$work)
logit.probs.training.mod4 <- predict(mod4,type="response")
logit.preds.training.mod4 <- ifelse(logit.probs.training.mod4>0.5,1,0)
table(logit.preds.training.mod4,training.data$work)
