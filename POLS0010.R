##Part A
#half data for training and half for validation
training.rows <-sample(nrow(a),(nrow(a)/ 2))
training.data <- a[training.rows,]
test.data <- a[-training.rows,]
#1. build logit model 
mod1 <- glm(sentences ~ age + sex + urban + leftright + married + 
              wclass + hincome + university,
            family=binomial(link='logit'),data = training.data)
summary(mod1)
logit.probs.test <- predict(mod1,test.data, type="response")
logit.preds.test <- ifelse(logit.probs.test>0.5,1,0)            #threshold = 0.5
table(logit.preds.test,test.data$sentences)                     #confusion matrix for the classifications
(85+287)/nrow(test.data)                                        #error rate
711/(711+85)                                                    #sensitivity
91/(91+287)                                                     #specificity
library(pROC)
rocplot <- roc(test.data$sentences,logit.probs.test)
rocplot$auc
plot(rocplot,legacy.axes=T)
#2.	Model optimization - age as marginal effects
mod2 <- glm(sentences ~ sex + urban + age * leftright + married + 
              wclass + age * hincome + university,
            family=binomial(link='logit'),data = training.data)
summary(mod2)
logit.probs.test <- predict(mod2,test.data, type="response")
logit.preds.test <- ifelse(logit.probs.test>0.5,1,0)            #same as before
table(logit.preds.test,test.data$sentences)
(710+86)/nrow(test.data)                                        #error rate
710/(710+86)                                                    #sensitivity
86/(86+292)                                                     #specificity
library(pROC)
rocplot <- roc(test.data$sentences,logit.probs.test)
rocplot$auc
plot(rocplot,legacy.axes=T)
#2.	Model optimization - Stepwise regression
mod3 <- glm(sentences ~  urban + leftright  + hincome + university,
            family=binomial(link='logit'),data = training.data)
summary(mod3)
logit.probs.test <- predict(mod3,test.data, type="response")
logit.preds.test <- ifelse(logit.probs.test>0.5,1,0)
table(logit.preds.test,test.data$sentences)
(81+288)/nrow(test.data)                                        #error rate
715/(715+81)                                                    #sensitivity
90/(90+288)                                                     #specificity
library(pROC)
rocplot <- roc(test.data$sentences,logit.probs.test)
rocplot$auc
plot(rocplot,legacy.axes=T)


#################################
##Part B
#logit model
reg1 <- glm(marriage ~ age_cat + edu_cat + female + p_evang + p_metroarea + 
              kerry_04 + p65over + pbachelors,
            family=binomial(link="logit"),data=g)
summary(reg1)
#logistic multilevel model
reg2 <- glmer(marriage ~ age_cat + edu_cat + female + p_evang + p_metroarea + 
                kerry_04 + p65over + pbachelors + (1|state),
              family=binomial(link="logit"),nAGQ=0,data=g)
summary(reg2)
#validation and post-stratified estimates
post$state <- as.character(post$state)                        #make sure it is char
post$prediction <- predict(reg2,newdata=post,type="response",allow.new.levels=TRUE)
post$weight.pred <- post$prediction*post$stateperc*100        #weighted pred
results2 <- data.table(post)[ , .(final.est = sum(weight.pred)), by = .(state)]   #group by state
results2
boxplot(results2$final.est)
median(results2$final.est)
mean(results2$final.est)
#MAE
MAE <- sum(abs(results2$final.est-est$estimate))/51
MAE
