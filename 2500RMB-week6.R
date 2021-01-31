cmeans <- tapply(e$leave,e$cname,mean)
cmeans_data <- as.data.frame(cbind(cmeans,sort(unique(e$cname))))
colnames(cmeans_data) <- c("cmeans","Constituency")
cmeans_data$cmeans[cmeans_data$Constituency=="Epping Forest"]
cmeans_data$cmeans[cmeans_data$Constituency=="Reading East"]

reg1 <- glm(leave ~ votecon + voteukip + female + age + highed + lowed,
            family=binomial(link="logit"),data=e)
summary(reg1)


post$prediction <- predict(reg1,newdata=post,type="response",allow.new.levels=TRUE)
post$weight.pred <- post$prediction*post$percent*100

library(data.table)
results1 <- data.table(post)[ , .(final.est = sum(weight.pred)), by = .(cname)]

reg2 <- glmer(leave ~ votecon + voteukip + female + age + highed + lowed +
                c_con15 + c_ukip15 + c_unemployed + c_whitebritish + c_deprived +
                (1|cname), family=binomial(link="logit"),nAGQ=0,data=e)
summary(reg2)

post$cname <- as.character(post$cname)
post$prediction <- predict(reg2,newdata=post,type="response",allow.new.levels=TRUE)
post$weight.pred <- post$prediction*post$percent*100
results2 <- data.table(post)[ , .(final.est = sum(weight.pred)), by = .(cname)]
median(results2$final.est)
mean(results2$final.est)
results2$final.est[results2$cname=="Epping Forest"]
results2$final.est[results2$cname=="Reading East"]