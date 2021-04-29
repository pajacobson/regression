library(tidyverse)
library(ROCR)

# read CSV:
( violets <- read_csv("datasets/violets.csv", col_types=cols()) %>% 
    select(Year:Day,Photoperiod:`Bud counts`))

( chasm <- violets %>% 
    filter(`Bud type`=="Chasmogamous") %>% 
    mutate(Bud = as.numeric((`Bud counts` > 0)) ) %>% 
    select(Photoperiod,Bud) )
( g <- ggplot(chasm, aes(Photoperiod, Bud)) + geom_point() )
# #ggsave("../images/chasm.png", device="png")

chasm %>% summarize(Total=n(), With.buds = sum(Bud), Percentage = mean(Bud))

( pi.hat <- mean( chasm$Bud ) )
( chasm <- chasm %>% mutate(Null.probability = (pi.hat^Bud)*(1-pi.hat)^(1-Bud) ) )
( null.likelihood <- prod( chasm$Null.probability ) )
( null.deviance <- -2*log( null.likelihood ) )

( null.fit <- glm(Bud ~ 1, data = chasm, family=binomial) )
# summary( null.fit )

( chasm.fit <- glm(Bud ~ Photoperiod, data = chasm, family=binomial) )
summary( chasm.fit )
link <- family(chasm.fit)$link
linkinv <- family(chasm.fit)$linkinv

new <- tibble( Photoperiod = c(13,13.5,14))
responses <- predict( chasm.fit, new, type="response" )
links <- predict( chasm.fit, new, type="link", se.fit=TRUE )
(predictions <- tibble( Photoperiod = new$Photoperiod ) %>%
    mutate( Response = responses ) %>% 
    mutate( Link = links$fit ) %>% 
    mutate( Link.SE = links$se.fit ))
( predictions <- predictions %>% 
    mutate( Link.lwr = Link - 1.96*Link.SE ) %>% 
    mutate( Link.upr = Link + 1.96*Link.SE ) )
( predictions <- predictions %>% 
    mutate( Response.lwr = linkinv(Link.lwr) ) %>% 
    mutate( Response.upr = linkinv(Link.upr) ) ) 
ggplot(chasm,aes(Photoperiod,Bud))+geom_point()+geom_smooth(method="glm",method.args = list(family = "binomial"))
#ggsave("../images/chasm_fit_ci.png",device="png")

vcov(chasm.fit)

( chasm <- chasm %>% mutate(Fit.probability = chasm.fit$fitted.values) )
g + geom_point(data=chasm,aes(Photoperiod,Fit.probability),color="red") + 
  geom_line(data=chasm,aes(Photoperiod,Fit.probability),color="red")
# #ggsave("../images/chasm_fit.png",device="png")

( chasm <- chasm %>% mutate(Classifier = as.numeric(Photoperiod < 13.5 ) ) )
classifier.table <- table( chasm$Bud, chasm$Classifier )
colnames(classifier.table)=c("Bud not predicted","Bud predicted")
rownames(classifier.table)=c("Bud not observed","Bud observed")
classifier.table
mean( chasm$Bud != chasm$Classifier)
mean( chasm$Bud == chasm$Classifier)

chasm <- chasm %>% mutate(Classifier = as.numeric(Fit.probability > .90 ) )
classifier.table <- table( chasm$Bud, chasm$Classifier ) 
colnames(classifier.table)=c("Bud not predicted","Bud predicted")
rownames(classifier.table)=c("Bud not observed","Bud observed")
classifier.table
mean( chasm$Bud != chasm$Classifier)

chasm <- chasm %>% mutate(Classifier = as.numeric(Fit.probability > .05 ) )
classifier.table <- table( chasm$Bud, chasm$Classifier )
colnames(classifier.table)=c("Bud not predicted","Bud predicted")
rownames(classifier.table)=c("Bud not observed","Bud observed")
classifier.table
mean( chasm$Bud != chasm$Classifier)

pred <- prediction(chasm$Fit.probability, chasm$Bud)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=TRUE )
auc <- performance( pred, "auc")
auc@y.values[[1]]

( counts <- chasm %>% group_by(Photoperiod) %>% summarize(N = n(), Observed = sum(Bud), Expected = mean(Fit.probability)*n(), .groups="drop"  ) )
# round( as.data.frame(counts), 3 )

chasm <- chasm %>% 
  mutate( Likelihood = (Fit.probability^Bud)*(1-Fit.probability)^(1-Bud))
( max.likelihood <- prod( chasm$Likelihood ) )
max.likelihood / null.likelihood
log( max.likelihood )
( residual.deviance <- -2*log(max.likelihood) ) 

( G2 <- -2*log(null.likelihood / max.likelihood) )
2*log(max.likelihood / null.likelihood) 
chasm.fit$null.deviance - chasm.fit$deviance
pchisq(G2, df=1, lower.tail=FALSE)

( R2.M <- 1 - (chasm.fit$deviance / chasm.fit$null.deviance) )

chasm <- chasm %>% 
  mutate( Null.residual = Bud - pi.hat ) %>% 
  mutate( Residual = Bud - Fit.probability ) %>% 
  mutate( Difference = Fit.probability - pi.hat )
( R2.S <- sum( (chasm$Difference)^2 ) / sum( (chasm$Null.residual)^2 ) )
1 - ( sum( chasm$Residual^2 ) / sum(chasm$Null.residual^2) )

ggplot(chasm,aes(Fit.probability)) + geom_histogram(bins=6) + facet_wrap(~Bud, ncol=1)
# #ggsave("../images/chasm_rd_hist.png", device="png")
chasm %>% 
  group_by(Bud) %>% 
  summarize(N=n(), Fit.average = mean(Fit.probability), .groups="drop" )
( R2.D <- .807 - .139 )

