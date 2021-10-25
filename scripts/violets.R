library(tidyverse)

( violets <- read_csv("datasets/violets.csv", col_types=cols()) )
violets <- violets %>% select(Year:Day,Photoperiod:`Bud counts`)
head(violets,10)

( chasm <- violets %>% 
  filter(`Bud type`=="Chasmogamous") %>%
  mutate( Bud = as.numeric(`Bud counts` > 0)) %>%
  select(Photoperiod, Bud) )

(g <- ggplot( chasm, aes(Photoperiod, Bud)) + geom_point())

chasm %>% summarize( N=n(), Successes=sum(Bud), Percentage=mean(Bud))

( pi.hat <- mean( chasm$Bud ) )
( chasm <- chasm %>% mutate(Null.probability = (pi.hat^Bud)*(1-pi.hat)^(1-Bud) ) )
( null.likelihood <- prod( chasm$Null.probability ) )
( null.deviance <- -2*log( null.likelihood ) )

( null.fit <- glm(Bud ~ 1, data = chasm, family=binomial) )

( chasm.fit <- glm(Bud ~ Photoperiod, data = chasm, family=binomial) )
( chasm <- chasm %>% mutate(Fit.probability = chasm.fit$fitted.values) )
g + geom_point(data=chasm,aes(Photoperiod,Fit.probability),color="red") + 
  geom_line(data=chasm,aes(Photoperiod,Fit.probability),color="red")

chasm <- chasm %>% mutate( Likelihood = (Fit.probability^Bud)*(1-Fit.probability)^(1-Bud))
( max.likelihood <- prod( chasm$Likelihood ) )
max.likelihood / null.likelihood
log( max.likelihood )
( residual.deviance <- -2*log(max.likelihood) ) 
( G2 <- -2*log( null.likelihood / max.likelihood ) )
2*log( max.likelihood / null.likelihood ) 
chasm.fit$null.deviance - chasm.fit$deviance
pchisq(G2, df=1, lower.tail=FALSE)

( R2.M <- 1 - (chasm.fit$deviance / chasm.fit$null.deviance) )
chasm <- chasm %>% 
  mutate( Null.residual = Bud - pi.hat ) %>% 
  mutate( Residual = Bud - Fit.probability ) %>% 
  mutate( Difference = Fit.probability - pi.hat )
( R2.S <- sum( (chasm$Difference)^2 ) / sum( (chasm$Null.residual)^2 ) )
1 - ( sum( chasm$Residual^2 ) / sum(chasm$Null.residual^2) )
