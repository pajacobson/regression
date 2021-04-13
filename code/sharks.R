library(tidyverse)
library(ROCR)

( sharks <- read_csv("datasets/sharks.csv", col_types=cols() ) )

threatened <- c("Critically Endangered","Endangered","Vulnerable")
nonthreatened <- c("Least Concern","Near Threatened")
( sharks <- sharks %>% mutate( Threatened = case_when( Category %in% threatened ~ 1, Category %in% nonthreatened ~ 0 )) )

sharks.fit <- glm( Threatened ~ log(Weight), data = sharks, family = binomial )
summary( sharks.fit )
pchisq( sharks.fit$null.deviance - sharks.fit$deviance, 1, lower.tail=F)
ggplot( sharks, aes(log(Weight),Threatened)) + geom_point() + geom_smooth( method=glm, method.args = list(family = "binomial"))
#ggsave("../images/sharks_fit.png", device="png")

link <- family(sharks.fit)$link
linkinv <- family(sharks.fit)$linkinv
new <- data.frame( Weight = c(1800,270000))
( responses <- predict( sharks.fit, new, type="response") )
links <- predict( sharks.fit, new, type="link", se.fit=TRUE)
( predictions <- data.frame( Weight = new$Weight, logWeight = log(new$Weight), Link.lwr = links$fit - 1.96*links$se.fit, Link = links$fit, Link.upr = links$fit + 1.96*links$se.fit))
( predictions <- predictions %>% mutate( Response.lwr = linkinv(Link.lwr), Response = linkinv(Link), Response.upr = linkinv(Link.upr)) )

# vcov( sharks.fit )

( boundary <- -sharks.fit$coefficients[[1]] / sharks.fit$coefficients[[2]] )
( sharks <- sharks %>% mutate( Classifier = as.numeric( log(Weight) > boundary )) )
sharks.table <- table( sharks$Threatened, sharks$Classifier )
rownames( sharks.table ) <- c("Not threatened", "Threatened")
colnames( sharks.table ) <- c("Not predicted to be threatened", "Predicted to be threatened" )
sharks.table 
mean( sharks$Threatened == sharks$Classifier )
mean( sharks$Threatened != sharks$Classifier )

# investigate the following results further!! 

# sharks <- sharks %>% mutate(pi.hat = sharks.fit$fitted.values) 

# (sharks.null <- mean( sharks$Threatened ))
# ( sharks.R2.M <- 1 - (sharks.fit$deviance / sharks.fit$null.deviance ) )
# ( sharks.R2.S <- sum( (sharks.fit$fitted.values - sharks.null)^2 ) / sum( (sharks$Threatened - sharks.null)^2 ) )
# sharks %>% group_by(Threatened) %>% summarize( N = n(), pi.average = mean(pi.hat)) 

# sharks <- sharks %>% mutate(v.hat = pi.hat*(1-pi.hat), r.i = Threatened - pi.hat)
# sharks.S <- sum( (sharks$r.i)^2 )
# X <- cbind(1,log(sharks$Weight))
# d <- 1 - 2*sharks$pi.hat
# V <- diag(sharks$v.hat)
# M <- V %*% X %*% solve( t(X) %*% V %*% X ) %*% t(X)
# I <- diag( rep(1,length(d)) )
# sharks.var <- as.numeric( t(d) %*% (I-M) %*% V %*% d )
# ( sharks.z <- ( sharks.S - sum(sharks$v.hat) ) / sqrt(sharks.var ) )
# ( sharksfit.pvalue <- 2*pnorm( -abs(sharks.z) ) )
