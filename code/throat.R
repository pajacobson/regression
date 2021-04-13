library(tidyverse)
library(ROCR)

throat <- read_csv("datasets/throat.csv", col_types=cols())
print(throat, n=35)

(pi.null <- mean( throat$SoreThroat ))
(throat.null <- log( pi.null / (1-pi.null) ))
throat.fit <- glm( SoreThroat ~ Duration, data=throat, family=binomial)
summary( throat.fit )
(boundary <- -throat.fit$coefficients[[1]] / throat.fit$coefficients[[2]])
pchisq( throat.fit$null.deviance - throat.fit$deviance, 1, lower.tail=FALSE)
throat <- throat %>% mutate( pi.hat = throat.fit$fitted.values )

confint.default( throat.fit )

ggplot( throat, aes(Duration, SoreThroat))+geom_point()+geom_vline(xintercept=boundary,linetype="dashed") + geom_smooth(method=glm,method.args=list(family=binomial))
# ggsave("images/throat_fit.png", device="png")

( throat.R2.M <- 1 - (throat.fit$deviance / throat.fit$null.deviance ) )
( throat.R2.S <- sum( (throat.fit$fitted.values - pi.null)^2 ) / sum( (throat$SoreThroat - pi.null)^2 ) )
throat %>% group_by(SoreThroat) %>% summarize( N = n(), pi.average = mean(pi.hat)) 

throat %>% group_by(Duration) %>% summarize(N=n(), Percentage=mean(SoreThroat))

( throat <- throat %>% mutate( v.hat = pi.hat*(1-pi.hat), r.i = SoreThroat - pi.hat) )

( S.throat <- sum( (throat$r.i)^2 ) )

X <- cbind(1,throat$Duration)
d <- 1 - 2*throat$pi.hat
V <- diag(throat$v.hat)
M <- V %*% X %*% solve( t(X) %*% V %*% X ) %*% t(X)
I <- diag( rep(1,length(d)) )
S.var <- as.numeric( t(d) %*% (I-M) %*% V %*% d )
( S.z <- ( S.throat - sum(throat$v.hat) ) / sqrt(S.var ) )
( S.pvalue <- 2*pnorm( -abs(S.z) ) )

# LMA <- throat %>% filter(Device==0)
# LMA.fit <- glm( SoreThroat ~ Duration, data=LMA, family=binomial)
# summary( LMA.fit )
# pchisq( LMA.fit$null.deviance - LMA.fit$deviance, 1, lower.tail=FALSE)

# TT <- throat %>% filter(Device==1)
# TT.fit <- glm( SoreThroat ~ Duration, data=TT, family=binomial)
# summary( TT.fit )
# pchisq( TT.fit$null.deviance - TT.fit$deviance, 1, lower.tail=FALSE)
