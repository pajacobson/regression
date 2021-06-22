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

confint.default( throat.fit )

ggplot( throat, aes(Duration, SoreThroat)) + 
  geom_point()+geom_vline(xintercept=boundary,linetype="dashed") + 
  geom_smooth(method=glm,method.args=list(family=binomial))
# ggsave("images/throat_fit.png", device="png")

( throat.R2.M <- 1 - (throat.fit$deviance / throat.fit$null.deviance ) )

throat <- throat %>% 
  mutate( Pi.null = pi.null) %>%
  mutate( Null.res = SoreThroat - Pi.null) %>% 
  mutate( Pi.hat = throat.fit$fitted.values ) %>%
  mutate( Pi.diff = Pi.hat - Pi.null )

( throat.R2.S <- sum( (throat$Pi.diff)^2 ) / sum( (throat$Null.res)^2 ) )
throat %>% group_by(SoreThroat) %>% summarize( N = n(), Pi.average = mean(Pi.hat)) 

throat %>% group_by(Duration) %>% summarize(N=n(), Percentage=mean(SoreThroat))

throat <- throat %>% mutate( v.hat = Pi.hat*(1-Pi.hat), r.i = SoreThroat - Pi.hat )
throat %>% select(Age:SoreThroat,Pi.hat,v.hat,r.i)

( S.throat <- sum( (throat$r.i)^2 ) )

X <- cbind(1,throat$Duration)
d <- 1 - 2*throat$Pi.hat
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
