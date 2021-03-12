library(tidyverse)
library(ROCR)

( sharks <- read_csv("datasets/sharks.csv", col_types=cols() ) )

threatened <- c("Critically Endangered","Endangered","Vulnerable")
nonthreatened <- c("Least Concern","Near Threatened")
( sharks <- sharks %>% mutate( Threatened = case_when( Category %in% threatened ~ 1, Category %in% nonthreatened ~ 0 )) )

sharks.fit <- glm( Threatened ~ log(Weight), data = sharks, family = binomial )
summary( sharks.fit )
ggplot( sharks, aes(log(Weight),Threatened)) + geom_point() + geom_smooth( method=glm, method.args = list(family = "binomial"))
#ggsave("../images/sharks_fit.png", device="png")

link <- family(sharks.fit)$link
linkinv <- family(sharks.fit)$linkinv
new <- data.frame( Weight = c(1800,270000))
( responses <- predict( sharks.fit, new, type="response") )
links <- predict( sharks.fit, new, type="link", se.fit=TRUE)
( predictions <- data.frame( Weight = new$Weight, logWeight = log(new$Weight), Link.lwr = links$fit - 1.96*links$se.fit, Link = links$fit, Link.upr = links$fit + 1.96*links$se.fit))
( predictions <- predictions %>% mutate( Response.lwr = linkinv(Link.lwr), Response = linkinv(Link), Response.upr = linkinv(Link.upr)) )

vcov( sharks.fit )
