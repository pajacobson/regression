library(tidyverse)
library(rfishbase)

species <- species() 
dim(species)

species <- species %>% select(Species, Length, Weight)
head(species,8)

train <- species %>% drop_na()
dim(train)
head(train,8)

ggplot(train, aes(Length,Weight)) + geom_point()
#ggsave("../images/fish_lw.png", device="png")

ggplot(train, aes(log(Length),log(Weight))) + geom_point()
#ggsave("../images/fish_logs.png", device="png")

cor( log(train$Weight), log(train$Length)) 
fish.fit <- lm( log(Weight) ~ log(Length), data = train)
summary( fish.fit) 
a <- fish.fit$coefficients[[1]]
b <- fish.fit$coefficients[[2]]
sharks <- read_csv("../datasets/chondrichthyes.csv")
dim(sharks)
sharks <- inner_join(species,sharks,by="Species")
dim(sharks)
sharks <- sharks %>% filter(Category != "Data Deficient")
dim(sharks)
sharks <- sharks %>% drop_na(Length) 
dim(sharks)
sharks$Weight <- case_when( is.na(sharks$Weight) ~ exp(a)*(sharks$Length)^b, !is.na(sharks$Weight) ~ sharks$Weight )
ggplot( sharks, aes( log(Length), log(Weight))) + geom_point()
#ggsave("../images/sharks_lw.png", device="png")

threatened <- c("Critically Endangered","Endangered","Vulnerable")
nonthreatened <- c("Least Concern","Near Threatened")
sharks <- sharks %>% mutate( Threatened = case_when( Category %in% threatened ~ 1, Category %in% nonthreatened ~ 0 ))
head(sharks,8)
write_csv(sharks,"../datasets/sharks.csv")

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
