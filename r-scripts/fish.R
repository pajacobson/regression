library(tidyverse)
library(rfishbase)

species <- species() 
dim(species)
write_csv(species, "fish.csv")

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
sharks <- read_csv("datasets/chondrichthyes.csv")
dim(sharks)
sharks <- inner_join(species,sharks,by="Species")
dim(sharks)
sharks <- sharks %>% filter(Category != "Data Deficient")
dim(sharks)
sharks <- sharks %>% drop_na(Length) 
dim(sharks)
# write_csv(sharks, "sharks_missing.csv")
sharks$Weight <- case_when( is.na(sharks$Weight) ~ exp(a)*(sharks$Length)^b, !is.na(sharks$Weight) ~ sharks$Weight )
ggplot( sharks, aes( log(Length), log(Weight))) + geom_point()
#ggsave("../images/sharks_lw.png", device="png")
# write_csv(sharks,"datasets/sharks.csv")
