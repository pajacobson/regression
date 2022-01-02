library(tidyverse)
library(rfishbase)

if( file.exists("../datasets/fish.csv")){
  (species <- read_csv("../datasets/fish.csv", col_types=cols()))
} else{
  species <- species() 
  write_csv(species, "../datasets/fish.csv")
}

# dim(species)
# head(species,8)

length( which( is.na( species$Length)))
length( which( is.na( species$Weight)))

(train <- species %>% drop_na())
# dim(train)
# head(train,8)

ggplot(train, aes(Length,Weight)) + geom_point()
ggplot(train, aes(Length,Weight)) + geom_point() + geom_smooth()
#ggsave("../images/fish_lw.png", device="png")

ggplot(train, aes(log(Length),log(Weight))) + geom_point()
# ggplot(train, aes(log(Length),log(Weight))) + geom_point() + geom_smooth()
ggplot(train, aes(log(Length),log(Weight))) + geom_point() + geom_smooth(method="lm")
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
sharks$Weight <- case_when( 
  is.na(sharks$Weight) ~ exp(a)*(sharks$Length)^b, 
  !is.na(sharks$Weight) ~ sharks$Weight )
ggplot( sharks, aes( log(Length), log(Weight))) + geom_point()
#ggsave("../images/sharks_lw.png", device="png")

if( !file.exists("datasets/sharks.csv")){
  write_csv(sharks,"datasets/sharks.csv")
}
