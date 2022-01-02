library(tidyverse)

# read the data :
( imr <- read_csv("../datasets/imr2011.csv", col_types=cols()) )
imr$Continent <- factor(imr$Continent)
imr <- imr %>% mutate( Continent = fct_reorder(Continent, -IMR) )
( imr.means <- imr %>% group_by(Continent) %>% summarize( N=n(), IMR=mean(IMR)) )

ggplot(imr, aes(Continent,IMR,color=Continent)) + 
  geom_boxplot() +
  geom_point() +
  coord_flip() +
  xlab(NULL) + ylab("Infant Mortality Rate (IMR)") +
  theme( legend.position="none")

# ggplot(imr, aes(Continent,IMR,color=Continent)) + 
#   geom_point() +
#   geom_point( data=imr.means, aes(x=Continent,y=IMR),color="black",size=3) +
#   coord_flip() +
#   xlab(NULL) + ylab("Infant Mortality Rate (IMR)") +
#   theme( legend.position="none")
#  geom_line( data=imr.means, aes(x=Continent,y=IMR), color="black", linetype="dashed", group=1 )

# ggsave("images/imr_by_continent.png", device="png")

imr.fit <- lm( IMR ~ Continent, data=imr)
summary(imr.fit)

new <- tibble(Continent=levels(imr$Continent))
predictions <- predict(imr.fit,new)
( imr.means <- imr.means %>% mutate( IMR.hat = predictions ) )

