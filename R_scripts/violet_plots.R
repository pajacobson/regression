library(tidyverse)

( violets <- read_csv("../datasets/violets.csv", col_types=cols() ) %>%
    select(Year:`Bud counts`) %>%
    mutate(Bud = as.numeric(`Bud counts`>0)) %>%
    select(-`Bud counts`) )

(violet.plots <- violets %>% group_by(Plot) %>% summarize(N=n(), Rate=mean(Bud)))

violetplot.fit <- glm( Bud ~ Plot, data=violets, family=binomial )
summary( violetplot.fit ) 
new <- tibble( Plot=violet.plots$Plot )
predictions <- predict( violetplot.fit, new, type="response")
( violet.plots <- violet.plots %>%
  mutate( Fit = predictions ) )
ggplot( violet.plots, aes(Plot,Rate)) +
  geom_point() + 
  geom_line() +
  geom_point( aes(Plot,Fit), color="red") + 
  geom_line( aes(Plot,Fit), color="red" ) +
  xlab("Plot number") +
  ylim(0,0.6)
# ggsave("images/plots_fit_bad.png",device="png")

str(violet.plots)
violets$Plot <- factor( violets$Plot )
(violet.plots <- violets %>% group_by(Plot) %>% summarize(N=n(), Rate=mean(Bud)))
str(violet.plots)
levels( violet.plots$Plot )
violetplot.fit <- glm( Bud ~ Plot, data=violets, family=binomial )
new <- tibble( Plot=violet.plots$Plot )
predictions <- predict( violetplot.fit, new, type="response" )
( violet.plots <- violet.plots %>%
    mutate( Fit = predictions ) )
ggplot( violet.plots, aes(as.numeric(Plot),Rate)) +
  geom_point() + 
  geom_line() +
  geom_point( aes(as.numeric(Plot),Fit), color="red") + 
  geom_line( aes(as.numeric(Plot),Fit), color="red" ) +
  xlab("Plot number") + 
  ylim(0,0.6)
# ggsave("images/plots_fit_good.png", device="png")

summary( violetplot.fit) 
