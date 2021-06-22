library(tidyverse)

# read CSV:
( concrete <- read_csv("datasets/concrete.csv", col_types=cols() ) )

concrete <- concrete %>% filter(Age == 28) %>% select(Cement, Water, Strength)

concrete <- concrete %>% mutate(Ratio = Water/Cement)
ggplot( concrete, aes(Ratio,Strength)) + 
  geom_point() + 
  geom_smooth(method="lm", level=0) 
#ggsave("../images/concrete_wc.png", device="png")
concrete.fit <- lm( Strength ~ Ratio, data = concrete)
summary(concrete.fit)
concrete <- concrete %>% mutate( Residuals = concrete.fit$residuals )
concrete.rse <- sigma( concrete.fit )
ggplot( concrete, aes(Ratio, Residuals)) + 
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed")
#ggsave("../images/concrete_wc_res.png",device="png")

concrete <- concrete %>% mutate(Ratio = Cement/Water)
ggplot( concrete, aes(Ratio,Strength)) + 
  geom_point() + 
  geom_smooth(method="lm", level=0) 
#ggsave("../images/concrete_cw.png", device="png")
cor( concrete$Ratio, concrete$Strength )
concrete.fit <- lm( Strength ~ Ratio, data = concrete)
summary(concrete.fit)
concrete <- concrete %>% mutate( Residuals = concrete.fit$residuals )
concrete.rse <- sigma( concrete.fit )
ggplot( concrete, aes(Ratio, Residuals)) + 
  geom_point() + 
  geom_hline(yintercept = concrete.rse*c(-2,0,2), linetype="dashed")
#ggsave("../images/concrete_cw_res.png",device="png")
ggplot(concrete, aes(Strength)) + geom_histogram( binwidth=7 ) 
#ggsave("../images/concrete_strength.png", device="png")
confint( concrete.fit )
