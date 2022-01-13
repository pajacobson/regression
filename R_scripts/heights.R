library(tidyverse)
# library(gghighlight)

# read the heights data :
( heights <- read_csv("../datasets/heights.csv", col_types=cols()) )
ggplot( gather(heights), aes(value)) + 
  geom_histogram(binwidth=1) + 
  facet_wrap(~key) + 
  xlab("Heights")
#ggsave("../images/height_histograms.png",device="png")

mean(heights$Father)
sd(heights$Father)

mean(heights$Son)
sd(heights$Son)

heights <- heights %>% mutate(Group = round(Father))
( son.means <- heights %>% 
    group_by(Group) %>% 
    summarize(Average = mean(Son)) )
# summarize(N = n(), Average = mean(Son), SD = sd(Son)))

(heights.plot <- ggplot(heights, aes(Father,Son)) + geom_point())
#ggsave("../images/heights.png",device="png")

ggplot(heights %>% filter(Group!=64), aes(Father,Son, alpha=0.75)) + 
  geom_point() + 
  geom_point(data=heights %>% filter(Group==64), aes(Father,Son, alpha=1.0) ) + 
  theme(legend.position = "none")
# ggsave("../images/fathers64son.png",device="png")

ggplot(heights %>% filter(Group!=70), aes(Father,Son, alpha=.75)) + 
  geom_point() + 
  geom_point(data=heights %>% filter(Group==70), aes(Father,Son, alpha=1.0) ) + 
  theme(legend.position = "none")
# ggsave("../images/fathers70son.png",device="png")

son.groups <- heights %>% 
  filter( Group == 64 | Group == 70) 
ggplot(son.groups, aes(Son)) + geom_histogram( binwidth=1 ) + facet_wrap(~Group)
#ggsave("../images/songroups.png",device="png")

heights.plot + 
  geom_point(data = son.means, aes(Group, Average), color="red", size=3)
#ggsave("../images/avgsons.png",device="png")

heights.plot + 
  geom_smooth( method="lm", level=0 ) + 
  geom_point(data = son.means, aes(Group, Average), color="red", size=3) 
#ggsave("../images/avgsons_with_line.png",device="png")

heights.plot + 
  geom_smooth( method="lm", level=0 )
#ggsave("../images/heights_with_line.png",device="png")

( heights.fit <- lm( Son ~ Father, data = heights ))
summary( heights.fit )

