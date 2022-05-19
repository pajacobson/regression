library(tidyverse)

# read the heights data :
(heights <- read_csv("../datasets/heights.csv", col_types = cols()))
ggplot(gather(heights), aes(value)) +
  geom_histogram(binwidth = 1) +
  facet_wrap( ~ key) +
  xlab("Heights")
#ggsave("../images/height_histograms.png",device="png")

mean(heights$Father)
sd(heights$Father)

# this is the null model! -----
mean(heights$Son) 
sd(heights$Son)

ggplot(heights, aes(Father, Son)) + geom_point()

heights <- heights %>%
  mutate(Group = round(Father))

# these provide _point estimates_ for predicted mean responses -----
# and margins of error via SD
(son.means <- heights %>%
    group_by(Group) %>%
    summarize(Average = mean(Son),
              SD = sd(Son)))
# summarize(N = n(), Average = mean(Son), SD = sd(Son)))

(heights.plot <- ggplot(heights, aes(Father, Son)) +
    geom_point())
#ggsave("../images/heights.png",device="png")

ggplot() +
  geom_point(data = heights %>% filter(Group != 64),
             aes(Father, Son),
             alpha = 0.2) +
  geom_point(data = heights %>% filter(Group == 64), aes(Father, Son)) +
  theme(legend.position = "none")

# ggsave("../images/fathers64son.png",device="png")

ggplot() +
  geom_point(data = heights %>% filter(Group != 70),
             aes(Father, Son),
             alpha =
               .2) +
  geom_point(data = heights %>% filter(Group == 70), aes(Father, Son)) +
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
  geom_smooth( method="lm", level=0, lwd=1 ) + 
  geom_point(data = son.means, aes(Group, Average), color="red", size=3) 
#ggsave("../images/avgsons_with_line.png",device="png")

heights.plot + 
  geom_smooth( method="lm", level=0 )
#ggsave("../images/heights_with_line.png",device="png")

( heights.fit <- lm( Son ~ Father, data = heights ))
summary( heights.fit )
