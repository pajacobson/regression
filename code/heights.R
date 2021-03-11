library(tidyverse)
library(gghighlight)

# read the heights data :
( heights <- read_csv("datasets/heights.csv", col_types=cols() )  )
ggplot( stack(heights), aes(values)) + geom_histogram(binwidth=1) + facet_wrap(~ind) + xlab("Heights")
#ggsave("../images/height_histograms.png",device="png")

mean(heights$Father)
sd(heights$Father)

mean(heights$Son)
sd(heights$Son)

# add column for grouping by father's height, rounded to nearest inch:
heights <- heights %>% mutate( Group = round(Father))

# father.stats <- data.frame(Group = "Fathers", Average = round(mean( heights$Father ),2), SD = round(sd(heights$Father),2))
# son.stats <- data.frame(Group = "Sons", Average = round(mean( heights$Son ),2), SD = round(sd(heights$Son),2))
# overall.stats <- rbind( father.stats, son.stats )
# knitr::kable(overall.stats)
# write_csv( overall.stats, "../tables/overall_stats.csv")

# compute average height for each group :
son.stats <- heights %>% group_by(Group) %>% summarize("Number of sons" = n(), Average = round(mean(Son),2), SD = round(sd(Son),2))
knitr::kable( son.stats )
# write_csv(son.stats, "../tables/son_stats.csv")

(g <- ggplot(heights, aes(Father,Son)) + geom_point())
#ggsave("../images/heights.png",device="png")

(g64 <- g + geom_vline(xintercept=c(63.5,64.5),linetype="dashed") + gghighlight( Group == 64))
#ggsave("../images/fathers64.png",device="png")

g64 + geom_point(aes(x=64,y=son.stats$Average[ son.stats$Group == 64]), color="red", size=3)
#ggsave("../images/fathers64son.png",device="png")

(g70 <- g + geom_vline(xintercept=c(69.5,70.5),linetype="dashed") + gghighlight( Group == 70 ))
#ggsave("../images/fathers70.png",device="png")

g70 + geom_point(aes(x=70,y=son.stats$Average[ son.stats$Group == 70]), color="red", size=3)
#ggsave("../images/fathers70son.png",device="png")

son.groups <- heights %>% filter( Group == 64 | Group == 70) %>% select(Son,Group)
ggplot(son.groups, aes(Son)) + geom_histogram( binwidth=1 ) + facet_wrap(~Group)
#ggsave("../images/songroups.png",device="png")

g + geom_point(data = son.stats, aes(Group, Average), color="red", size=3)
#ggsave("../images/avgsons.png",device="png")

g + geom_smooth( method="lm", level=0 ) + geom_point(data = son.stats, aes(Group, Average), color="red", size=3) 
#ggsave("../images/avgsons_with_line.png",device="png")

g + geom_smooth( method="lm", level=0 )
#ggsave("../images/heights_with_line.png",device="png")

( heights.fit <- lm( Son ~ Father, data = heights ))
summary( heights.fit )

# sons whose fathers are 64 inches tall to the nearest inch:
sons.64 <- heights %>% filter(Group == 64) %>% select(Son)
summary( sons.64$Son  ) 
sd( sons.64$Son )
ggplot( sons.64, aes(Son)) + geom_histogram( binwidth=2 )
#ggsave("../images/sons64.png", device="png")

# sons whose fathers are 70 inches tall to the nearest inch:
sons.70 <- heights %>% filter(Group == 70) %>% select(Son)
summary( sons.70$Son  ) 
sd( sons.70$Son )
ggplot( sons.70, aes(Son)) + geom_histogram( binwidth=2 )
#ggsave("../images/sons70.png", device="png")

