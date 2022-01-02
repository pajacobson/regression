library(tidyverse)

# read the data :
africa <- read_csv("../datasets/africa.csv")
(n_countries <- dim(africa)[1])

( g <- ggplot( africa, aes(Literacy,IMR)) + geom_point() )
#ggsave( "../images/literacy_deaths.png", device="png")

( x_bar <- mean(africa$Literacy) )
( y_bar <- mean(africa$IMR) )

g + geom_vline(xintercept=x_bar,linetype="dashed",color="blue") + 
  geom_hline(yintercept=y_bar, linetype="dashed",color="blue")
#ggsave("../images/quadrants.png",device="png")

# correlation:
(africa_r <- cor(africa$Literacy, africa$IMR))

# null model:
g + geom_hline(yintercept=y_bar,color="blue",size=1)
#ggsave("../images/africa_nullmodel.png",device="png")

(africa_tss <- sum( (africa$IMR - y_bar )^2 ))

var(africa$IMR)
(n_countries - 1)*var(africa$IMR)
 
# SD line:
(s_x <- sd(africa$Literacy))
(s_y <- sd(africa$IMR))
(m <- -(s_y / s_x))
(b <- y_bar - (m*x_bar))
g + geom_abline(slope = m, intercept=b, color="deeppink",size=1)
#ggsave("../images/sdline.png",device="png")

# contour plot:
# m.seq <- seq(-1,-.6,length=500)
# b.seq <- seq(115, 125, length=250)
# mb.grid <- expand.grid(b.seq,m.seq)
# z <- rep(0,125000)
# for(i in 1:125000){ z[i] <- sse(mb.grid[i,1],mb.grid[i,2],africa$Literacy,africa$Deaths) }
# ss <- data.frame(intercept=mb.grid$Var1,slope=mb.grid$Var2,height=z)
# ggplot(ss,aes(x=intercept,y=slope,z=height))+
#  geom_contour(bins=30)+geom_vline(xintercept = 120.2917, linetype="dashed")+geom_hline(yintercept = -.8476, linetype="dashed")
# #ggsave("../images/rss-contour.png",device="png")

(m <- africa_r*( s_y / s_x ) ) 
(b <- y_bar - m*x_bar)

g + geom_smooth(level=0, method="lm")
#ggsave( "../images/literacy_deaths_with_line.png", device="png")
 
g + geom_smooth(level=.95, method="lm")
#ggsave( "../images/literacy_deaths_with_ci.png", device="png")
# 
# g + geom_smooth(level=0, method="lm") + geom_abline(slope = m, intercept=b, color="violet",size=1)
# #ggsave("../images/compare_fits.png", device="png")
# 

( africa_fit <- lm( IMR ~ Literacy, data = africa ) )
summary( africa_fit )
sigma( africa_fit )

( null_fit <- lm( IMR ~ 1, data = africa ) )
summary( null_fit )
sigma( null_fit )

( africa_rss <- sum(africa_fit$residuals^2) )
africa_rse <- sqrt( africa_rss / africa_fit$df.residual )

africa <- africa %>% 
  mutate(Fit = africa_fit$fitted.values) %>% 
  mutate(Residual = africa_fit$residuals)
# summary(africa.fit$residuals)
ggplot(africa, aes(Literacy,Residual)) + 
  geom_point()+geom_hline(yintercept=c(-2*africa_rse,0,2*africa_rse), linetype="dashed")
#ggsave("../images/literacy_deaths_residuals.png", device="png")

pred <- predict(africa_fit, interval="prediction")
ggplot( cbind(africa,pred),aes(Literacy,IMR)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.2)
#ggsave("../images/literacy_deaths_ci_preds.png", device="png")
