library(tidyverse)
library(gghighlight)

#sse <- function(b,m,x,y){ 
#  total <- 0 ;
#  for(i in 1:length(x)){
#    total <- total + ( y[i] - b - m*x[i] )^2 ; 
#  }
#  
#  return(total) ;
#}

# read the data :

africa <- read_csv("datasets/africa.csv")

( g <- ggplot( africa, aes(Literacy,Deaths)) + geom_point() )
#ggsave( "../images/literacy_deaths.png", device="png")

( x.bar <- mean(africa$Literacy) )
( y.bar <- mean(africa$Deaths) )

g + geom_vline(xintercept=x.bar,linetype="dashed",color="blue") + geom_hline(yintercept=y.bar, linetype="dashed",color="blue")
#ggsave("../images/quadrants.png",device="png")

# correlation:
(africa.r <- cor(africa$Literacy, africa$Deaths))

# null model:
g + geom_hline(yintercept=y.bar,color="blue",size=1)
#ggsave("../images/africa_nullmodel.png",device="png")

(africa.tss <- sum( (africa$Deaths - y.bar )^2 ))

var(africa$Deaths)
39*var(africa$Deaths)

# sqrt( var(africa$Deaths))
# sd( africa$Deaths ) 
 
# SD line:
(s.x <- sd(africa$Literacy))
(s.y <- sd(africa$Deaths))
(m <- -(s.y / s.x))
(b <- y.bar - (m*x.bar))
g + geom_abline(slope = m, intercept=b, color="deeppink",size=1)
#ggsave("../images/sdline.png",device="png")
 
# # show residuals for Namibia (row 32) and Somalia (row 39) :
# ( x1 <- c(africa[[32,2]],africa[[39,2]]) )
# ( y1 <- c(africa[[32,3]],africa[[39,3]]) )
# ( y2 <- b + m*x1 )
# g + geom_abline(slope = m, intercept=b, color="violet",size=1) + geom_segment(aes(x=x1[1],y=y1[1],xend=x1[1],yend=y2[1]),color="red") + geom_segment(aes(x=x1[2],y=y1[2],xend=x1[2],yend=y2[2]),color="red")
# #ggsave("../images/residuals.png",device="png")

# contour plot:
# m.seq <- seq(-1,-.6,length=500)
# b.seq <- seq(115, 125, length=250)
# mb.grid <- expand.grid(b.seq,m.seq)
# z <- rep(0,125000)
# for(i in 1:125000){ z[i] <- sse(mb.grid[i,1],mb.grid[i,2],africa$Literacy,africa$Deaths) }
# ss <- data.frame(intercept=mb.grid$Var1,slope=mb.grid$Var2,height=z)
# ggplot(ss,aes(x=intercept,y=slope,z=height))+geom_contour(bins=30)+geom_vline(xintercept = 120.2917, linetype="dashed")+geom_hline(yintercept = -.8476, linetype="dashed")
# #ggsave("../images/rss-contour.png",device="png")

(m <- africa.r*(s.y/s.x)) 
(b <- y.bar - m*x.bar)

g + geom_smooth(level=0, method="lm")
#ggsave( "../images/literacy_deaths_with_line.png", device="png")
 
g + geom_smooth(level=.95, method="lm")
#ggsave( "../images/literacy_deaths_with_ci.png", device="png")
# 
# g + geom_smooth(level=0, method="lm") + geom_abline(slope = m, intercept=b, color="violet",size=1)
# #ggsave("../images/compare_fits.png", device="png")
# 

( africa.fit <- lm( Deaths ~ Literacy, data = africa ) )
summary( africa.fit )
sigma( africa.fit )

( null.fit <- lm( Deaths ~ 1, data = africa ) )
summary( null.fit )
sigma( null.fit )

( africa.rss <- sum(africa.fit$residuals^2) )
africa.rse <- sqrt( africa.rss / africa.fit$df.residual )

africa <- africa %>% mutate(Fit = africa.fit$fitted.values, Residual = africa.fit$residuals)
# summary(africa.fit$residuals)
ggplot(africa, aes(Literacy,Residual))+geom_point()+geom_hline(yintercept=c(-2*africa.rse,0,2*africa.rse), linetype="dashed")
#ggsave("../images/literacy_deaths_residuals.png", device="png")

pred <- predict(africa.fit, interval="prediction")
# ggplot( cbind(africa,UN.predict),aes(Literacy,Deaths))+geom_point()+geom_smooth(method="lm")+geom_line(aes(y=lwr),color="red")+geom_line(aes(y=upr),color="red")
ggplot( cbind(africa,pred),aes(Literacy,Deaths))+geom_point()+geom_smooth(method="lm")+geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.2)
#ggsave("../images/literacy_deaths_ci_preds.png", device="png")

