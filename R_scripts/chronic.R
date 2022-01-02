library(tidyverse)
library(ROCR)

# to recreate chronic.csv :
# namcs <- read_delim("datasets/namcs2009.tsv","\t", escape_double = FALSE, trim_ws = TRUE)
# chronic <- namcs %>% filter( NOCHRON < 2 ) %>% select(Age = AGE, Condition = NOCHRON)
# chronic$Condition <- 1 - chronic$Condition
# chronic$Sex <- chronic$Sex - 1
# summary( chronic )
# write_csv( chronic, "datasets/chronic.csv" )

( chronic <- read_csv("../datasets/chronic.csv", col_types=cols()) )
( chronic.size <- dim(chronic)[1] )

ggplot( chronic, aes(Age,Condition)) + geom_point()
# ggsave("../images/chronic_plot.png", device="png")

chronic %>% 
  group_by(Condition) %>% 
  summarize( Number = n(), Percentage = Number/chronic.size, .groups = "drop")

( cohorts <- chronic %>% 
    group_by(Age) %>% 
    summarize(Number = n(), Percentage = mean(Condition), .groups = "drop") )
ggplot( cohorts, aes(Age,Percentage) ) + geom_point( size=1 ) + ylim(0,1)
# ggsave("../images/chronic_age_cohorts.png", device="png")

# create tabular summary:
chronic <- chronic %>% mutate( Decade = 10*(round(Age/10) ))
( decades <- chronic %>% group_by(Decade) %>% 
    summarize( Number = n(), With.condition = sum(Condition), Percentage = mean(Condition), .groups="drop" ))

ggplot( decades, aes(Decade,Percentage) ) + 
  geom_point( color="red", size=2) + ylim(0,1) + xlab("Age")
# ggsave("../images/chronic_cohorts.png", device="png")

ggplot(chronic, aes(Age,Condition)) + 
  geom_smooth(method=glm,method.args=list(family=binomial),size=1) + 
  geom_point( data=cohorts, aes(Age,Percentage), size=1) + 
  geom_point( data=decades, aes(Decade,Percentage), color="red", size=2)
# ggsave("images/ages_decades.png", device="png")

z = seq(-5,5,len=1000)
logit <- data.frame(z = z, sigma = 1 / ( 1 + exp(-z) ) )
ggplot( logit, aes(z,sigma)) + geom_line( size=1 )
# ggsave("../images/logit.png", device="png")

chronic.fit <- glm( Condition ~ Age, data = chronic, family = "binomial" )
summary(chronic.fit)
chronic <- chronic %>% mutate( Probability = chronic.fit$fitted.values)

# new <- data.frame(Age=seq(-70,150,by=1))
# predictions <- predict(chronic.fit,new,type="response")
# extended.curve <- data.frame(Age = new$Age, Probability = predictions)
# g + xlim(-70,150) + geom_line( data=extended.curve, aes( x = Age, y = Probability), size=1 )
# ggsave("../images/chronic_logit.png",device="png")

( chronic <- chronic %>% mutate( Classifier = as.numeric(Age > 39.36) ) )
class.table <- table( chronic$Condition, chronic$Classifier )
rownames( class.table ) <- c("Does not have condition", "Has condition")
colnames( class.table ) <- c("Condition not predicted", "Condition predicted" )
class.table 
mean( chronic$Condition == chronic$Classifier )
mean( chronic$Condition != chronic$Classifier )

chronic <- chronic %>% mutate( Classifier = as.numeric(Probability > 0.80) )
class.table <- table( chronic$Condition, chronic$Classifier )
rownames( class.table ) <- c("Does not have condition", "Has condition")
colnames( class.table ) <- c("Condition not predicted", "Condition predicted" )
class.table
prop.table( class.table, 1 )
mean( chronic$Condition == chronic$Classifier )
mean( chronic$Condition != chronic$Classifier )

chronic <- chronic %>% mutate( Classifier = as.numeric(Probability > 0.20) )
class.table <- table( chronic$Condition, chronic$Classifier )
rownames( class.table ) <- c("Does not have condition", "Has condition")
colnames( class.table ) <- c("Condition not predicted", "Condition predicted" )
class.table 
prop.table( class.table, 1 )
mean( chronic$Condition == chronic$Classifier )
mean( chronic$Condition != chronic$Classifier )

pred <- prediction(chronic$Probability, chronic$Condition)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=TRUE )
points( c(.735,.377,.09), c(.975,.832,.349), pch=19)
auc <- performance( pred, "auc")
auc@y.values[[1]]

( cohorts <- chronic %>% 
    group_by(Age) %>% 
    summarize(N=n(), Successes=sum(Condition), Failures=N-Successes, Percentage=mean(Condition), .groups="drop"))
cohorts.null <- glm( cbind(Successes,Failures) ~ 1, data = cohorts, family=binomial)
summary(cohorts.null)
null.odds <- exp(cohorts.null$coefficients[[1]]) 
pi.null <- null.odds / (1+null.odds) 
cohorts <- cohorts %>% mutate( Pi.null = pi.null )
( cohorts <- cohorts %>% 
    mutate( Null.deviance = 2*(Successes*log(Percentage/Pi.null) + Failures*log((1-Percentage)/(1-Pi.null)) )))
sum( cohorts$Null.deviance )

cohorts.fit <- glm( cbind(Successes,Failures) ~ Age, data = cohorts, family=binomial)
summary(cohorts.fit)

new <- data.frame( Age = cohorts$Age )
predictions <- predict( cohorts.fit, new, type="response" )
cohorts <- cohorts %>% mutate( Pi.hat = predictions )
( cohorts <- cohorts %>% 
    mutate(Deviance = 2*( Successes*log(Percentage/Pi.hat) + Failures*log((1-Percentage)/(1-Pi.hat)) )) )
sum( cohorts$Deviance ) 
( G2.cohorts <- cohorts.fit$null.deviance - cohorts.fit$deviance )
pchisq( G2.cohorts, df=1, lower.tail = FALSE)

( G2.chronic <- chronic.fit$null.deviance - chronic.fit$deviance )
pchisq( G2.chronic, df=1, lower.tail = FALSE)

X.cohorts <- residuals(cohorts.fit, type="pearson")
(X2.cohorts <- sum( X.cohorts^2 ))
pchisq( X2.cohorts, cohorts.fit$df.residual, lower.tail=FALSE)

(cohorts <- cohorts %>% 
    mutate( Deviance = sqrt(Deviance)*sign(Percentage - Pi.hat)) %>% 
    mutate( Pearson = X.cohorts ))

( cohorts <- cohorts %>% 
    mutate( Hat = hatvalues( cohorts.fit) ) %>%
    mutate( Deviance.std = Deviance / sqrt(1-Hat)) %>% 
    mutate( Pearson.std = Pearson / sqrt(1-Hat) ) %>% 
    select( -Hat ) %>% 
    select( Age,Deviance:Pearson.std) %>% 
    gather(Type, Residual, Deviance:Pearson.std) )
ggplot(cohorts,aes(Age,Residual))+geom_point()+facet_wrap(~Type)
# ggsave("images/cohorts_residuals.png",device="png")
