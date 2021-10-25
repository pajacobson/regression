library(tidyverse)

enroll <- read_csv("datasets/enrollment.csv")
enroll <- enroll %>% mutate( Births = Births / 1000000, Enrollment = Enrollment / 1000000)
trend <- enroll %>% filter(EnrollmentYear > 1980 & EnrollmentYear < 2019)
ggplot(trend,aes(EnrollmentYear,Enrollment)) + 
  geom_line() + 
  geom_point() + 
  xlab("Year") + 
  ylab("Enrollment (millions)")
#ggsave("../images/annual_enrollments.png",device="png")

train <- enroll %>% filter( BirthYear < 1999)
ggplot(train, aes(Births,Enrollment)) + 
  geom_point() + 
  xlab("Births (millions)") + 
  ylab("Enrollment (millions)")
#ggsave("../images/enrollment_births.png",device="png")
 
train <- train %>% mutate(BirthDecade = BirthYear - BirthYear %% 10 )
train$BirthDecade <- factor(train$BirthDecade)
ggplot(train, aes(Births,Enrollment,color=BirthDecade)) + 
  geom_point() + 
  xlab("Births (millions)") + 
  ylab("Enrollment (millions)") + 
  labs(color="Birth decade")
#ggsave("../images/enrollment_births_decade.png",device="png")

train <- train %>% filter(BirthYear > 1978)
ggplot(train, aes(Births,Enrollment,color=BirthDecade)) + 
  geom_point() + 
  xlab("Births (millions)") + 
  ylab("Enrollment (millions)") + 
  labs(color="Birth decade")
#ggsave("../images/enrollment_births_80s.png",device="png")

cor(train$Births,train$Enrollment)
 
ggplot(train, aes(Births,Enrollment)) + 
  geom_point() + 
  geom_smooth(method="lm",se=F) + 
  xlab("Births (millions)") + 
  ylab("Enrollment (millions)")
#ggsave("../images/enrollment_births_lm.png",device="png")
 
eb.fit <- lm(Enrollment ~ Births, data = train )
summary(eb.fit)
confint(eb.fit)

new <- enroll %>% filter(BirthYear > 1998) %>% select(BirthYear, Births, EnrollmentYear)
predictions <- predict(eb.fit,new,interval="prediction")
predictions <- data.frame( new, Enrollment = predictions[,1], Lower = predictions[,2], Upper = predictions[,3])
(g <- ggplot(train, aes(Births,Enrollment)) + 
  geom_point() + 
  geom_smooth(method="lm",se=F) +
  geom_point(data=predictions,mapping=aes(Births,y=Enrollment),color="red",size=2) + 
  xlab("Births (millions)") + 
  ylab("Enrollment (millions)") )
#ggsave("../images/predicted_enrollments.png",device="png")

predictions %>% filter(Enrollment > 19)

ggplot(predictions,aes(EnrollmentYear,Enrollment))+geom_point()+geom_line()

predictions <- predictions %>% 
  select(BirthYear:Enrollment) %>% 
  mutate( Type = "Predicted") 
trend <- trend %>% mutate( Type = "Observed")
combined <- rbind(trend,predictions)
ggplot(combined, aes(EnrollmentYear,Enrollment,color=Type)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept=19,linetype="dashed") + 
  xlab("Year") + 
  ylab("Enrollment (millions)")
#ggsave("../images/combined_enrollments.png",device="png")
