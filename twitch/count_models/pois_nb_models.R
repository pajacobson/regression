library(tidyverse)
library(MASS)
library(countreg)

crashes <- read_csv("TDOT.csv")
tn_county_pops <- read_csv("tn_county_pops.csv") %>%
  filter(CTYNAME != "Tennessee") %>%
  mutate(county = gsub(" County","",CTYNAME)) %>%
  dplyr::select(-CTYNAME) %>%
  dplyr::select(county, Pop2010 = CENSUS2010POP)

crashes_apr16 <- crashes %>% filter(Year=="2016") %>% dplyr::select(county=County, crashes=April)
crashes_apr16 <- left_join( crashes_apr16, tn_county_pops)

ggplot( crashes_apr16, aes(log(Pop2010),crashes)) + geom_point()

crashes_nb_fit <- glm.nb( crashes ~ log(Pop2010), data=crashes_apr16 )
( theta <- crashes_nb_fit$theta )
( alpha <- 1/theta )
summary( crashes_nb_fit )
rootogram( crashes_nb_fit, style="standing" )

births <- read_csv( "births.csv" )
county_pops <- read_csv("county_pops.csv")
births <- left_join( births, county_pops )
county_birth_stats <- births %>% group_by(county) %>% summarize(N=n(), Mean=mean(births), Var=var(births), Ratio = Mean/Var)

apr16 <- births %>% filter( year=="2016", month=="April")
ggplot( apr16, aes(log(Pop2010),births)) + geom_point()
ggplot( apr16, aes(log(Pop2010),births)) + 
  geom_point() + 
  geom_smooth(method="glm", method.args=list(family="poisson")) + ylim(0,6000)

pois_counties <- county_birth_stats %>% filter( abs(Ratio-1)<0.10 ) 
pois_births <- births %>% filter( county %in% pois_counties$county )

apr16_pois <- pois_births %>% filter( year == 2016, month == "April" )
ggplot( apr16_pois, aes(log(Pop2010),births)) + geom_point() + ylim(0,160)
ggplot( apr16_pois, aes(log(Pop2010),births)) + geom_point() + geom_smooth(method="glm", method.args=list(family="poisson")) + ylim(0,160)
apr16_pois_fit <- glm( births ~ log(Pop2010), data = apr16_pois, family="poisson")
summary(apr16_pois_fit)
rootogram( apr16_pois_fit, style="standing")

# using ALL of the data:
apr16_pois_fit <- glm( births ~ log(Pop2010), data = apr16, family="poisson")
summary(apr16_pois_fit)
rootogram( apr16_pois_fit, style="hanging", ylim=c(-1,3))

ggplot( apr16, aes(log(Pop2010),births)) + 
  geom_point() + 
  geom_smooth(method="glm.nb") + ylim(0,6000)

ggplot( apr16, aes(log(Pop2010),births)) + 
  geom_point() + 
  geom_smooth(method="glm", method.args=list(family="poisson"), color="blue") + 
  geom_smooth(method="glm.nb", color="purple") + ylim(0,6000)

apr16_nb_fit <- glm.nb( births ~ log(Pop2010), data=apr16 )
( theta <- apr16_nb_fit$theta )
( alpha <- 1/theta )
summary( apr16_nb_fit )
rootogram( apr16_nb_fit, style="hanging", ylim=c(-1,3))

