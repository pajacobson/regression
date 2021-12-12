library(tidyverse)
library(MASS)
# library(countreg)

options(scipen=99)

# TN counties: crashes per month -----
######################################

crashes <- read_csv("TDOT.csv")

crash_stats <- crashes %>% 
  dplyr::select(-Year) %>%
  pivot_longer(!County, names_to="Month", values_to="Crashes") %>%
  group_by(County) %>% 
  drop_na(Crashes) %>%
  summarize( N=n(),
             Mean = mean(Crashes),
             Var = var(Crashes))

ggplot( crash_stats, aes(x=Mean, y=Var)) + 
  geom_point() + 
  labs( x = "Average number of crashes per month",
          y = "Variance of crashes per month") 

ggplot( crash_stats, aes(x=log(Mean), y=log(Var))) + 
  geom_point() + 
  labs( x = "log( Average number of crashes per month )",
        y = "log( Variance of crashes per month )") 

ggplot( crash_stats, aes(x=log(Mean), y=log(Var))) + 
  geom_point() + 
  labs( x = "log( Average number of crashes per month )",
        y = "log( Variance of crashes per month )") +
  geom_smooth( method = "lm", level=0 )

crash_lm <- lm( log(Var) ~ log(Mean), data = crash_stats )
summary(crash_lm)

ggplot( crash_stats, aes(x=Mean, y=sqrt(Var))) + 
  geom_point() + 
  labs( x = "Average number of crashes per month",
        y = "sqrt( Variance of crashes per month )") 

tn_county_pops <- read_csv("tn_county_pops.csv") %>%
  filter(CTYNAME != "Tennessee") %>%
  mutate(county = gsub(" County","",CTYNAME)) %>%
  dplyr::select(-CTYNAME) %>%
  dplyr::select(county, Pop2010 = CENSUS2010POP)

crashes_apr_2016 <- crashes %>% 
  filter(Year=="2016") %>% 
  dplyr::select(county=County, crashes=April)
crashes_apr_2016 <- left_join( crashes_apr_2016, tn_county_pops)

ggplot( crashes_apr_2016, aes(Pop2010,crashes)) + geom_point() # + geom_smooth(method="glm.nb",level=0)
ggplot( crashes_apr_2016, aes(log(Pop2010),crashes)) + geom_point() # + geom_smooth(method="glm.nb",level=0)

# explain why the following is NOT helpful!!! --------
ggplot( crashes_apr_2016, aes(log(Pop2010),log(crashes))) + geom_point() # + geom_smooth(method="glm.nb",level=0)

crashes_nb_fit <- glm.nb( crashes ~ log(Pop2010), data=crashes_apr_2016 )
( theta <- crashes_nb_fit$theta )
( alpha <- 1/theta )
summary( crashes_nb_fit )

ggplot( crash_stats, aes(x=Mean,y=Var)) + 
  geom_point() + 
  geom_smooth( level=0 ) +
  geom_function(fun = function(x) exp(-1.305)*x^1.570, color="darkgreen" ) +
  geom_function(fun = function(x) x + alpha*x^2, color="purple" ) 
  # scale_color_manual()

# compare after removing high-leverage counties ------
######################################################

crashes_apr_2016_small <- crashes_apr_2016 %>%
  filter( crashes < 800 )

# N.B.!!! -----
###############

ggplot( crashes_apr_2016_small, aes(Pop2010,crashes)) + geom_point() + geom_smooth(method="glm.nb",level=0)

# crashes_nb_fit_small <- glm.nb( crashes ~ Pop2010, data=crashes_apr_2016_small )
# ( theta <- crashes_nb_fit_small$theta )
# ( alpha <- 1/theta )
# summary( crashes_nb_fit_small )

# This is much better! -----
############################

ggplot( crashes_apr_2016_small, aes(log(Pop2010),crashes)) + geom_point() + geom_smooth(method="glm.nb",level=0)

crashes_nb_fit_small <- glm.nb( crashes ~ log(Pop2010), data=crashes_apr_2016_small )
( theta <- crashes_nb_fit_small$theta )
( alpha <- 1/theta )
summary( crashes_nb_fit_small )

ggplot( crash_stats %>% filter( Mean < 500) , aes(x=Mean,y=Var)) + 
  geom_point() + 
  geom_smooth( level=0 ) +
  geom_function(fun = function(x) x + alpha*x^2, color="purple" ) 

# is this really useful??
# rootogram( crashes_nb_fit_small, style="hanging" )

# US counties: births per month ------
######################################

births <- read_csv( "births.csv" )
county_pops <- read_csv("county_pops.csv")
births <- left_join( births, county_pops )
county_birth_stats <- births %>% 
  group_by(county) %>% 
  summarize(N=n(), Mean=mean(births), Var=var(births), Ratio = Mean/Var)

births_apr_2016 <- births %>% filter( year=="2016", month=="April")
ggplot( births_apr_2016, aes(log(Pop2010),births)) + geom_point()
ggplot( births_apr_2016, aes(log(Pop2010),births)) + 
  geom_point() + 
  geom_smooth(method="glm", method.args=list(family="poisson"), color="purple") + 
  geom_smooth(method="glm.nb", color="blue") + ylim(0,6000)

pois_counties <- county_birth_stats %>% filter( abs(Ratio-1) < 0.10 ) 
pois_births <- births %>% filter( county %in% pois_counties$county )

births_apr_2016_pois <- pois_births %>% filter( year == 2016, month == "April" )
ggplot( births_apr_2016_pois, aes(log(Pop2010),births)) + 
  geom_point() + 
  ylim(0,160)
ggplot( births_apr_2016_pois, aes(log(Pop2010),births)) + 
  geom_point() + 
  geom_smooth(method="glm", method.args=list(family="poisson")) + ylim(0,160)
births_apr_2016_pois_fit <- glm( births ~ log(Pop2010), data = births_apr_2016_pois, family="poisson")
summary(births_apr_2016_pois_fit)
# rootogram( births_apr_2016_pois_fit, style="standing")

# using ALL of the data:

ggplot( births_apr_2016, aes(log(Pop2010),births)) + 
  geom_point() + 
  geom_smooth(method="glm", method.args=list(family="poisson"), color="purple") + 
  geom_smooth(method="glm.nb", color="blue") + ylim(0,6000)

births_apr_2016_nb_fit <- glm.nb( births ~ log(Pop2010), data=births_apr_2016 )
( theta <- births_apr_2016_nb_fit$theta )
( alpha <- 1/theta )
summary( births_apr_2016_nb_fit )
# rootogram( births_apr_2016_nb_fit, style="hanging", ylim=c(-1,3))

