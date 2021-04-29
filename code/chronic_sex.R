library(tidyverse)
library(ROCR)

# to recreate chronic.csv :
# namcs <- read_delim("datasets/namcs2009.tsv","\t", escape_double = FALSE, trim_ws = TRUE)
# chronic <- namcs %>% filter( NOCHRON < 2 ) %>% select(Age = AGE, Sex = SEX, Condition = NOCHRON)
# chronic$Condition <- 1 - chronic$Condition
# summary( chronic )
# chronic$Sex <- factor( chronic$Sex, labels=c("Female","Male"))
# write_csv( chronic, "datasets/chronic.csv" )

( chronic <- read_csv("datasets/chronic.csv", col_types=cols()) )
chronic$Sex <- factor( chronic$Sex ) 
( chronic.size <- dim( chronic)[1] )

str(chronic)

chronic %>% group_by( Sex ) %>% summarize( N=n(), Percentage=mean(Condition) )

sex.fit <- glm( Condition ~ Sex, data=chronic, family=binomial )
summary( sex.fit )
pchisq( sex.fit$null.deviance - sex.fit$deviance, 1, lower.tail=FALSE )
