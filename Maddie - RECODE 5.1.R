rm(list=ls())
library(haven)
library(arrow)
library(dplyr)
library(tidyverse)
library(epiR)

#df_1 is 2019 data, df_2 is 2024 data. 
# Checking on sample size for power calculation
# 2019 data

df_1 <- read_parquet("/Users/madisonclay/epi514/BRFSS_2019.parquet")
df_2 <- read_parquet("/Users/madisonclay/epi514/BRFSS_2024.parquet")


# Bind together for full dataset
df <- bind_rows(df_1, df_2)

# RECODE SECTION -- Maddie on 4/30
#changing labels to lowercase 
names(df_1) <- tolower(names(df_1))
names(df_1)

names(df_2) <- tolower(names(df_2))
names(df_2)

## take away underscore
names(df_1) <- gsub("_", "", names(df_1))
names(df_1)

names(df_2) <- gsub("_", "", names(df_2))
names(df_2)

## Just laking a look at each variable here 
#2019 survey year - df_1 
names(df_1)
summary(df_1$sexvar) #dichotomous, no NAs, no blanks 
summary(df_1$urbstat) #dichotomous, no NAs, yes blanks 
summary(df_1$rfdrhv7) #dichotomous, no NAS
summary(df_1$drnkany5) # 4 levels
summary(df_1$ageg5yr) #levels 1-13,  
summary(df_1$race) #levels 1-9, no NA, yes blanks
summary(df_1$incomg) #6 levels, no NAs, yes blanks

#2024 survey year 
names(df_2)
summary(df_2$sexvar) #dichotomous, no NAs, no blanks
summary(df_2$urbstat) #dichotomous, no NAs, yes blanks 
summary(df_2$rfdrhv9) #dichotomous, no NAS
summary(df_2$drnkany6) # 4 levels, but filtered to just be yes 
summary(df_2$ageg5yr) #levels 1-13 
summary(df_2$race) #levels 1-9, no NAs
summary(df_2$incomg1) #6 levels, no NAs, yes blanks

############# RECODE 2019 DATA ################
#recode sex 2019 data -- df_1
df_1$sex_cat <- NA
df_1$sex_cat[df_1$sexvar == 1] <- 0 #reference group, male
df_1$sex_cat[df_1$sexvar == 2] <- 1 #female
df_1$sex_cat <- factor(df_1$sex_cat,levels=0:1,labels=c("Male", "Female"))
#check 
table(df_1$sexvar, df_1$sex_cat, useNA = "always")
table(df_1$sexvar, useNA = "always")

#recode urbstat 2019 data -- df_1
df_1$urbstat_cat <- NA
df_1$urbstat_cat[df_1$urbstat == 2] <- 0 #reference group, rural 
df_1$urbstat_cat[df_1$urbstat == 1] <- 1 #urban 
df_1$urbstat_cat <- factor(df_1$urbstat_cat,levels=0:1,labels=c("Urban", "Rural"))
#check 
table(df_1$urbstat, df_1$urbstat_cat, useNA = "always")
table(df_1$urbstat_cat, useNA = "always")

#recode rfdrhv7 2019 data -- df_1
df_1$heavyalc <- NA
df_1$heavyalc[df_1$rfdrhv7 == 1] <- 0 #reference group, no 
df_1$heavyalc[df_1$rfdrhv7 == 2] <- 1 #yes
df_1$heavyalc[df_1$rfdrhv7 == 9] <- NA #dont know, refused, missing 
df_1$heavyalc <- factor(df_1$heavyalc,levels=0:1,labels=c("No", "Yes"))
#check 
table(df_1$rfdrhv7, df_1$heavyalc, useNA = "always")
table(df_1$heavyalc, useNA = "always")

#recode drnkany5 2019 data -- df_1 
df_1$drnkany <- NA
df_1$drnkany[df_1$drnkany5 == 2] <- 0 #no, reference group 
df_1$drnkany[df_1$drnkany5 == 1] <- 1 #yes
df_1$drnkany[df_1$drnkany5 == 7 & df_1$drnkany5 == 9] <- NA #dont know, not sure, refused, missing  
df_1$drnkany <- factor(df_1$drnkany,levels=0:1,labels=c("No", "Yes"))
#check 
table(df_1$drnkany5, df_1$drnkany, useNA = "always")
table(df_1$heavyalc, useNA = "always")

#recode ageg5yr 2019 data -- df_1

df_1$age_cat <- NA
df_1$age_cat[df_1$ageg5yr == 1] <- 0 # 18-24yo 
df_1$age_cat[df_1$ageg5yr == 2] <- 1 # 25-29 yo
df_1$age_cat[df_1$ageg5yr == 3] <- 2 # 30-34 yo
df_1$age_cat[df_1$ageg5yr == 4] <- 3 # 35-39 yo
df_1$age_cat[df_1$ageg5yr == 5] <- 4 # 40-44 yo
df_1$age_cat[df_1$ageg5yr == 6] <- 5 # 45-49 yo
df_1$age_cat[df_1$ageg5yr == 7] <- 6 # 50-54 yo
df_1$age_cat[df_1$ageg5yr == 8] <- 7 # 55-59 yo
df_1$age_cat[df_1$ageg5yr == 9] <- 8 # 60-64 yo
df_1$age_cat[df_1$ageg5yr == 10] <- 9 # 65-69 yo
df_1$age_cat[df_1$ageg5yr == 11] <- 10 # 70-74 yo
df_1$age_cat[df_1$ageg5yr == 12] <- 11 # 75-79 yo
df_1$age_cat[df_1$ageg5yr == 13] <- 12 # 80+ yo
df_1$age_cat[df_1$ageg5yr == 14] <- NA

df_1$age_cat <- factor(df_1$age_cat,
                       levels=0:12,
                       labels=c("18-24yo", "25-29 yo", "30-34 yo", "35-39 yo","40-44 yo", "45-49 yo", "50-54 yo", "55-59 yo", "60-64 yo", "65-69 yo", "70-74 yo", "75-79 yo", "80+ yo"))
#Check 
table(df_1$ageg5yr, df_1$age_cat, useNA = "always")
table(df_1$age_cat, useNA = "always")

#recode race 2019 data -- df_1

df_1$race_cat <- NA
df_1$race_cat[df_1$race == 1] <- 0 # White only 
df_1$race_cat[df_1$race == 2] <- 1 # Black only
df_1$race_cat[df_1$race == 3] <- 2 # AI/AN only
df_1$race_cat[df_1$race == 4] <- 3 # Asian only
df_1$race_cat[df_1$race == 5] <- 4 # Native Hawaiian or other Pacific Islander only
df_1$race_cat[df_1$race == 6] <- 5 # Other race only
df_1$race_cat[df_1$race == 7] <- 6 # Multiracial 
df_1$race_cat[df_1$race == 8] <- 7 # Hispanic 
df_1$race_cat[df_1$race == 9] <- NA # Don't know, refused, not sure

df_1$race_cat <- factor(df_1$race_cat,
                        levels=0:7,
                        labels=c("White only", "Black only", "AI/AN only", "Asian only","Native Hawaiian or other Pacific Islander only", "Other race only", "Multiracial", "Hispanic"))
#Check 
table(df_1$race, df_1$race_cat, useNA = "always")
table(df_1$race_cat, useNA = "always")

#recode income 2019 data -- df_1

df_1$inc_cat <- NA
df_1$inc_cat[df_1$incomg == 1] <- 0 # Less than $15,000 
df_1$inc_cat[df_1$incomg == 2] <- 1 # ≥$15,000 and <$25,000
df_1$inc_cat[df_1$incomg == 3] <- 2 # ≥$25,000 and <$35,000
df_1$inc_cat[df_1$incomg == 4] <- 3 # ≥$35,000 and <$50,000
df_1$inc_cat[df_1$incomg == 5] <- 4 # ≥$50,000
df_1$inc_cat[df_1$incomg == 9] <- NA # Don't know, not sure, missing

df_1$inc_cat <- factor(df_1$inc_cat,
                       levels=0:4,
                       labels=c("Less than $15,000", "≥$15,000 and <$25,000", "≥$25,000 and <$35,000", "≥$35,000 and <$50,000","≥$50,000"))
#Check 
table(df_1$incomg, df_1$inc_cat, useNA = "always")
table(df_1$inc_cat, useNA = "always")

############### RECODE 2024 #####################

#recode sex 2024 data -- df_2
df_2$sex_cat <- NA
df_2$sex_cat[df_2$sexvar == 1] <- 0 #reference group, male
df_2$sex_cat[df_2$sexvar == 2] <- 1 #female
df_2$sex_cat <- factor(df_2$sex_cat,levels=0:1,labels=c("Male", "Female"))
#check 
table(df_2$sexvar, df_2$sex_cat, useNA = "always")
table(df_2$sexvar, useNA = "always")

#recode urbstat 2024 data -- df_2
df_2$urbstat_cat <- NA
df_2$urbstat_cat[df_2$urbstat == 2] <- 0 #reference group, rural 
df_2$urbstat_cat[df_2$urbstat == 1] <- 1 #urban 
df_2$urbstat_cat <- factor(df_2$urbstat_cat,levels=0:1,labels=c("Urban", "Rural"))
#check 
table(df_2$urbstat, df_2$urbstat_cat, useNA = "always")
table(df_2$urbstat_cat, useNA = "always")

#recode rfdrhv7 2024 data -- df_2
df_2$heavyalc <- NA
df_2$heavyalc[df_2$rfdrhv9 == 1] <- 0 #reference group, no 
df_2$heavyalc[df_2$rfdrhv9 == 2] <- 1 #yes
df_2$heavyalc[df_2$rfdrhv9 == 9] <- NA #dont know, refused, missing 
df_2$heavyalc <- factor(df_2$heavyalc,levels=0:1,labels=c("No", "Yes"))
#check 
table(df_2$rfdrhv9, df_2$heavyalc, useNA = "always")
table(df_2$heavyalc, useNA = "always")

#recode drnkany6 2024 data -- df_2 
df_2$drnkany <- NA
df_2$drnkany[df_2$drnkany6 == 2] <- 0 #no, reference group 
df_2$drnkany[df_2$drnkany6 == 1] <- 1 #yes
df_2$drnkany[df_2$drnkany6 == 7 & df_2$drnkany6 == 9] <- NA #dont know, not sure, refused, missing  
df_2$drnkany <- factor(df_2$drnkany,levels=0:1,labels=c("No", "Yes"))
#check 
table(df_2$drnkany6, df_2$drnkany, useNA = "always")
table(df_2$heavyalc, useNA = "always")

#recode ageg5yr 2024 data -- df_2
df_2$age_cat <- NA
df_2$age_cat[df_2$ageg5yr == 1] <- 0 # 18-24yo 
df_2$age_cat[df_2$ageg5yr == 2] <- 1 # 25-29 yo
df_2$age_cat[df_2$ageg5yr == 3] <- 2 # 30-34 yo
df_2$age_cat[df_2$ageg5yr == 4] <- 3 # 35-39 yo
df_2$age_cat[df_2$ageg5yr == 5] <- 4 # 40-44 yo
df_2$age_cat[df_2$ageg5yr == 6] <- 5 # 45-49 yo
df_2$age_cat[df_2$ageg5yr == 7] <- 6 # 50-54 yo
df_2$age_cat[df_2$ageg5yr == 8] <- 7 # 55-59 yo
df_2$age_cat[df_2$ageg5yr == 9] <- 8 # 60-64 yo
df_2$age_cat[df_2$ageg5yr == 10] <- 9 # 65-69 yo
df_2$age_cat[df_2$ageg5yr == 11] <- 10 # 70-74 yo
df_2$age_cat[df_2$ageg5yr == 12] <- 11 # 75-79 yo
df_2$age_cat[df_2$ageg5yr == 13] <- 12 # 80+ yo
df_2$age_cat[df_2$ageg5yr == 14] <- NA

df_2$age_cat <- factor(df_2$age_cat,
                       levels=0:12,
                       labels=c("18-24yo", "25-29 yo", "30-34 yo", "35-39 yo","40-44 yo", "45-49 yo", "50-54 yo", "55-59 yo", "60-64 yo", "65-69 yo", "70-74 yo", "75-79 yo", "80+ yo"))
#check
table(df_2$ageg5yr, df_2$age_cat, useNA = "always")
table(df_2$age_cat, useNA = "always")

#recode race 2024 data -- df_2

df_2$race_cat <- NA
df_2$race_cat[df_2$race == 1] <- 0 # White only 
df_2$race_cat[df_2$race == 2] <- 1 # Black only
df_2$race_cat[df_2$race == 3] <- 2 # AI/AN only
df_2$race_cat[df_2$race == 4] <- 3 # Asian only
df_2$race_cat[df_2$race == 5] <- 4 # Native Hawaiian or other Pacific Islander only
df_2$race_cat[df_2$race == 6] <- 5 # Other race only
df_2$race_cat[df_2$race == 7] <- 6 # Multiracial 
df_2$race_cat[df_2$race == 8] <- 7 # Hispanic 
df_2$race_cat[df_2$race == 9] <- NA # Don't know, refused, not sure

df_2$race_cat <- factor(df_2$race_cat,
                        levels=0:7,
                        labels=c("White only", "Black only", "AI/AN only", "Asian only","Native Hawaiian or other Pacific Islander only", "Other race only", "Multiracial", "Hispanic"))
#Check 
table(df_2$race, df_2$race_cat, useNA = "always")
table(df_2$race_cat, useNA = "always")

#recode income 2024 data -- df_2

df_2$inc_cat <- NA
df_2$inc_cat[df_2$incomg1 == 1] <- 0 # Less than $15,000 
df_2$inc_cat[df_2$incomg1 == 2] <- 1 # ≥$15,000 and <$25,000
df_2$inc_cat[df_2$incomg1 == 3] <- 2 # ≥$25,000 and <$35,000
df_2$inc_cat[df_2$incomg1 == 4] <- 3 # ≥$35,000 and <$50,000
df_2$inc_cat[df_2$incomg1 == 5] <- 4 # ≥50,000 and <$100,000
# HEAD
df_2$inc_cat[df_2$incomg1 == 6] <- 4 # ≥100,000 and <$200,000
df_2$inc_cat[df_2$incomg1 == 7] <- 4 # ≥$200,000
#
df_2$inc_cat[df_2$incomg1 == 6] <- 5 # ≥100,000 and <$200,000
df_2$inc_cat[df_2$incomg1 == 7] <- 6 # ≥$200,000
#ea105259af870d936b35ade204aa78d19cafc2b1
df_2$inc_cat[df_2$incomg1 == 9] <- NA # Don't know, not sure, missing

df_2$inc_cat <- factor(df_2$inc_cat,
                       levels=0:4,
                       labels=c("Less than $15,000", "≥$15,000 and <$25,000", "≥$25,000 and <$35,000", "≥$35,000 and <$50,000","≥$50,000"))
#Check 
table(df_2$incomg1, df_2$inc_cat, useNA = "always")
table(df_2$inc_cat, useNA = "always")

# Exporting
write_parquet(df_1, "C:/Users/mayaol/epi514/BRFSS_2019.parquet")
write_parquet(df_2, "C:/Users/mayaol/epi514/BRFSS_2024.parquet")

##### NEW, RECODED VARIABLES 

#2019 data# 
df_1$sex_cat
df_1$urbstat_cat
df_1$heavyalc
df_1$drnkany
df_1$age_cat
df_1$race_cat
df_1$inc_cat

#2024 data# 
df_2$sex_cat
df_2$urbstat_cat
df_2$heavyalc
df_2$drnkany
df_2$age_cat
df_2$race_cat
df_2$inc_cat

# table 2 -- 2x2 tables for alcohol x urbanicity and income / age 
#2019 data crude 
first.2by2 <- with(df_1, table(heavyalc, urbstat_cat))
first.2by2.output <- epi.2by2(first.2by2, method = 'cross.sectional')
first.2by2.output

#2024 data crude 
second.2by2 <- with(df_2, table(heavyalc, urbstat_cat))
second.2by2.output <- epi.2by2(second.2by2, method = 'cross.sectional')
second.2by2.output


#2019 with age 
age19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, age_cat))
age19.2by2.output <- epi.2by2(age19.2by2, method = 'cross.sectional')
age19.2by2.output

#2024 with age 
age24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, age_cat))
age24.2by2.output <- epi.2by2(age24.2by2, method = 'cross.sectional')
age24.2by2.output


#2019 with income
income19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, inc_cat))
income19.2by2.output <- epi.2by2(income19.2by2, method = 'cross.sectional')
income19.2by2.output

#2024 with income
income24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, inc_cat))
income24.2by2.output <- epi.2by2(income24.2by2, method = 'cross.sectional')
income24.2by2.output


#2019 with income and age 

complicated.19 <- xtabs(~ heavyalc + urbstat_cat + urbstat_cat + inc_cat,
                         data = df_1)
n_strata <- 2 * 2

complicated.array <- array(
  complicated.19,
  dim = c(2, 2, n_strata),
  dimnames = list(
    heavyalc = levels(df_1$heavyalc),
    urbanicity = levels(df_1$urbstat_cat),
    stratum        = seq_len(n_strata)
  )
)

complicated.output <- epi.2by2(dat = complicated.array, method = "cross.sectional")
complicated.output

#2024 with income and age 
complicated.24 <- xtabs(~ heavyalc + urbstat_cat + urbstat_cat + inc_cat,
                        data = df_2)
n_strata <- 2 * 2

complicated.array <- array(
  complicated.24,
  dim = c(2, 2, n_strata),
  dimnames = list(
    heavyalc = levels(df_2$heavyalc),
    urbanicity = levels(df_2$urbstat_cat),
    stratum        = seq_len(n_strata)
  )
)

complicated.output.24 <- epi.2by2(dat = complicated.array, method = "cross.sectional")
complicated.output.24

#2019 with sex 
sex19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, sex_cat))
sex19.2by2.output <- epi.2by2(sex19.2by2, method = 'cross.sectional')
sex19.2by2.output

#2024 with sex 
sex24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, sex_cat))
sex24.2by2.output <- epi.2by2(sex24.2by2, method = 'cross.sectional')
sex24.2by2.output

#2019 with race
race19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, race_cat))
race19.2by2.output <- epi.2by2(race19.2by2, method = 'cross.sectional')
race19.2by2.output

#2024 with race
race24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, race_cat))
race24.2by2.output <- epi.2by2(race24.2by2, method = 'cross.sectional')
race24.2by2.output
