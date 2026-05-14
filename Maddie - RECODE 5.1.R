rm(list=ls())
library(haven)
library(arrow)
library(dplyr)
library(tidyverse)
library(epiR)

#df_1 is 2019 data, df_2 is 2024 data. 
# Checking on sample size for power calculation
# 2019 data

df_1 <- read_parquet("C:/Users/HP/Documents/epi514/LLCP2019.parquet")
df_2 <- read_parquet("C:/Users/HP/Documents/epi514/LLCP2024.parquet")


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
df_1$urbstat_cat <- factor(df_1$urbstat_cat,levels=0:1,labels=c("Rural", "Urban"))
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
df_2$urbstat_cat <- factor(df_2$urbstat_cat,levels=0:1,labels=c("Rural", "Urban"))
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
write_parquet(df_1, "C:/Users/HP/Documents/epi514/BRFSS_2019.parquet")
write_parquet(df_2, "C:/Users/HP/Documents/epi514/BRFSS_2024.parquet")

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

# Bind together for full dataset
df <- bind_rows(df_1, df_2)
# combined years race 
race_all.2by2 <- with(df, table(heavyalc, urbstat_cat, race_cat))
race_all.2by2.output <- epi.2by2(race_all.2by2, method = 'cross.sectional')
race_all.2by2.output

#table 2 work -- 2019 , Age 
library(gt)
library(tidyverse)

#2019 with age 
age19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, age_cat))
age19.2by2.output <- epi.2by2(age19.2by2, method = 'cross.sectional')
age19.2by2.output 

table2_data <- data.frame(
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3350, 55222),  #using the age MH test outcome Rural (Exposure-), then Urban (Exposure+)
  Total = c(22764, 354562),# # Rural (Exposure-), then Urban (Exposure+)
  Prevalence = c("14.72%", "15.57%"),
  Crude_PR = c("1.00 (Ref)", "1.06 (1.02, 1.09)"),
  Adjusted_PR = c("1.00 (Ref)", "1.03 (0.99, 1.06)") # Using your M-H adjusted values
)
table2_data

#now making it into a table 
table2_output <- table2_data %>%
  gt() %>%
  # Add Main Heading and Subheading
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking adjusted by Age in 2019",
    subtitle = "Analysis of Prevalence Risk Ratios (n = 377,326)"
  ) %>%
  # Rename columns for clarity
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  # Alignment and Styling
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  # Add footnotes for the M-H adjustment
  tab_footnote(
    footnote = "Adjusted for stratification variables using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  # Add thick/thin lines for publication style
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

# Display the table
table2_output


#table 2 work -- 2024 , Age 

#2024 with age 
age24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, age_cat))
age24.2by2.output <- epi.2by2(age24.2by2, method = 'cross.sectional')
age24.2by2.output

table2_data <- data.frame(
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3311, 50268),  #using the age MH test outcome Rural (Exposure-), then Urban (Exposure+)
  Total = c(23254, 368716),# Rural (Exposure-), then Urban (Exposure+)
  Prevalence = c("14.24%", "13.63%"),
  Crude_PR = c("1.00 (Ref)", "0.96 (0.93, 0.99)"),
  Adjusted_PR = c("1.00 (Ref)", "0.93 (0.90, 0.96)") # Using your M-H adjusted values
)
table2_data

#now making it into a table 
table2_output <- table2_data %>%
  gt() %>%
  # Add Main Heading and Subheading
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking adjusted by Age in 2024",
    subtitle = "Analysis of Prevalence Risk Ratios (n = 391,970)"
  ) %>%
  # Rename columns for clarity
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  # Alignment and Styling
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  # Add footnotes for the M-H adjustment
  tab_footnote(
    footnote = "Adjusted for stratification variables using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  # Add thick/thin lines for publication style
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

# Display the table
table2_output

#table 2, Sex 2019 
#2019 with sex 
sex19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, sex_cat))
sex19.2by2.output <- epi.2by2(sex19.2by2, method = 'cross.sectional')
sex19.2by2.output

table2_data <- data.frame(
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3368, 55801),
  Total = c(22926, 359601),
  Prevalence = c("14.69%", "15.52%"),
  Crude_PR = c("1.00 (Ref)", "1.06 (1.02, 1.09)"),
  Adjusted_PR = c("1.00 (Ref)", "1.05 (1.02, 1.09)")
)

table2_output <- table2_data %>%
  gt() %>%
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Sex in 2019",
    subtitle = "Analysis of Prevalence Risk Ratios (n = 382,527)"
  ) %>%
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  tab_footnote(
    footnote = "Adjusted for sex using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

table2_output

#table 2, 2024 and Sex 
#2024 with sex 
sex24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, sex_cat))
sex24.2by2.output <- epi.2by2(sex24.2by2, method = 'cross.sectional')
sex24.2by2.output

table2_data <- data.frame(
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3338, 50920),
  Total = c(23459, 374350),
  Prevalence = c("14.23%", "13.60%"),
  Crude_PR = c("1.00 (Ref)", "0.96 (0.93, 0.99)"),
  Adjusted_PR = c("1.00 (Ref)", "0.95 (0.92, 0.99)")
)

table2_output <- table2_data %>%
  gt() %>%
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Sex in 2024",
    subtitle = "Analysis of Prevalence Risk Ratios (n = 397,809)"
  ) %>%
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  tab_footnote(
    footnote = "Adjusted for sex using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

table2_output

#table 2 race 2019 
#2019 with race
race19.2by2 <- with(df_1, table(heavyalc, urbstat_cat, race_cat))
race19.2by2.output <- epi.2by2(race19.2by2, method = 'cross.sectional')
race19.2by2.output

table2_data <- data.frame(
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3312, 54828),
  Total = c(22568, 352553),
  Prevalence = c("14.68%", "15.55%"),
  Crude_PR = c("1.00 (Ref)", "1.06 (1.03, 1.09)"),
  Adjusted_PR = c("1.00 (Ref)", "1.09 (1.06, 1.13)")
)

table2_output <- table2_data %>%
  gt() %>%
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Race/Ethnicity in 2019",
    subtitle = "Analysis of Prevalence Risk Ratios (n = 375,121)"
  ) %>%
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  tab_footnote(
    footnote = "Adjusted for race/ethnicity using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

table2_output

#table 2 race/eth in 2024 
#2024 with race
race24.2by2 <- with(df_2, table(heavyalc, urbstat_cat, race_cat))
race24.2by2.output <- epi.2by2(race24.2by2, method = 'cross.sectional')
race24.2by2.output

table2_data <- data.frame(
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3300, 50132),
  Total = c(23160, 367735),
  Prevalence = c("14.25%", "13.63%"),
  Crude_PR = c("1.00 (Ref)", "0.96 (0.93, 0.99)"),
  Adjusted_PR = c("1.00 (Ref)", "1.01 (0.98, 1.04)")
)

table2_output <- table2_data %>%
  gt() %>%
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Race/Ethnicity in 2024",
    subtitle = "Analysis of Prevalence Risk Ratios (n = 390,895)"
  ) %>%
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  tab_footnote(
    footnote = "Adjusted for race/ethnicity using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

table2_output

#########################
# Maya's addition

df_1 <- read_parquet("C:/Users/HP/Documents/epi514/BRFSS_2019.parquet")
df_2 <- read_parquet("C:/Users/HP/Documents/epi514/BRFSS_2024.parquet")

# Checking for n=0 in strata


library(dplyr)
library(tidyr)
library(forcats)

# 1. Subset to current drinkers only
df_1_drnk <- df_1 %>% filter(drnkany == "Yes")
df_2_drnk <- df_2 %>% filter(drnkany == "Yes")

# 2. Conflict-proof Readiness Check Function
check_analysis_readiness <- function(df, year_label) {
  message(paste("\n--- Checking Readiness for", year_label, "---"))
  
  # Check missingness using explicit dplyr calls
  missing_summary <- df %>%
    dplyr::select(urbstat_cat, heavyalc, sex_cat, age_cat, race_cat) %>%
    dplyr::summarise(dplyr::across(everything(), ~sum(is.na(.))))
  
  print("Missing counts per variable:")
  print(missing_summary)
  
  # Check for sparse strata (combinations with < 5 people)
  # This helps decide if we need to collapse Age or Race
  strata_check <- df %>%
    dplyr::group_by(age_cat, race_cat, sex_cat) %>%
    dplyr::tally() %>%
    dplyr::filter(n < 5)
  
  message(paste("Total strata combinations with < 5 people:", nrow(strata_check)))
  
  if(nrow(strata_check) > 0) {
    print("Sample of very small strata:")
    print(head(strata_check))
  }
}

check_analysis_readiness(df_1_drnk, "2019")
check_analysis_readiness(df_2_drnk, "2024")

collapse_variables <- function(df) {
  df %>%
    mutate(
      # Collapse Age: 18-34, 35-54, 55+
      age_collapsed = fct_collapse(age_cat,
                                   "18-54" = c("18-24yo", "25-29 yo", "30-34 yo"),
                                   "35-54" = c("35-39 yo", "40-44 yo", "45-49 yo", "50-54 yo"),
                                   "55+"   = c("55-59 yo", "60-64 yo", "65-69 yo", "70-74 yo", "75-79 yo", "80+ yo")
      ),
      # Collapse Race: Grouping small Ns like AI/AN and NH/PI into 'Other'
      race_collapsed = fct_collapse(race_cat,
                                    "White" = "White only",
                                    "Black" = "Black only",
                                    "Hispanic" = "Hispanic",
                                    "Asian" = "Asian only",
                                    "Other/Multiracial" = c("AI/AN only", "Multiracial", "Other race only", 
                                                            "Native Hawaiian or other Pacific Islander only")
      ),
      # ENSURE FACTOR LEVELS: Exposed (Urban) and Outcome (Yes) must be first!
      exposure = factor(urbstat_cat, levels = c("Urban", "Rural")),
      outcome  = factor(heavyalc, levels = c("Yes", "No"))
    )
}

df_1_final <- collapse_variables(df_1_drnk)
df_2_final <- collapse_variables(df_2_drnk)

library(dplyr)
library(forcats)
library(epiR)

prepare_analysis_data <- function(df) {
  df %>%
    # 1. Standardize and Relevel
    mutate(
      exposure = factor(urbstat_cat, levels = c("Urban", "Rural")),
      outcome  = factor(heavyalc, levels = c("Yes", "No")),
      
      # 2. Collapse Age (3 Groups)
      age_broad = fct_collapse(age_cat,
                               "18-54" = c("18-24yo", "25-29 yo", "30-34 yo","35-39 yo", "40-44 yo", "45-49 yo", "50-54 yo"),
                               "55+"   = c("55-59 yo", "60-64 yo", "65-69 yo", "70-74 yo", "75-79 yo", "80+ yo")
      ),
      
      # 3. Collapse Race (5 Groups)
      race_broad = fct_collapse(race_cat,
                                "White" = "White only",
                                "Black" = "Black only",
                                "Hispanic" = "Hispanic",
                                #"Asian" = "Asian only",
                                "Asian/Other/Multiracial" = c("Asian only","AI/AN only", "Multiracial", "Other race only", 
                                                        "Native Hawaiian or other Pacific Islander only")
      )
    ) %>%
    # 4. Remove rows with missing data in our variables of interest (Listwise Deletion)
    filter(!is.na(exposure), !is.na(outcome), !is.na(sex_cat), !is.na(age_broad), !is.na(race_broad))
}

df_1_final <- prepare_analysis_data(df_1_drnk)
df_2_final <- prepare_analysis_data(df_2_drnk)


run_mh_analysis <- function(df, year_label) {
  # 1. Create the interaction
  df$strata <- interaction(df$sex_cat, df$age_broad, df$race_broad, drop = TRUE)
  
  # 2. Build the 3D table
  tab_3d <- with(df, table(exposure, outcome, strata))
  
  # 3. STABILITY FILTER: A stratum is only kept if it has:
  #    - At least one Urban person with 'Yes'
  #    - At least one Rural person with 'Yes'
  #    - At least one person with 'No' (in either group)
  keep <- apply(tab_3d, 3, function(x) {
    urban_cases <- x["Urban", "Yes"] > 0
    rural_cases <- x["Rural", "Yes"] > 0
    any_no      <- sum(x[, "No"]) > 0
    return(urban_cases & rural_cases & any_no)
  })
  
  # 4. Filter the table
  tab_clean <- tab_3d[, , keep, drop = FALSE]
  
  # 5. Diagnostic messages
  message(paste("\n--- Mantel-Haenszel Analysis:", year_label, "---"))
  message(paste("Total possible strata combinations:", dim(tab_3d)[3]))
  message(paste("Stable strata used in analysis:", dim(tab_clean)[3]))
  message(paste("Strata excluded due to zero-case cells:", 
                dim(tab_3d)[3] - dim(tab_clean)[3]))
  
  # 6. Execution
  if (dim(tab_clean)[3] > 0) {
    # We use small = TRUE as an extra safety measure for sparse data
    results <- epi.2by2(tab_clean, method = "cross.sectional")
    print(results)
    return(results)
  } else {
    message("Warning: No strata remained with cases in both Urban and Rural groups.")
    message("This usually means heavy drinking is too rare in some sub-groups.")
    message("Switching to Poisson regression is recommended for these results.")
  }
}

# Run the analysis again
results_2019 <- run_mh_analysis(df_1_final, "2019")
results_2024 <- run_mh_analysis(df_2_final, "2024")




# Combining tables together
library(gt)
library(dplyr)

# 1. Define the 2019 data
table2_2019 <- data.frame(
  Year = "2019",
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3298, 19131),
  Total = c(25524, 162184),
  Prevalence = c("12.9%", "11.8%"),
  Crude_PR = c("1.00 (Ref)", "0.91 (0.88, 0.94)"),
  Adjusted_PR = c("1.00 (Ref)", "0.92 (0.88, 0.95)")
)

# 2. Define the 2024 data
table2_2024 <- data.frame(
  Year = "2024",
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(3276, 19705),
  Total = c(23892, 172571),
  Prevalence = c("13.7%", "11.4%"),
  Crude_PR = c("0.83 (0.80, 0.86)"),
  Adjusted_PR = c("0.84 (0.81, 0.87)")
)

# 3. Combine and create the gt table
table2_combined <- bind_rows(table2_2019, table2_2024) %>%
  gt(groupname_col = "Year") %>%
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Age, Sex and Race/Ethnicity",
    subtitle = "Comparative Analysis of Prevalence Risk Ratios (N = 384,171)"
  ) %>%
  cols_label(
    Group = "Exposure Status",
    Cases = "Cases (n)",
    Total = "Total (N)",
    Prevalence = "Prevalence",
    Crude_PR = "Crude PR (95% CI)",
    Adjusted_PR = "Adjusted PR (95% CI)*"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = Group) %>%
  tab_footnote(
    footnote = "Adjusted for age, sex, and race/ethnicity using Mantel-Haenszel methods.",
    locations = cells_column_labels(columns = Adjusted_PR)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    row_group.font.weight = "bold"
  )

table2_combined




# # Checking for 0 strata
# 
# library(dplyr)
# library(tidyr)
# 
# # 1. Create the diagnostic summary with explicit dplyr calls
# strata_diagnostics <- df_1_final %>%
#   dplyr::group_by(sex_cat, age_collapsed, race_collapsed) %>%
#   dplyr::summarise(
#     total_n = dplyr::n(),
#     # Count how many are Urban vs Rural
#     urban_n = sum(urbstat_cat == "Urban", na.rm = TRUE),
#     rural_n = sum(urbstat_cat == "Rural", na.rm = TRUE),
#     # Count how many heavy drinkers (Outcome+)
#     heavy_yes = sum(heavyalc == "Yes", na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   # Flag why a stratum would break epi.2by2
#   dplyr::mutate(
#     problem_type = dplyr::case_when(
#       urban_n == 0 & rural_n == 0 ~ "Empty Stratum",
#       urban_n == 0               ~ "Zero Urban",
#       rural_n == 0               ~ "Zero Rural",
#       heavy_yes == 0             ~ "Zero Heavy Drinkers",
#       TRUE                       ~ "Healthy"
#     )
#   )
# 
# # 2. View only the "Broken" strata
# broken_strata <- strata_diagnostics %>% 
#   dplyr::filter(problem_type != "Healthy") %>%
#   dplyr::arrange(problem_type, desc(total_n))
# 
# # 3. Print the results to see the culprits
# print(broken_strata)


# 
# # 2019
# # Need to collapse some categories
# library(dplyr)
# library(forcats)
# library(epiR)
# 
# # 1. Clean Recoding (Starting from original variables to avoid NA-traps)
# df_1 <- df_1 %>%
#   mutate(
#     # Exposure: Urban = 1 (Exposed), Rural = 0 (Reference)
#     # We use as.character first to make sure we are matching the labels correctly
#     exposure = factor(urbstat_cat, levels = c("Urban", "Rural")),
#     
#     # Outcome: Yes = 1 (Outcome+), No = 0 (Outcome-)
#     outcome = factor(heavyalc, levels = c("Yes", "No")),
#     
#     # Strata (using the collapsed versions we made earlier)
#     strata = interaction(race_broad, age_broad, sex_cat, drop = TRUE)
#   )
# 
# # 2. Build the Table
# race19.2by2_simple <- with(df_1, table(exposure, outcome, strata))
# 
# # 3. Filter out problematic strata (The "Keep" Logic)
# # We only want strata with at least one person AND at least one 'Yes' case
# keep_idx <- apply(race19.2by2_simple, 3, sum) > 0 & 
#   apply(race19.2by2_simple, 3, function(x) sum(x[,1])) > 0
# 
# # Check how many strata survived (this should be > 0!)
# cat("Number of strata surviving the filter:", sum(keep_idx), "\n")
# 
# if(sum(keep_idx) > 0) {
#   # 4. Filter the table WITHOUT dropping dimensions
#   race19.2by2_clean <- race19.2by2_simple[, , keep_idx, drop = FALSE]
#   
#   # 5. Run the analysis
#   race19.2by2_output <- epi.2by2(race19.2by2_clean, method = 'cross.sectional')
#   print(race19.2by2_output)
# } else {
#   message("Error: No strata contain any 'Yes' outcomes. Check your variable coding!")
# }
# 
# # # Create & populate table
# # table2_data <- data.frame(
# #   Group = c("Rural (Reference)", "Urban"),
# #   Cases = c(19131, 3298),
# #   Total = c(313233, 57641),
# #   Prevalence = c("6.11%", "5.72%"),
# #   Crude_PR = c("1.00 (Ref)", "1.00 (1.00, 1.01)"),
# #   Adjusted_PR = c("1.00 (Ref)", "1.00 (1.00, 1.01)")
# # )
# # 
# # table2_output <- table2_data %>%
# #   gt() %>%
# #   tab_header(
# #     title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Age, Sex and Race/Ethnicity in 2019",
# #     subtitle = "Analysis of Prevalence Risk Ratios (n = 370,874)"
# #   ) %>%
# #   cols_label(
# #     Group = "Exposure Status",
# #     Cases = "Cases (n)",
# #     Total = "Total (N)",
# #     Prevalence = "Prevalence",
# #     Crude_PR = "Crude PR (95% CI)",
# #     Adjusted_PR = "Adjusted PR (95% CI)*"
# #   ) %>%
# #   cols_align(align = "center", columns = everything()) %>%
# #   cols_align(align = "left", columns = Group) %>%
# #   tab_footnote(
# #     footnote = "Adjusted for race/ethnicity using Mantel-Haenszel methods.",
# #     locations = cells_column_labels(columns = Adjusted_PR)
# #   ) %>%
# #   opt_row_striping() %>%
# #   tab_options(
# #     table.border.top.color = "black",
# #     table.border.bottom.color = "black",
# #     heading.border.bottom.color = "black",
# #     column_labels.border.bottom.color = "black",
# #     column_labels.border.top.color = "black"
# #   )
# # 
# # table2_output
# 
# # 2024
# # Need to collapse some categories
# 
# library(dplyr)
# library(forcats)
# 
# df_2$urbstat_binary <- as.numeric(df_2$urbstat_cat) - 1
# 
# df_2 <- df_2 %>%
#   mutate(
#     # Collapse Age into 3 broad groups
#     age_broad = fct_collapse(age_cat,
#                              "18-34" = c("18-24yo", "25-29 yo", "30-34 yo"),
#                              "35-54" = c("35-39 yo", "40-44 yo", "45-49 yo", "50-54 yo"),
#                              "55+"   = c("55-59 yo", "60-64 yo", "65-69 yo", "70-74 yo", "75-79 yo", "80+ yo")
#     ),
#     # Collapse Race into fewer categories to avoid small cells for specific groups
#     race_broad = fct_collapse(race_cat,
#                               "White" = "White only",
#                               "Black" = "Black only",
#                               "Hispanic" = "Hispanic",
#                               "AI/AN only" = "AI/AN only",
#                               "Asian only" = "Asian only",
#                               "Native Hawaiian or other Pacific Islander only" = "Native Hawaiian or other Pacific Islander only",
#                               "Other/Multiracial" = c("Other race only", "Multiracial")
#     )
#   )
# 
# # Create the new interaction with fewer levels
# df_2$strata_simple <- interaction(df_2$race_broad, df_2$age_broad, df_2$sex_cat, drop = TRUE)
# 
# # Re-build the table (Exposure, Outcome, Strata)
# race19.2by2_simple <- with(df_2, table(urbstat_cat, heavyalc, strata_simple))
# 
# # Run the MH analysis
# race19.2by2_output <- epi.2by2(race19.2by2_simple, method = 'cross.sectional')
# print(race19.2by2_output)
# 
# # Create & populate table
# table2_data <- data.frame(
#   Group = c("Rural (Reference)", "Urban"),
#   Cases = c(19705, 3276),
#   Total = c(333160, 52846),
#   Prevalence = c("5.91%", "6.20%"),
#   Crude_PR = c("1.00 (Ref)", "1.00 (0.99, 1.00)"),
#   Adjusted_PR = c("1.00 (Ref)", "1.00 (1.00, 1.00)")
# )
# 
# table2_output <- table2_data %>%
#   gt() %>%
#   tab_header(
#     title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Age, Sex and Race/Ethnicity in 2024",
#     subtitle = "Analysis of Prevalence Risk Ratios (n = 386,006)"
#   ) %>%
#   cols_label(
#     Group = "Exposure Status",
#     Cases = "Cases (n)",
#     Total = "Total (N)",
#     Prevalence = "Prevalence",
#     Crude_PR = "Crude PR (95% CI)",
#     Adjusted_PR = "Adjusted PR (95% CI)*"
#   ) %>%
#   cols_align(align = "center", columns = everything()) %>%
#   cols_align(align = "left", columns = Group) %>%
#   tab_footnote(
#     footnote = "Adjusted for race/ethnicity using Mantel-Haenszel methods.",
#     locations = cells_column_labels(columns = Adjusted_PR)
#   ) %>%
#   opt_row_striping() %>%
#   tab_options(
#     table.border.top.color = "black",
#     table.border.bottom.color = "black",
#     heading.border.bottom.color = "black",
#     column_labels.border.bottom.color = "black",
#     column_labels.border.top.color = "black"
#   )
# 
# table2_output
# 
