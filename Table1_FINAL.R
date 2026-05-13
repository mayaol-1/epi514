rm(list=ls())
library(haven)
library(arrow)
library(dplyr)
library(tidyverse)
library(epiR)
library(dplyr)
library(tidyr)
#install.packages("gtsummary")
library(gtsummary)
library(forcats)
library(gt)
#table 1 
# load data 
df_1 <- read_parquet('C:/Users/HP/Documents/epi514/BRFSS_2019.parquet')
df_2 <- read_parquet ('C:/Users/HP/Documents/epi514/BRFSS_2024.parquet')

head(df_1)
head(df_2)
# remove $ and yo, abbreviate NHPI
df_1 <- df_1 %>%
  mutate(
    inc_cat = str_remove_all(inc_cat, fixed("$")),
    age_cat = str_squish(str_remove_all(age_cat, fixed("yo"))),
    
    # Remove "only" from race categories
    race_cat = str_remove(race_cat, " only$"),
    
    # Rename NHPI category
    race_cat = str_replace(
      race_cat,
      "Native Hawaiian or other Pacific Islander",
      "NHPI"
    ),
    
    # Move categories to the end
    race_cat = fct_relevel(
      race_cat,
      "White",
      "Multiracial",
      "Other race",
      after = Inf
    )
  )

df_2 <- df_2 %>%
  mutate(
    inc_cat = str_remove_all(inc_cat, fixed("$")),
    age_cat = str_squish(str_remove_all(age_cat, fixed("yo"))),
    
    race_cat = str_remove(race_cat, " only$"),
    
    race_cat = str_replace(
      race_cat,
      "Native Hawaiian or other Pacific Islander",
      "NHPI"
    ),
    
    race_cat = fct_relevel(
      race_cat,
      "White",
      "Multiracial",
      "Other race",
      after = Inf
    ) )

# Calculate missingness of each variable
df_1 %>%
  dplyr::summarise(across(
    c(age_cat, race_cat, inc_cat, urbstat_cat, heavyalc), 
    ~sum(is.na(.))
  ))

# age_cat - 6688 - 1.6%
# race_cat - 8935 - 2.1%
# inc_cat - 79781 - 19%
# urbstat_cat - 8458 - 2.0%
# heavyalc - 27699 - 6.6%

df_2 %>%
  dplyr::summarise(across(
    c(age_cat, race_cat, inc_cat, urbstat_cat, heavyalc), 
    ~sum(is.na(.))
  ))

# age_cat - 8310
# race_cat - 9103
# inc_cat - 117117
# urbstat_cat - 14623
# heavyalc - 46698

missing <- bind_rows(df_1, df_2)

missing %>%
  dplyr::summarise(across(
    c(age_cat, race_cat, inc_cat, urbstat_cat, heavyalc), 
    ~sum(is.na(.))
  ))

# age_cat - 14998 - 1.7%
# race_cat - 18038 - 2.1%
# inc_cat - 196898 - 22%
# urbstat_cat - 23081 - 2.6%
# heavyalc - 74397 - 8.5%


# Update filtering to keep NAs for inc_cat and heavyalc
df_1 <- df_1 %>%
  filter(drnkany == "Yes") %>%
  filter(!is.na(age_cat)) %>%
  filter(!is.na(race_cat)) %>%
  filter(!is.na(urbstat_cat)) 
# Removed: filter(!is.na(inc_cat)) 
# Removed: filter(!is.na(heavyalc))

df_2 <- df_2 %>%
  filter(drnkany == "Yes") %>%
  filter(!is.na(age_cat)) %>%
  filter(!is.na(race_cat)) %>%
  filter(!is.na(urbstat_cat))
# Removed: filter(!is.na(inc_cat)) 
# Removed: filter(!is.na(heavyalc))

# Combine and select columns
df_2019 <- df_1 %>%
  mutate(year = "2019") %>%
  select(year, sex_cat, urbstat_cat, heavyalc, drnkany, age_cat, race_cat, inc_cat)

df_2024 <- df_2 %>%
  mutate(year = "2024") %>%
  select(year, sex_cat, urbstat_cat, heavyalc, drnkany, age_cat, race_cat, inc_cat)

# 1. Combine and select columns (Keep NAs as NAs)
df_combined <- bind_rows(df_2019, df_2024)

# 2. Reorganize income category 
# (fct_relevel works fine with NAs; it just ignores them)
df_table1 <- df_combined %>%
  mutate(
    inc_cat = fct_relevel(
      inc_cat,
      "Less than 15,000",
      after = 0
    )
  )

# 3. Generate the table
table1_output <- df_table1 %>%
  select(
    urbstat_cat, 
    sex_cat, 
    age_cat, 
    race_cat, 
    inc_cat
  ) %>%
  tbl_summary(
    by = urbstat_cat,
    # This ensures NAs are shown but excluded from % math
    missing = "ifany", 
    missing_text = "Missing", 
    label = list(
      sex_cat  ~ "Sex",
      age_cat  ~ "Age Category",
      race_cat ~ "Race/Ethnicity",
      inc_cat  ~ "Income Category"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = all_categorical() ~ c(0, 1)
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Urban Status**") %>%
  modify_caption(
    "**Table 1. BRFSS 2019 and 2024 Participant Characteristics by Urbanicity Status**"
  ) %>%
  as_gt() %>%
  # Note: Ensure library(gt) is loaded or use gt::tab_source_note
  tab_source_note(
    source_note = "Abbreviations: AI/AN = American Indian and Alaskan Native; NHPI = Native Hawaiian or other Pacific Islander."
  ) %>%
  tab_source_note(
    source_note = "Missingness of all variables assessed - variables with >5% missingness in overall data file have been reported in the table. Missing age - 1.7%; missing race - 2.1%; missing income - 22%; missing urbanicity - 2.6%; missing heavy drinking - 8.5%"
  )

table1_output

# save as PDF
#load webshot2
gtsave(table1_output, "table1.pdf")