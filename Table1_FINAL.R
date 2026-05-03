#table 1 
# load data 
df_1 <- read_parquet('C:/Users/mayaol/epi514/BRFSS_2019.parquet')
df_2 <- read_parquet('C:/Users/mayaol/epi514/BRFSS_2024.parquet')

# Calculate missingness of each variable
df_1 %>%
  summarise(across(
    c(age_cat, race_cat, inc_cat, urbstat_cat, heavyalc), 
    ~sum(is.na(.))
  ))

# age_cat - 6688 - 1.6%
# race_cat - 8935 - 2.1%
# inc_cat - 79781 - 19%
# urbstat_cat - 8458 - 2.0%
# heavyalc - 27699 - 6.6%

df_2 %>%
  summarise(across(
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
  summarise(across(
    c(age_cat, race_cat, inc_cat, urbstat_cat, heavyalc), 
    ~sum(is.na(.))
  ))

# age_cat - 14998 - 1.7%
# race_cat - 18038 - 2.1%
# inc_cat - 196898 - 22%
# urbstat_cat - 23081 - 2.6%
# heavyalc - 74397 - 8.5%


# 1. Update filtering to keep NAs for inc_cat and heavyalc
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

# 2. Combine and select columns
df_2019 <- df_1 %>%
  mutate(year = "2019") %>%
  select(year, sex_cat, urbstat_cat, heavyalc, drnkany, age_cat, race_cat, inc_cat)

df_2024 <- df_2 %>%
  mutate(year = "2024") %>%
  select(year, sex_cat, urbstat_cat, heavyalc, drnkany, age_cat, race_cat, inc_cat)

df_combined <- bind_rows(df_2019, df_2024)

df_combined_clean <- df_combined %>%
  mutate(across(
    c(inc_cat, heavyalc), 
    ~tidyr::replace_na(as.character(.), "Missing")
  ))

# 2. Build the table
table1_output <- df_combined_clean %>%
  select(
    urbstat_cat, 
    sex_cat, 
    heavyalc, 
    drnkany, 
    age_cat, 
    race_cat, 
    inc_cat
  ) %>%
  tbl_summary(
    by = urbstat_cat,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = all_categorical() ~ c(0, 1),
    label = list(
      heavyalc ~ "Heavy Alcohol Use",
      sex_cat ~ "Sex",
      drnkany ~ "Any Alcohol Use",
      age_cat ~ "Age Category",
      race_cat ~ "Race/Ethnicity",
      inc_cat ~ "Income Category"
    ),
    # Set missing to "no" because "Missing" is now a regular category
    missing = "no" 
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 1. BRFSS 2019 and 2024 Participant Characteristics by Urbanicity Status**") %>%
  modify_source_note(source_note = "Missingness of all variables assessed - variables with >5% missingness in overall data file have been reported in the table. Missing age - 14998 - 1.7%; missing race -  18038 - 2.1%; missing income - 196898 - 22%; missing urbanicity - 23081 - 2.6%; missing heavy drinking - 74397 - 8.5%")

# View output
table1_output