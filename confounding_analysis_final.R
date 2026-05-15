library(dplyr)
library(tidyr)
library(forcats)
library(epiR)
library(arrow)
library(haven)

#########################################################################
#########################################################################
########### STEP 0: LOAD DATA
#########################################################################
#########################################################################

df_1 <- read_parquet("C:/Users/HP/Documents/epi514/BRFSS_2019.parquet")
df_2 <- read_parquet("C:/Users/HP/Documents/epi514/BRFSS_2024.parquet")

#########################################################################
#########################################################################
########### STEP 1: SUBSET & COLLAPSE DATA
#########################################################################
#########################################################################



collapse_variables <- function(df) {
  df %>%
    mutate(
      # Collapse Age: 18-34, 35-54, 55+
      age_collapsed = fct_collapse(age_cat,
                                   "18-54" = c("18-24yo", "25-29 yo", "30-34 yo", "35-39 yo", "40-44 yo", "45-49 yo", "50-54 yo"),
                                   "55+"   = c("55-59 yo", "60-64 yo", "65-69 yo", "70-74 yo", "75-79 yo", "80+ yo")
      ),
      # Collapse Race: Grouping small Ns like AI/AN and NH/PI into 'Other'
      race_collapsed = fct_collapse(race_cat,
                                    "White" = "White only",
                                    "Black" = "Black only",
                                    "Hispanic" = "Hispanic",
                                    "Asian, AI/AN, NHPI, Other/Multiracial" = c("Asian only", "AI/AN only","Native Hawaiian or other Pacific Islander only","Multiracial", "Other race only")
      ),
      # Collapse income
      inc_collapsed = fct_collapse(inc_cat,
                                   "<$25,000" = c("≥$15,000 and <$25,000", "Less than $15,000"),
                                   "≥$25,000 - $50,000" = c("≥$25,000 and <$35,000", "≥$35,000 and <$50,000", "≥$50,000")
      ),
      # Collapse income
      educa_collapsed = fct_collapse(na_if(as.character(educa), "9"), 
                                     "High school or less" = c("1", "2", "3"),
                                     "Some college or college graduate" = c("4","5", "6")),
      # ENSURE FACTOR LEVELS: Exposed (Urban) and Outcome (Yes) must be first!
      exposure = factor(urbstat_cat, levels = c("Urban", "Rural")),
      outcome  = factor(heavyalc, levels = c("Yes", "No"))
    )
}

df_1_final <- collapse_variables(df_1)
df_2_final <- collapse_variables(df_2)

# 1. Conflict-proof Readiness Check Function
check_analysis_readiness <- function(df, year_label) {
  message(paste("\n--- Checking Readiness for", year_label, "---"))
  
  # Check missingness using explicit dplyr calls
  missing_summary <- df %>%
    dplyr::select(urbstat_cat, heavyalc, race_collapsed, inc_collapsed, educa_collapsed) %>%
    dplyr::summarise(dplyr::across(everything(), ~sum(is.na(.))))
  
  print("Missing counts per variable:")
  print(missing_summary)
  
  # Check for sparse strata (combinations with < 5 people)
  # This helps decide if we need to collapse Age or Race
  strata_check <- df %>%
    dplyr::group_by(race_collapsed, inc_collapsed, educa_collapsed) %>%
    dplyr::tally() %>%
    dplyr::filter(n < 5)
  
  message(paste("Total strata combinations with < 5 people:", nrow(strata_check)))
  
  if(nrow(strata_check) > 0) {
    print("Sample of very small strata:")
    print(head(strata_check))
  }
}

check_analysis_readiness(df_1_final, "2019")
check_analysis_readiness(df_2_final, "2024")


prepare_analysis_data <- function(df) {
  df %>%
    # 1. Ensure primary variables are factors with correct reference levels
    # (Doing this again here is a safe "double-check")
    mutate(
      exposure = factor(exposure, levels = c("Urban", "Rural")),
      outcome  = factor(outcome, levels = c("Yes", "No")) 
     ) %>%

    # 2. Listwise Deletion
    # We remove rows missing ANY variable that will be in your final model
     filter(
       !is.na(exposure),
       !is.na(outcome),
       #!is.na(sex_cat),
       !is.na(age_collapsed),
       !is.na(race_collapsed),
       !is.na(inc_collapsed),
       !is.na(educa_collapsed)
     )
}

df_1_final <- prepare_analysis_data(df_1_final)
df_2_final <- prepare_analysis_data(df_2_final)

# Subset to drinkers only
df_1_final <- df_1_final %>% filter(drnkany == "Yes")
df_2_final <- df_2_final %>% filter(drnkany == "Yes")

#########################################################################
#########################################################################
########### STEP 2: ASSESS FOR CONFOUNDING
#########################################################################
#########################################################################

#2019 and 2024 combined crude
df_3_final <- bind_rows(df_1_final, df_2_final)
crudeboth.2by2 <- with(df_3_final, table(exposure, outcome))
crudeboth.2by2.output <- epi.2by2(crudeboth.2by2, method = 'cross.sectional')
crudeboth.2by2.output

#2019 crude
crude19.2by2 <- with(df_1_final, table(exposure, outcome))
crude19.2by2.output <- epi.2by2(crude19.2by2, method = 'cross.sectional')
crude19.2by2.output

#2024 crude
crude24.2by2 <- with(df_2_final, table(exposure, outcome))
crude24.2by2.output <- epi.2by2(crude24.2by2, method = 'cross.sectional')
crude24.2by2.output

#2019 sex
sex19.2by2 <- with(df_1_final, table(exposure, outcome, sex_cat))
sex19.2by2.output <- epi.2by2(sex19.2by2, method = 'cross.sectional')
sex19.2by2.output

#2024 sex
sex24.2by2 <- with(df_2_final, table(exposure, outcome, sex_cat))
sex24.2by2.output <- epi.2by2(sex24.2by2, method = 'cross.sectional')
sex24.2by2.output

#2019 age
age19.2by2 <- with(df_1_final, table(exposure, outcome, age_collapsed))
age19.2by2.output <- epi.2by2(age19.2by2, method = 'cross.sectional')
age19.2by2.output

#2024 age
age24.2by2 <- with(df_2_final, table(exposure, outcome, age_collapsed))
age24.2by2.output <- epi.2by2(age24.2by2, method = 'cross.sectional')
age24.2by2.output

#2019 race
race19.2by2 <- with(df_1_final, table(exposure, outcome, race_collapsed))
race19.2by2.output <- epi.2by2(race19.2by2, method = 'cross.sectional')
race19.2by2.output

#2024 race
race24.2by2 <- with(df_2_final, table(exposure, outcome, race_collapsed))
race24.2by2.output <- epi.2by2(race24.2by2, method = 'cross.sectional')
race24.2by2.output

#2019 income
income19.2by2 <- with(df_1_final, table(exposure, outcome, inc_collapsed))
income19.2by2.output <- epi.2by2(income19.2by2, method = 'cross.sectional')
income19.2by2.output

#2024 income
income24.2by2 <- with(df_2_final, table(exposure, outcome, inc_collapsed))
income24.2by2.output <- epi.2by2(income24.2by2, method = 'cross.sectional')
income24.2by2.output

#2019 education
income19.2by2 <- with(df_1_final, table(exposure, outcome, educa_collapsed))
income19.2by2.output <- epi.2by2(income19.2by2, method = 'cross.sectional')
income19.2by2.output

#2024 education
income24.2by2 <- with(df_2_final, table(exposure, outcome, educa_collapsed))
income24.2by2.output <- epi.2by2(income24.2by2, method = 'cross.sectional')
income24.2by2.output

# New confounders: age, race, income, education (sex and age are not different MH vs crude PRR values)

#########################################################################
#########################################################################
########### STEP 3: RUN FULLY ADJUSTED MODEL
#########################################################################
#########################################################################

run_mh_analysis <- function(df, year_label) {
  # 1. Create the interaction
  df$strata <- interaction(df$age_collapsed, df$race_collapsed, df$inc_collapsed, df$educa_collapsed, drop = TRUE)
  
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


#########################################################################
#########################################################################
########### STEP 4: CREATE TABLE 2
#########################################################################
#########################################################################

# Combining tables together
library(gt)
library(dplyr)

# 1. Define the 2019 data
table2_2019 <- data.frame(
  Year = "2019",
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(2917, 17167),
  Total = c(22257, 142006),
  Prevalence = c("13.11%", "12.09%"),
  Crude_PR = c("1.00 (Ref)", "0.92 (0.89, 0.96)"),
  Adjusted_PR = c("1.00 (Ref)", "0.95 (0.91, 0.98)")
)

# 2. Define the 2024 data
table2_2024 <- data.frame(
  Year = "2024",
  Group = c("Rural (Reference)", "Urban"),
  Cases = c(2125, 10756),
  Total = c(15089, 88412),
  Prevalence = c("14.08%", "12.17%"),
  Crude_PR = c("1.00 (Ref)","0.86 (0.83, 0.90)"),
  Adjusted_PR = c("1.00 (Ref)","0.89 (0.85, 0.93)")
)

# 3. Combine and create the gt table
table2_combined <- bind_rows(table2_2019, table2_2024) %>%
  gt(groupname_col = "Year") %>%
  tab_header(
    title = "Table 2. Association Between Urbanicity and Heavy Drinking Adjusted by Age, Race/Ethnicity, Income and Education",
    subtitle = "Comparative Analysis of Prevalence Risk Ratios (N = 267764)"
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
    footnote = "Abbreviations: AI/AN = American Indian and Alaskan Native; NHPI = Native Hawaiian or other Pacific Islander. Adjusted for age, race/ethnicity, income and education using Mantel-Haenszel methods.",
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

#########################################################################
#########################################################################
########### STEP 5: EXPORT COLLAPSED FILES
#########################################################################
#########################################################################

write_parquet(df_1_final, "C:/Users/HP/Documents/epi514/BRFSS_2019_collapsed.parquet")
write_parquet(df_2_final, "C:/Users/HP/Documents/epi514/BRFSS_2024_collapsed.parquet")

