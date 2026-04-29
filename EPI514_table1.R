rm(list = ls())
# load library 
library(haven)
library(arrow)
library(dplyr)
library(tidyverse)
library(epiR)
library(flextable)
# Checking on sample size for power calculation
# 2019 data
df_1 <- read_parquet("/Users/betitessema/epi514/LLCP2019.parquet")

# Filter to DRNKANY5 == 1 & all covars present
df_1 <- df_1 %>%
  filter(DRNKANY5 == 1) %>%
  filter(`_AGEG5YR` != 14) %>%
  filter(`_RACE` != 9) %>%
  filter(`_INCOMG` != 9) %>%
  filter(`_URBSTAT` != '.') %>%
  filter(`_RFDRHV7` != 9)

# Assign exposed & unexposed based on _URBSTAT 1 & 2
df_1 <- df_1 %>%
  mutate(exposed = if_else(`_URBSTAT` == 1, 1, 2))

# Heavy drinking status
df_1 <- df_1 %>%
  mutate(heavy_drink = if_else(`_RFDRHV7` == 2, 
                               1,                            2
  ))

#create table 1 and save as a word document 
tab <- table(df_1$DRNK3GE5, df_1$`_URBSTAT`, useNA = 'ifany')
prop.table(tab,margin = 2)
tab1 <- as.data.frame.matrix(prop.table(tab, 1))
tab1$Category <- rownames(tab1)
ft <- flextable(tab1)
save_as_docx(ft, path = 'my_table.docx')



# 2024 data
df_2 <- read_parquet("C:/Users/mayaol/epi514/LLCP2024.parquet")

# Filter to DRNKANY6 == 1 & all covars present
df_2 <- df_2 %>%
  filter(DRNKANY6 == 1) %>%
  filter(`_AGEG5YR` != 14) %>%
  filter(`_RACE` != 9) %>%
  filter(`_INCOMG1` != 9) %>%
  filter(`_URBSTAT` != '.') %>%
  filter(`_RFDRHV9` != 9)

df_2 <- df_2 %>%
  mutate(exposed = if_else(`_URBSTAT` == 1, 1, 2))

# Heavy drinking status
df_2 <- df_2 %>%
  mutate(heavy_drink = if_else(`_RFDRHV9` == 2, 
    1, 
    2
  ))

# Bind together for full dataset
df <- bind_rows(df_1, df_2)

exposed <- df %>%
  filter(exposed == 1) # 293322 exposed

unexposed <- df %>%
  filter(exposed == 2) # 43430 unexposed

# Need to calculate baseline proportion (p0) or total heavy drinking among everyone
table(df$heavy_drink)

# Heavy drink   Not heavy drink 
# 40768         295984 

# Parameters
p0_val <- 0.20          # [UPDATE] Prevalence in unexposed
pr_val <- 1.05          # [UPDATE] Prevalence Ratio
n_total <- 336752      
# Correct r: Exposed (336,428) / Unexposed (324)
r_val <- 40768 / (336752 - 40768) 

# In new epiR, set power = NA to calculate power
epi.ssxsectn(
  pdexp1 = p0_val * pr_val, 
  pdexp0 = p0_val,
  n = n_total,
  power = NA,              # This tells R: "I give you N, you give me power"
  r = r_val,
  design = 1,
  sided.test = 2,
  conf.level = 0.95
)

# Need to do sample size calculations next
# In new epiR, set power = NA to calculate power
epi.ssxsectn(
  pdexp1 = 0.15 * 1.09, 
  pdexp0 = 0.15,
  n = NA,
  power = 0.8,              # This tells R: "I give you N, you give me power"
  r = r_val,
  design = 1,
  sided.test = 2,
  conf.level = 0.95
)


# MDPR
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.15,
  n = n_total,
  power = 0.8,              # This tells R: "I give you N, you give me power"
  r = r_val,
  design = 1,
  sided.test = 2,
  conf.level = 0.95
)

####################################################
# Converting BRFSS files to parquet
library(haven)
library(arrow)
library(dplyr)
library(tidyverse)
library(epiR)

# 1. Read the SAS Transport file
# Haven is very efficient with .XPT files
df_xpt <- read_xpt("C:/Users/mayaol/epi514/LLCP2024.XPT")

# 2. Write to Parquet for your future self
# Parquet is still much faster for daily work than XPT
write_parquet(df_xpt, "C:/Users/mayaol/epi514/LLCP2024.parquet")

# 1. Read the SAS Transport file
# Haven is very efficient with .XPT files
df_xpt <- read_xpt("C:/Users/mayaol/epi514/LLCP2019.XPT")

# 2. Write to Parquet for your future self
# Parquet is still much faster for daily work than XPT
write_parquet(df_xpt, "C:/Users/mayaol/epi514/LLCP2019.parquet")