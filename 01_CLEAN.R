# 01_CLEAN.R
# BRFSS 2023-2025: Variable creation on multi-year dataset

# Produces one clean, fully-labeled dataset (ALL ages, ALL
# respondents, missingness kept as its own category) that both
# 02_table1.R and 03_multiple_imputation.R read from.

# Load packages
library(haven)
library(dplyr)

# 1. Import BRFSS data
brfss_master <- readRDS("data/processed/brfss_master.rds")

# 2. Select relevant variables
brfss_clean <- brfss_master %>%
  mutate(
    
    # year 
    year = factor(year),
    
    # SURVEY WEIGHT ADJUSTMENT for multi-year pooling
    # divide weight by number of years, so pooled estimates represent one year of pop'n
    # USE THIS not raw _LLCPWT in svydesign() calls
    weight_adj = `_LLCPWT` / n_distinct(year),
    
    # CORE ANALYSIS VARIABLES
    # Employment Status
    employment = factor(
      case_when(
        EMPLOY1 == 1 ~ "Wage Worker",
        EMPLOY1 == 2 ~ "Self-Employed",
        EMPLOY1 %in% c(3, 4) ~ "Unemployed",
        EMPLOY1 %in% c(5, 6, 7, 8) ~ "Not in Labor Force",
        EMPLOY1 == 9 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("Wage Worker", "Self-Employed", "Unemployed",
                 "Not in Labor Force", "Missing")
    ),
    
    # PRIMARY EXPOSURE #1: employment x income group ---
    # Used for Table 1A. Missing income is kept as its own level
    # for wage workers and self-employed; 
    # EMPLOY1 == 9 (missing/ refused employment) is excluded (NA).
    employ_income = factor(
      case_when(
        EMPLOY1 == 1 & `_INCOMG1` %in% c(5, 6, 7) ~ "High Income WW",
        EMPLOY1 == 1 & `_INCOMG1` %in% c(1, 2, 3, 4) ~ "Low Income WW",
        EMPLOY1 == 1 & `_INCOMG1` == 9 ~ "WW Missing Income",
        EMPLOY1 == 2 & `_INCOMG1` %in% c(5, 6, 7) ~ "High Income SE",
        EMPLOY1 == 2 & `_INCOMG1` %in% c(1, 2, 3, 4) ~ "Low Income SE",
        EMPLOY1 == 2 & `_INCOMG1` == 9 ~ "SE Missing Income",
        EMPLOY1 %in% c(3, 4) ~ "Unemployed",
        EMPLOY1 %in% c(5, 6, 7, 8) ~ "Not in Labor Force",
        TRUE ~ NA_character_ 
      ),
      levels = c("High Income WW", "Low Income WW", "WW Missing Income",
                 "High Income SE", "Low Income SE", "SE Missing Income",
                 "Unemployed", "Not in Labor Force")
    ),
    
    # PRIMARY EXPOSURE #2:
    employ_educ = factor(
      case_when(
        EMPLOY1 == 1 & `_EDUCAG` %in% c(3, 4) ~ "High Educ WW",
        EMPLOY1 == 1 & `_EDUCAG` %in% c(1, 2) ~ "Low Educ WW",
        EMPLOY1 == 1 & `_EDUCAG` == 9 ~ "WW Missing Educ",
        EMPLOY1 == 2 & `_EDUCAG` %in% c(3, 4) ~ "High Educ SE",
        EMPLOY1 == 2 & `_EDUCAG` %in% c(1, 2) ~ "Low Educ SE",
        EMPLOY1 == 2 & `_EDUCAG` == 9 ~ "SE Missing Educ",
        EMPLOY1 %in% c(3, 4) ~ "Unemployed",
        EMPLOY1 %in% c(5, 6, 7, 8) ~ "Not in Labor Force",
        TRUE ~ NA_character_
      ),
      levels = c("High Educ WW", "Low Educ WW", "WW Missing Educ",
                 "High Educ SE", "Low Educ SE", "SE Missing Educ",
                 "Unemployed", "Not in Labor Force")
    ),
    
    # Household income (dichotomized, missing kept)
    # Keep missing as a category before imputation.
    income_group = factor(
      case_when(
        `_INCOMG1` %in% c(1, 2, 3, 4) ~ "Low (<$50k)",
        `_INCOMG1` %in% c(5, 6, 7) ~ "High ($50k+)",
        TRUE ~ "Missing"
      ),
      levels = c("Low (<$50k)", "High ($50k+)", "Missing")
    ),
    
    # Education (missing kept)
    educ_group = factor(
      case_when(
        `_EDUCAG` %in% c(1, 2) ~ "HS or Less",
        `_EDUCAG` == 3 ~ "Some College",
        `_EDUCAG` == 4 ~ "College+",
        `_EDUCAG` == 9 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("HS or Less", "Some College", "College+", "Missing")
    ),
    
    # Children in household
    children = factor(
      case_when(
        CHILDREN == 88 ~ "No Children",
        CHILDREN >= 1 & CHILDREN <= 87 ~ "Children Present",
        CHILDREN == 99 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("No Children", "Children Present", "Missing")
    ),
    
    # All age groups
    # Use _AGE80 because it is based on CDC-imputed age (_IMPAGE).
    # _AGEG5YR is based on reported AGE and can therefore be missing.
    age_group = factor(
      case_when(
        `_AGE80` >= 18 & `_AGE80` <= 29 ~ "18-29",
        `_AGE80` >= 30 & `_AGE80` <= 44 ~ "30-44",
        `_AGE80` >= 45 & `_AGE80` <= 64 ~ "45-64",
        `_AGE80` >= 65 ~ "65+",
        TRUE ~ NA_character_
      ),
      levels = c("18-29", "30-44", "45-64", "65+")
    ),
    
    # WORKING AGE AGE GROUPS
    # Also based on CDC-imputed age through _AGE80.
    age_strata = factor(
      case_when(
        `_AGE80` >= 18 & `_AGE80` <= 34 ~ "18-34",
        `_AGE80` >= 35 & `_AGE80` <= 49 ~ "35-49",
        `_AGE80` >= 50 & `_AGE80` <= 64 ~ "50-64",
        TRUE ~ NA_character_
      ),
      levels = c("18-34", "35-49", "50-64")
    ),
    
    # Sex (imputed by CDC)
    sex = factor(
      case_when(
        SEXVAR == 1 ~ "Male",
        SEXVAR == 2 ~ "Female",
        TRUE ~ NA_character_
      ),
      levels = c("Male", "Female")
    ),
    
    # Race/ethnicity (imputed by CDC)
    race = factor(
      case_when(
        `_IMPRACE` == 1 ~ "NH White",
        `_IMPRACE` == 2 ~ "NH Black",
        `_IMPRACE` == 3 ~ "NH Asian",
        `_IMPRACE` == 4 ~ "NH AI/AN",
        `_IMPRACE` == 5 ~ "Hispanic",
        `_IMPRACE` == 6 ~ "Other/Multiracial",
        TRUE ~ NA_character_
      ),
      levels = c("NH White", "NH Black", "NH Asian", "NH AI/AN",
                 "Hispanic", "Other/Multiracial")
    ),
    
    # Marital status
    marital = factor(
      case_when(
        MARITAL %in% c(1, 6) ~ "Married/Partnered",
        MARITAL %in% c(2, 3, 4, 5) ~ "Not Married/Partnered",
        TRUE ~ "Missing"
      ),
      levels = c("Married/Partnered", "Not Married/Partnered", "Missing")
    ),
    
    # General Health
    health_label = factor(
      case_when(
        GENHLTH %in% c(1, 2, 3) ~ "Good/Very Good/Excellent",
        GENHLTH %in% c(4, 5)    ~ "Fair/Poor",
        TRUE ~ "Missing"
      ),
      levels = c("Good/Very Good/Excellent", "Fair/Poor", "Missing")
    ),
    
    # Mental health
    mental_cat = factor(
      case_when(
        `_MENT14D` == 1 ~ "0 days",
        `_MENT14D` == 2 ~ "1-13 days",
        `_MENT14D` == 3 ~ "14+ days",
        TRUE ~ "Missing"
      ),
      levels = c( "0 days", "1-13 days", "14+ days", "Missing")
    ),
    
    # Physical activity
    exercise = factor(
      case_when(
        `_TOTINDA` == 1 ~ "Yes",
        `_TOTINDA` == 2 ~ "No",
        `_TOTINDA` == 9 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("Yes", "No", "Missing")
    ),

    # Heavy drinking
    alcohol = factor(
      case_when(
        `_RFDRHV9` == 1 ~ "No",
        `_RFDRHV9` == 2 ~ "Yes",
        `_RFDRHV9` == 9 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("No", "Yes", "Missing")
    ),
    
    # Smoking
    smoking = factor(
      case_when(
        `_SMOKER3` %in% c(1, 2) ~ "Current Smoker",
        `_SMOKER3` == 3 ~ "Former Smoker",
        `_SMOKER3` == 4 ~ "Never Smoked",
        `_SMOKER3` == 9 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("Current Smoker", "Former Smoker", "Never Smoked", "Missing")
    ),
    
    # PREDICTORY ONLY (AUXILARY) VARIABLES - not imputed, more info for imputation
    # Census region, derived from _STATE FIPS code
    region = factor(
      case_when(
        `_STATE` %in% c(9, 23, 25, 33, 44, 50, 34, 36, 42) ~ "Northeast",
        `_STATE` %in% c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46) ~ "Midwest",
        `_STATE` %in% c(10, 12, 13, 24, 37, 45, 51, 11, 54, 1, 21, 28, 47, 5, 22, 40, 48) ~ "South",
        `_STATE` %in% c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53) ~ "West",
        `_STATE` %in% c(66, 72, 78, 60) ~ "Territory",
        TRUE ~ NA_character_
      ),
      levels = c("Northeast", "Midwest", "South", "West", "Territory")
    ),
    
    # State level FIPS code 
    state_fips = `_STATE`,
    
    # Health insurance coverage (standardized in 00)
    health_insurance = factor(
      case_when(
        health_insurance_raw == 1 ~ "Insured",
        health_insurance_raw == 2 ~ "Uninsured",
        health_insurance_raw == 9 ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("Insured", "Uninsured", "Missing")
    ),
    
    # Metropolitan Status
    met_stat = factor(
      case_when(
        `_METSTAT` == 1 ~ "Metropolitan counties",
        `_METSTAT` == 2 ~ "Nonmetropolitan counties",
        TRUE ~ "Missing"
      ),
      levels = c("Metropolitan counties", "Nonmetropolitan counties", "Missing")
    ),

    # Physical health days (numeric, kept continuous - useful predictor
    # of both general and mental health outcomes). 88 = zero days.
    physhlth_days = case_when(
      PHYSHLTH == 88 ~ 0,
      PHYSHLTH >= 1 & PHYSHLTH <= 30 ~ PHYSHLTH,
      TRUE ~ NA_real_
    ),
    
    flu_shot = factor(
      case_when(
        FLUSHOT7 == 1 ~ "Yes",
        FLUSHOT7 == 2 ~ "No",
        FLUSHOT7 %in% c(7, 9) ~ "Missing",
        TRUE ~ "Missing"
      ),
      levels = c("Yes", "No", "Missing")
    ),
    
    obesity = factor(
      case_when(
        `_BMI5CAT` == 1 ~ "Underweight",
        `_BMI5CAT` == 2 ~ "Normal weight",
        `_BMI5CAT` == 3 ~ "Overweight",
        `_BMI5CAT` == 4 ~ "Obese",
        TRUE ~ "Missing"
      ),
      levels = c("Underweight","Normal weight","Overweight","Obese","Missing")
    )
  )


# 4. Check the cleaned data
dim(brfss_clean)
table(brfss_clean$year, useNA = "ifany")
table(brfss_clean$employ_income, brfss_clean$year, useNA = "ifany")
table(brfss_clean$region, useNA = "ifany")
table(brfss_clean$health_insurance, useNA = "ifany")

# Sample size per year - see how many dropped/ retained at this stage
n_by_year <- brfss_clean %>% count(year, name = "n_clean")
print(n_by_year)

# 5. Save cleaned data
saveRDS(brfss_clean, "data/processed/brfss_clean.rds")
