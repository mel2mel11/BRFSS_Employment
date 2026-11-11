# 03_IMPUTATION.R
#
# BRFSS 2023-2025: Multiple imputation
#
# Population: working age adults 18-64
#
# Changes from prior version: 
#   - restrict age before imputation
#   - FOR NOW: reduce max iterationd from 10 to 5
#   - 'year' - non-imputed predictor
#   - Auxiliary variables (region, health insurance, vaccination
#     status, physical health days, number of adults) are added
#     as predictor-only columns. not imputed themselves, but help predict
#   - state_fips excluded from the predictor matrix , too many levels


# Load Packages
library(mice)
library(dplyr)

# Load Cleaned Data
brfss_clean <- readRDS("data/processed/brfss_clean.rds")

# 0. restrict to working age before imputation
n_all_ages <- brfss_clean %>% count(year, name = "n_all_ages")

brfss_clean <- brfss_clean %>%
  filter(as.numeric(as.character(`_AGEG5YR`)) %in% 1:9)

n_18_64 <- brfss_clean %>% count(year, name = "n_18_64")
print(left_join(n_all_ages, n_18_64, by = "year"))

# 1. Prepare Data for Imputation:
# Create dataset
impute_data <- brfss_clean %>%
  select(
    
    # Variables to be imputed
    EMPLOY1, GENHLTH, `_INCOMG1`, `_EDUCAG`,
    `_MENT14D`, SEXVAR, MARITAL, `_IMPRACE`, `_AGEG5YR`, `_TOTINDA`,
    `_RFDRHV9`,`_SMOKER3`, CHILDREN,
    
    # Survey Design Variables - not imputed, not used as predictors
    `_STSTR`, `_LLCPWT`, `_PSU`,
    
    # non-imputed predictors:
    year, region, state_fips,
    
    # auxiliary predictors KEPT (low missingness; NA -> "Unknown" below):
    `_METSTAT`, health_insurance_raw
    
    # auxiliary predictors COMMENTED OUT (too much missingness?):
    # `_RACE`, `_CHLDCNT`, `_BMI5CAT`, CHECKUP1, FLUSHOT7, PHYSHLTH, MENTHLTH
  ) %>%
  mutate(
    
    # Convert BRFSS special codes to NA.
    # _LLCPWT, _STSTR, _PSU are survey design variables. 
    # SEXVAR, _IMPRACE, _AGEG5YR already imputed by BRFSS.
    EMPLOY1 = ifelse(EMPLOY1 == 9, NA_real_, as.numeric(EMPLOY1)),
    `_INCOMG1` = ifelse(`_INCOMG1` == 9, NA_real_, as.numeric(`_INCOMG1`)),
    `_EDUCAG` = ifelse(`_EDUCAG` == 9, NA_real_, as.numeric(`_EDUCAG`)),
    GENHLTH = ifelse(GENHLTH %in% c(7,9), NA_real_, as.numeric(GENHLTH)),
    `_MENT14D` = ifelse(`_MENT14D` == 9, NA_real_, as.numeric(`_MENT14D`)),
    `_TOTINDA` = ifelse(`_TOTINDA` == 9, NA_real_, as.numeric(`_TOTINDA`)),
    `_RFDRHV9` = ifelse(`_RFDRHV9` == 9, NA_real_, as.numeric(`_RFDRHV9`)),
    `_SMOKER3` = ifelse(`_SMOKER3` == 9, NA_real_, as.numeric(`_SMOKER3`)),
    MARITAL = ifelse(MARITAL == 9, NA_real_, as.numeric(MARITAL)),
    CHILDREN = ifelse(CHILDREN == 99, NA_real_, as.numeric(CHILDREN)),
    
    # --- auxiliary predictors: clean special codes, NAs stay as is.
    health_insurance = factor(
      case_when(
        health_insurance_raw == 1 ~ "Insured",
        health_insurance_raw == 2 ~ "Uninsured",
        TRUE ~ "Unknown"
      ),
      levels = c("Insured", "Uninsured", "Unknown")
    ),
    
    urban_rural = factor(
      case_when(
        `_METSTAT` == 1 ~ "Metropolitan",
        `_METSTAT` == 2 ~ "Nonmetropolitan",
        TRUE ~ "Unknown"
      ),
      levels = c("Metropolitan", "Nonmetropolitan", "Unknown")
    ),
    
    # # COMMENTED OUT AUXILARY PREDICTORS
    # 
    # # redundant with GENHEALTH, this one has NAs
    # physhlth_days = case_when(
    #   PHYSHLTH == 88 ~ 0,
    #   PHYSHLTH >= 1 & PHYSHLTH <= 30 ~ PHYSHLTH,
    #   TRUE ~ "Unknown"
    # ),
    # 
    # # derived from _IMPRACE, already in the model
    # race_detailed = factor(
    #   case_when(
    #     `_RACE` %in% 1:8 ~ as.character(`_RACE`),
    #     TRUE ~ NA_character_   # 9 = don't know/refused/missing
    #   )
    # ),
    # 
    # # Derived from CHILDREN, missingness
    # children_count = factor(
    #   case_when(
    #     `_CHLDCNT` %in% 1:6 ~ as.character(`_CHLDCNT`),
    #     TRUE ~ NA_character_   # 9 = missing
    #   )
    # ),
    # 
    # # About 9% missingness
    # obesity = factor(
    #   case_when(
    #     `_BMI5CAT` %in% 1:4 ~ as.character(`_BMI5CAT`),
    #     TRUE ~ NA_character_   # BLANK = don't know/refused/missing (no "9" code for this var)
    #   )
    # ),
    # 
    # checkup = factor(
    #   case_when(
    #     CHECKUP1 == 1 ~ "Within past year",
    #     CHECKUP1 == 2 ~ "1-2 years ago",
    #     CHECKUP1 == 3 ~ "2-5 years ago",
    #     CHECKUP1 == 4 ~ "5+ years ago",
    #     CHECKUP1 == 8 ~ "Never",
    #     TRUE ~ NA_character_   # 7/9 = don't know/refused
    #   ),
    #   levels = c("Within past year", "1-2 years ago", "2-5 years ago",
    #              "5+ years ago", "Never")
    # ),
    # 
    # # ~8% Missing
    # flu_shot = factor(
    #   case_when(
    #     FLUSHOT7 == 1 ~ "Yes",
    #     FLUSHOT7 == 2 ~ "No",
    #     TRUE ~ NA_character_   # 7/9 = don't know/refused
    #   ),
    #   levels = c("Yes", "No")
    # )
    # 
  ) %>%
  select(-health_insurance_raw, -`_METSTAT`)

  # And aditionally, commented out now,
  # select(-PHYSHLTH, -FLUSHOT7, -`_RACE`, -`_CHLDCNT`, -`_BMI5CAT`, -CHECKUP1,)

# check missingness
colSums(is.na(impute_data))


# DIAGNOSTIC: % missing by year. 
impute_data %>%
  group_by(year) %>%
  summarise(across(everything(), ~ round(mean(is.na(.x)) * 100, 1))) %>%
  print(width = Inf)


# convert categorical vars to factor
impute_data <- impute_data %>%
  mutate(
    EMPLOY1    = as.factor(EMPLOY1),
    GENHLTH    = as.factor(GENHLTH),
    `_INCOMG1` = as.factor(`_INCOMG1`),
    `_EDUCAG`  = as.factor(`_EDUCAG`),
    `_MENT14D` = as.factor(`_MENT14D`),
    MARITAL    = as.factor(MARITAL),
    `_SMOKER3` = as.factor(`_SMOKER3`),
    `_TOTINDA` = as.factor(`_TOTINDA`),
    `_RFDRHV9` = as.factor(`_RFDRHV9`)
    # CHILDREN, physhlth_days stay numeric for pmm
    # MENTHLTH stays numeric
    # survey vars stay as-is
    # year, region, race_detailed,health_insurance, flu_shot, 
    # obesity, checkup, met_stat are already factors
  )

# 2. Define Imputation Methods
methods <- make.method(impute_data)

# polyreg:  >2 class categorical variable 
methods["GENHLTH"]  <- "polyreg"
methods["_MENT14D"] <- "polyreg"
methods["_INCOMG1"] <- "polyreg"
methods["_EDUCAG"] <- "polyreg"
methods["EMPLOY1"] <- "polyreg"
methods["MARITAL"] <- "polyreg"
methods["_SMOKER3"] <- "polyreg"

# logreg: binary categorical varaibles
methods["_TOTINDA"] <- "logreg"
methods["_RFDRHV9"] <- "logreg"

# pmm: numeric count varible, predictive mean matching
methods["CHILDREN"] <- "pmm"

# "": do not impute these survey weights
methods["_LLCPWT"] <- ""
methods["_STSTR"] <- ""
methods["_PSU"] <- ""
methods["SEXVAR"]  <- ""
methods["_IMPRACE"] <- ""
methods["_AGEG5YR"] <- ""

# "": do not impute - auxiliary predictors
methods["year"]              <- ""
methods["region"]            <- ""
methods["state_fips"]        <- ""
methods["health_insurance"]  <- ""
# methods["physhlth_days"]     <- ""
# methods["race_detailed"]     <- ""
# methods["children_count"]    <- ""
# methods["obesity"]           <- ""
# methods["checkup"]           <- ""
methods["urban_rural"]          <- ""
# methods["flu_shot"]          <- ""

# 3. Create Predictor Matrix
pred <- make.predictorMatrix(impute_data)
# # Survey Variables excluded from prediction/imputation
pred[, c("_LLCPWT", "_STSTR", "_PSU")] <- 0
# Exclude state_fips as a predictor- too large
pred[, "state_fips"] <- 0
# Exclude predictors that have too much missing in themselves


# NOT USING THESE PREDICTORS- going the automatic route (left in code as reference)
# # Income and Employment likely predict eachother
# pred["EMPLOY1", "_INCOMG1"] <- 0
# pred["_INCOMG1", "EMPLOY1"] <- 0
# # Also Income and Education
# pred["_EDUCAG", "_INCOMG1"] <- 0
# pred["_INCOMG1", "_EDUCAG"] <- 0
# # Children and Age 
# pred["CHILDREN", "_AGEG5YR"] <- 0
# pred["_AGEG5YR", "CHILDREN"] <- 0



# GUARD: any variable that is NOT imputed but IS used as a predictor must have
# zero NAs. If this fails, the imputed variables will come back with NAs.
not_imputed   <- names(methods)[methods == ""]
used_as_pred  <- not_imputed[colSums(pred[, not_imputed, drop = FALSE]) > 0]
na_in_pred    <- colSums(is.na(impute_data[, used_as_pred, drop = FALSE]))
print(na_in_pred)
stopifnot(all(na_in_pred == 0))


# 4. Run MICE Imputation______________________________________________
set.seed(12345)

mice_imp <- mice(
  impute_data,
  m = 5,
  maxit = 5, # FOR NOW, change back to 10!
  method = methods,
  predictorMatrix = pred,
  seed = 12345,
  printFlag = TRUE
)

# Check Imputation
summary(mice_imp)
# Check for no missing values
colSums(is.na(complete(mice_imp, 1)))

# 5. Build analysis variables on each completed dataset
# includes outcomes, covariates, and the primary exposure employ_income
create_analysis_data <- function(data) {
  data %>%
    mutate(
      # convert back to numeric codes for case_when
      EMPLOY1    = as.numeric(as.character(EMPLOY1)),
      GENHLTH    = as.numeric(as.character(GENHLTH)),
      `_INCOMG1` = as.numeric(as.character(`_INCOMG1`)),
      `_EDUCAG`  = as.numeric(as.character(`_EDUCAG`)),
      `_MENT14D` = as.numeric(as.character(`_MENT14D`)),
      SEXVAR     = as.numeric(as.character(SEXVAR)),
      MARITAL    = as.numeric(as.character(MARITAL)),
      `_IMPRACE` = as.numeric(as.character(`_IMPRACE`)),
      `_AGEG5YR` = as.numeric(as.character(`_AGEG5YR`)),
      `_TOTINDA` = as.numeric(as.character(`_TOTINDA`)),
      `_RFDRHV9` = as.numeric(as.character(`_RFDRHV9`)),
      `_SMOKER3` = as.numeric(as.character(`_SMOKER3`))
    ) %>%
    mutate(
      # outcome: general health
      health_binary = case_when(
        GENHLTH %in% c(4, 5) ~ 1,
        GENHLTH %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # outcome: mental health
      mental_binary = case_when(
        `_MENT14D` == 3 ~ 1,
        `_MENT14D` %in% c(1, 2) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # outcome: physical inactivity
      inactive_binary = case_when(
        `_TOTINDA` == 2 ~ 1,
        `_TOTINDA` == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      
      # outcome: heavy drinking
      drinking_binary = case_when(
        `_RFDRHV9` == 2 ~ 1,
        `_RFDRHV9` == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      
      # outcome: current smoking
      smoking_binary = case_when(
        `_SMOKER3` %in% c(1, 2) ~ 1,
        `_SMOKER3` %in% c(3, 4) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # sex
      sex = factor(
        case_when(
          SEXVAR == 1 ~ "Male",
          SEXVAR == 2 ~ "Female",
          TRUE ~ NA_character_
        ),
        levels = c("Male", "Female")
      ),
      
      # age
      age_group = factor(
        case_when(
          `_AGEG5YR` %in% c(1, 2) ~ "18-29",
          `_AGEG5YR` %in% c(3, 4, 5) ~ "30-44",
          `_AGEG5YR` %in% c(6, 7, 8, 9) ~ "45-64",
          `_AGEG5YR` %in% c(10, 11, 12, 13) ~ "65+",
          TRUE ~ NA_character_
        ),
        levels = c("18-29", "30-44", "45-64", "65+")
      ),
      
      # precise strata for effect modification (see note in 01_CLEAN.R)
      age_strata = factor(
        case_when(
          `_AGEG5YR` %in% c(1, 2, 3) ~ "18-34",
          `_AGEG5YR` %in% c(4, 5, 6) ~ "35-49",
          `_AGEG5YR` %in% c(7, 8, 9) ~ "50-64",
          TRUE ~ NA_character_
        ),
        levels = c("18-34", "35-49", "50-64")
      ),
      
      # race / ethnicity
      race_collapsed = factor(
        case_when(
          `_IMPRACE` == 1 ~ "NH White",
          `_IMPRACE` == 2 ~ "NH Black",
          `_IMPRACE` == 3 ~ "NH Asian",
          `_IMPRACE` == 5 ~ "Hispanic",
          TRUE ~ "Other/Multiracial" # collapses AI/AN + multiracial
        ),
        levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other/Multiracial")
      ),
      
      # marital status
      marital = factor(
        case_when(
          MARITAL %in% c(1, 6) ~ "Married/Partnered",
          MARITAL %in% c(2, 3, 4, 5) ~ "Not Married/Partnered",
          TRUE ~ NA_character_
        ),
        levels = c("Married/Partnered", "Not Married/Partnered"
        )
      ),
      
      # children
      children = factor(
        case_when(
          CHILDREN == 88 ~ "No Children",
          CHILDREN >= 1 & CHILDREN <= 87 ~ "Children Present",
          TRUE ~ NA_character_
        ),
        levels = c("No Children", "Children Present")
      ),
      
      # income 
      income_group = factor(
        case_when(
          `_INCOMG1` %in% c(1, 2, 3, 4) ~ "Low (<$50k)",
          `_INCOMG1` %in% c(5, 6, 7) ~ "High ($50k+)",
          TRUE ~ NA_character_
        ),
        levels = c("High ($50k+)", "Low (<$50k)")
      ),
      
      # education
      educ_group = factor(
        case_when(
          `_EDUCAG` %in% c(1, 2) ~ "HS or Less",
          `_EDUCAG` == 3 ~ "Some College",
          `_EDUCAG` == 4 ~ "College+",
          TRUE ~ NA_character_
        ),
        levels = c( "College+", "Some College", "HS or Less")
      ),
      
      # PRIMARY EXPOSURE - after imputation, no "missing" levels
      employ_income = factor(
        case_when(
          EMPLOY1 == 1 & `_INCOMG1` %in% c(5, 6, 7)    ~ "High Income WW",
          EMPLOY1 == 1 & `_INCOMG1` %in% c(1, 2, 3, 4) ~ "Low Income WW",
          EMPLOY1 == 2 & `_INCOMG1` %in% c(5, 6, 7)    ~ "High Income SE",
          EMPLOY1 == 2 & `_INCOMG1` %in% c(1, 2, 3, 4) ~ "Low Income SE",
          EMPLOY1 %in% c(3, 4) ~ "Unemployed",
          EMPLOY1 %in% c(5, 6, 7, 8) ~ "Not in Labor Force"
        ),
        levels = c("High Income WW", "Low Income WW", "High Income SE",
                   "Low Income SE",  "Unemployed", "Not in Labor Force")
      )
    ) %>%
    
  # set reference levels
  mutate(
    employ_income = relevel(employ_income, ref = "High Income WW"),
    sex = relevel(sex, ref = "Male"),
    race_collapsed = relevel(race_collapsed, ref = "NH White"),
    age_group = relevel(age_group, ref = "18-29"),
    marital = relevel(marital, ref = "Married/Partnered"),
    educ_group = relevel(educ_group, ref = "College+"),
    children = relevel(children, ref = "No Children"),
    year = relevel(year, ref = levels(year)[1]) 
  )
}

# create all completed datasets
completed_data <- lapply(1:5, function(i) {
    create_analysis_data(complete(mice_imp, i))
  }
)

# check
table(completed_data[[1]]$employ_income, useNA = "ifany")
table(completed_data[[1]]$year, useNA = "ifany")
table(completed_data[[1]]$age_strata, useNA = "ifany")

# Sample size per year, post-imputation (should match n_clean_by_year since imputation doesn't drop rows)
n_imputed_by_year <- completed_data[[1]] %>% count(year, name = "n_imputed")
print(n_imputed_by_year)

# 6. save imputation objects
saveRDS(mice_imp, "data/processed/mice_imp.rds")
saveRDS(completed_data, "data/processed/completed_data.rds")


# NOTE: employ_educ (employment x education) is intentionally NOT
# re-derived here. It's only used descriptively in Table 1B, which
# is built pre-imputation directly from brfss_clean (02_table1.R).
