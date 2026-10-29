# 04_VARIABLES.R
# Construct analysis variables AFTER multiple imputation

# 1. import packages and data
library(dplyr)
library(mice)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

d <- readRDS("data/processed/02_clean.rds")
imp <- readRDS("data/processed/03_mice.rds")

targets <- c(
  "EMPLOY1", "INCOME3", "EDUCA", "MARITAL",
  "CHILDREN_CAT", "SMOKING", "ALCOHOL"
)

stopifnot(
  all(targets %in% names(imp$data)),
  nrow(d) == nrow(imp$data)
)

# Convert factor-coded BRFSS responses back to numeric codes.
to_code <- function(x) {
  as.integer(as.character(x))
}

# 2. Derive analysis variables

make_vars <- function(x) {
  
  # keep original, observed children count.
  if (!"CHILDREN_CAT" %in% names(x)) {
    x$CHILDREN_CAT <- ifelse(
      is.na(x$CHILDREN),
      NA_integer_,
      as.integer(x$CHILDREN > 0)
    )
  }
  
  # original cleaned smoking/alcohol codes
  if (!"SMOKING" %in% names(x)) {
    x$SMOKING <- x$smoking_code
  }
  
  if (!"ALCOHOL" %in% names(x)) {
    x$ALCOHOL <- x$alcohol_code
  }
  
  x %>%
    mutate(
      
      # Employment
      employment = case_when(
        EMPLOY1 == 1 ~ "Wage worker",
        EMPLOY1 == 2 ~ "Self-employed",
        EMPLOY1 %in% 3:4 ~ "Unemployed",
        EMPLOY1 %in% 5:8 ~ "Not in labor force",
        TRUE ~ NA_character_
      ),
      
      # Household income
      income_group = case_when(
        INCOME3 %in% 1:6 ~ "Under $50k",
        INCOME3 %in% 7:11 ~ "$50k+",
        TRUE ~ NA_character_
      ),
      
      # Education
      education_group = case_when(
        EDUCA %in% 1:4 ~ "High school or less",
        EDUCA == 5 ~ "Some college",
        EDUCA == 6 ~ "College graduate",
        TRUE ~ NA_character_
      ),
      
      # Main exposure: six categories
      employment_income = case_when(
        EMPLOY1 == 1 & INCOME3 %in% 1:6 ~
          "Low-income wage worker",
        EMPLOY1 == 1 & INCOME3 %in% 7:11 ~
          "High-income wage worker",
        EMPLOY1 == 2 & INCOME3 %in% 1:6 ~
          "Low-income self-employed",
        EMPLOY1 == 2 & INCOME3 %in% 7:11 ~
          "High-income self-employed",
        EMPLOY1 %in% 3:4 ~ "Unemployed",
        EMPLOY1 %in% 5:8 ~ "Not in labor force",
        TRUE ~ NA_character_
      ),
      
      employment_income = factor(
        employment_income,
        levels = c(
          "High-income wage worker",
          "Low-income wage worker",
          "High-income self-employed",
          "Low-income self-employed",
          "Unemployed",
          "Not in labor force"
        )
      ),
      
      # Age: five categories for main models
      age_model = case_when(
        age >= 18 & age <= 24 ~ "18-24",
        age >= 25 & age <= 34 ~ "25-34",
        age >= 35 & age <= 44 ~ "35-44",
        age >= 45 & age <= 54 ~ "45-54",
        age >= 55 & age <= 64 ~ "55-64",
        TRUE ~ NA_character_
      ),
      
      age_model = factor(
        age_model,
        levels = c(
          "18-24", "25-34", "35-44", "45-54", "55-64"
        )
      ),
      
      # Age: three categories for effect modification
      age_effect = case_when(
        age >= 18 & age <= 34 ~ "18-34",
        age >= 35 & age <= 49 ~ "35-49",
        age >= 50 & age <= 64 ~ "50-64",
        TRUE ~ NA_character_
      ),
      
      age_effect = factor(
        age_effect,
        levels = c("18-34", "35-49", "50-64")
      ),
      
      # Sex
      sex_group = factor(
        case_when(
          sex == 1 ~ "Male",
          sex == 2 ~ "Female",
          TRUE ~ NA_character_
        ),
        levels = c("Male", "Female")
      ),
      
      # Race/ethnicity
      race_group = factor(
        case_when(
          race == 1 ~ "NH White",
          race == 2 ~ "NH Black",
          race == 3 ~ "NH Asian",
          race == 4 ~ "American Indian/Alaska Native",
          race == 5 ~ "Hispanic",
          race == 6 ~ "Other",
          TRUE ~ NA_character_
        ),
        levels = c(
          "NH White",
          "NH Black",
          "NH Asian",
          "American Indian/Alaska Native",
          "Hispanic",
          "Other"
        )
      ),
      
      # Marital status
      marital_group = factor(
        case_when(
          MARITAL %in% c(1, 6) ~ "Married/Partnered",
          MARITAL %in% c(2, 3, 4) ~
            "Divorced/Widowed/Separated",
          MARITAL == 5 ~ "Never married",
          TRUE ~ NA_character_
        ),
        levels = c(
          "Married/Partnered",
          "Divorced/Widowed/Separated",
          "Never married"
        )
      ),
      
      # Children
      children_group = factor(
        case_when(
          CHILDREN_CAT == 0 ~ "No children",
          CHILDREN_CAT == 1 ~ "One or more children",
          TRUE ~ NA_character_
        ),
        levels = c("No children", "One or more children")
      ),
      
      # Smoking
      smoking_group = factor(
        case_when(
          SMOKING %in% c(1, 2) ~ "Current",
          SMOKING == 3 ~ "Former",
          SMOKING == 4 ~ "Never",
          TRUE ~ NA_character_
        ),
        levels = c("Never", "Former", "Current")
      ),
      
      # Heavy drinking
      alcohol_group = factor(
        case_when(
          ALCOHOL == 1 ~ "No",
          ALCOHOL == 2 ~ "Yes",
          TRUE ~ NA_character_
        ),
        levels = c("No", "Yes")
      ),
      
      # Survey year
      year_group = factor(
        as.character(year),
        levels = c("2023", "2024", "2025")
      ),
      
      # Health outcomes: NOT imputed
      poor_general_health = case_when(
        GENHLTH %in% 4:5 ~ 1L,
        GENHLTH %in% 1:3 ~ 0L,
        TRUE ~ NA_integer_
      ),
      
      poor_mental_health_14d = case_when(
        MENTHLTH >= 14 & MENTHLTH <= 30 ~ 1L,
        MENTHLTH >= 0 & MENTHLTH < 14 ~ 0L,
        TRUE ~ NA_integer_
      )
    )
}

# 2. Save observed Data
observed <- make_vars(d)
saveRDS(observed, "data/processed/04_observed.rds")

# 3. Completed datasets
completed <- lapply(seq_len(imp$m), function(i) {
  filled <- mice::complete(imp, action = i)
  x <- d
  
  # add all seven imputed variables.
  for (v in targets) {
    x[[v]] <- to_code(filled[[v]])
  }
  
  x <- make_vars(x)
  x$.imp <- i
  
  x
})

# 4. Valididation
stopifnot(all(vapply(completed, nrow, integer(1)) == nrow(d)))

for (x in completed) {
  stopifnot(all(colSums(is.na(x[, targets, drop = FALSE])) == 0))
  
  # Outcomes must remain unchanged.
  stopifnot(
    identical(x$GENHLTH, d$GENHLTH),
    identical(x$MENTHLTH, d$MENTHLTH)
  )
}

# Verify survey weights are available.
stopifnot(
  all(c(
    "weight_3yr",
    "weight_annual",
    "psu_year",
    "strata_year"
  ) %in% names(completed[[1]]))
)

# 5. save
saveRDS(completed, "data/processed/04_imputed_datasets.rds")

cat("\nEmployment-income groups:\n")
print(table(completed[[1]]$employment_income, useNA = "ifany"))

cat("\nRemaining missing values in completed dataset #1:\n")
print(colSums(is.na(completed[[1]][, targets, drop = FALSE])))

# Final Message
message("Script 04 complete.")
