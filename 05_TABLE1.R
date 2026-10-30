# 05_TABLE1.R
#
# Table 1A: Observed, unweighted
# Table 1B: Imputed, unweighted
# Table 1C: Imputed, survey-weighted
#
# imputed tables use completed imputation #1 produced from script 03

# 1. load packages and import data
library(dplyr)
library(tidyr)
library(purrr)
library(gtsummary)
library(gt)
library(survey)

options(survey.lonely.psu = "adjust")
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

observed <- readRDS("data/processed/04_observed.rds")
imputed <- readRDS("data/processed/04_imputed_datasets.rds")
stopifnot(is.list(imputed), length(imputed) > 0)

# use imputed dataset #1
completed <- imputed[[1]]

# 2. DEFINE EMPLOYMENT-INCOME GROUPS

# Also preserve missing-income groups in Table 1A.

make_exposure <- function(df, include_missing = FALSE) {
  
  df <- df %>%
    mutate(
      employment_income = as.character(employment_income)
    )
  
  if (include_missing) {
    
    df <- df %>%
      mutate(
        employment_income = case_when(
          EMPLOY1 == 1 & is.na(INCOME3) ~
            "Wage worker - missing income",
          
          EMPLOY1 == 2 & is.na(INCOME3) ~
            "Self-employed - missing income",
          
          TRUE ~ employment_income
        )
      )
  }
  
  group_order <- c(
    "High-income wage worker",
    "Low-income wage worker",
    "Wage worker - missing income",
    "High-income self-employed",
    "Low-income self-employed",
    "Self-employed - missing income",
    "Unemployed",
    "Not in labor force"
  )
  
  df$employment_income <- factor(
    df$employment_income,
    levels = group_order
  )
  
  df
}

observed <- make_exposure(observed, TRUE)
completed <- make_exposure(completed, FALSE)

# 3. PREPARE TABLE INFORMATION

prepare_table_data <- function(df) {
  
  df %>%
    mutate(
      children_group = case_when(
        is.na(CHILDREN_CAT) ~ NA_character_,
        CHILDREN_CAT == 0 ~ "No children",
        CHILDREN_CAT > 0 ~ "One or more children"
      ),
      
      children_group = factor(
        children_group,
        levels = c(
          "No children",
          "One or more children"
        )
      ),
      
      general_health = factor(
        case_when(
          poor_general_health == 1 ~ "Fair/Poor",
          poor_general_health == 0 ~
            "Excellent/Very good/Good",
          TRUE ~ NA_character_
        ),
        levels = c(
          "Excellent/Very good/Good",
          "Fair/Poor"
        )
      ),
      
      mental_health = factor(
        case_when(
          poor_mental_health_14d == 1 ~ "14+ days",
          poor_mental_health_14d == 0 ~ "0-13 days",
          TRUE ~ NA_character_
        ),
        levels = c("0-13 days", "14+ days")
      )
    )
}

observed <- prepare_table_data(observed)
completed <- prepare_table_data(completed)

table_vars <- c(
  "age_model",
  "sex_group",
  "race_group",
  "marital_group",
  "education_group",
  "income_group",
  "children_group",
  "smoking_group",
  "alcohol_group",
  "general_health",
  "mental_health"
)

stopifnot(
  all(table_vars %in% names(observed)),
  all(table_vars %in% names(completed))
)

# 4. Create Sample Flow
able1a_data <- observed %>%
  filter(!is.na(employment_income)) %>%
  droplevels()

table1b_data <- completed %>%
  filter(!is.na(employment_income)) %>%
  droplevels()

sample_flow <- tibble(
  measure = c(
    "Eligible adults ages 18-64",
    "Missing employment",
    "Wage workers missing income",
    "Self-employed missing income",
    "Table 1A included",
    "Table 1B/1C included"
  ),
  
  n = c(
    nrow(observed),
    sum(is.na(observed$EMPLOY1)),
    sum(
      observed$EMPLOY1 == 1 & is.na(observed$INCOME3),
      na.rm = TRUE
    ),
    sum(
      observed$EMPLOY1 == 2 & is.na(observed$INCOME3),
      na.rm = TRUE
    ),
    nrow(table1a_data),
    nrow(table1b_data)
  )
)

print(sample_flow)

write.csv(
  sample_flow,
  "results/tables/table1_sample_flow.csv",
  row.names = FALSE
)

# 5. LABELS

table_labels <- list(
  age_model ~ "Age",
  sex_group ~ "Sex",
  race_group ~ "Race/ethnicity",
  marital_group ~ "Marital status",
  education_group ~ "Education",
  income_group ~ "Household income",
  children_group ~ "Children in household",
  smoking_group ~ "Smoking status",
  alcohol_group ~ "Heavy drinking",
  general_health ~ "General health",
  mental_health ~ "Poor mental health days"
)

# 6. TABLE 1A: UNIMPUTED, UNWEIGHTED

table1A <- table1a_data %>%
  select(employment_income, all_of(table_vars)) %>%
  tbl_summary(
    by = employment_income,
    missing = "ifany",
    missing_text = "Missing",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = table_labels
  ) %>%
  add_overall() %>%
  bold_labels()

# 7. TABLE 1B: IMPUTED, UNWEIGHTED

table1B <- table1b_data %>%
  select(employment_income, all_of(table_vars)) %>%
  tbl_summary(
    by = employment_income,
    missing = "ifany",
    missing_text = "Missing",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = table_labels
  ) %>%
  add_overall() %>%
  bold_labels()

# 8. TABLE 1C: IMPUTED, SURVEY-WEIGHTED
# note: This one takes so long to run, because of tbl_svysummary?

# survey design
design <- svydesign(
  ids = ~psu_year,
  strata = ~strata_year,
  weights = ~weight_3yr,
  data = table1b_data,
  nest = TRUE
)

# Create table 1C
table1C <- design %>%
  tbl_svysummary(
    by = employment_income,
    include = all_of(table_vars),
    
    statistic = all_categorical() ~
      "{n_unweighted} ({p}%)",
    
    missing = "ifany",
    missing_text = "Missing",
    
    label = table_labels
  ) %>%
  add_overall() %>%
  bold_labels()

# format
table1C_gt <- table1C %>%
  as_gt() %>%
  tab_header(
    title = "Table 1C. After Imputation - Survey-Weighted",
    subtitle = "BRFSS 2023-2025, ages 18-64"
  ) %>%
  tab_source_note(
    source_note = paste(
      "Completed imputation #1.",
      "Unweighted counts and survey-weighted percentages.",
      "Pooled survey weights = annual weights / 3.",
      "Health outcomes were not imputed."
    )
  )

# 9. SAVE TABLES
save_table <- function(tbl, title, note, filename) {
  
  output <- tbl %>%
    as_gt() %>%
    tab_header(title = title) %>%
    tab_source_note(source_note = note)
  
  gtsave(output, filename)
}

save_table(
  table1A,
  "Table 1A. Before Imputation - Unweighted",
  "Observed respondent counts and column percentages. Missing values shown.",
  "results/tables/table1A.html"
)

save_table(
  table1B,
  "Table 1B. After Imputation - Unweighted",
  "Completed imputation #1. Health outcomes were not imputed.",
  "results/tables/table1B.html"
)

gtsave(table1C_gt, "results/tables/table1C.html")

saveRDS(
  list(A = table1A, B = table1B, C = table1C),
  "data/processed/05_table1_objects.rds"
)

# Final Message
message("All three Table 1 tables saved.")
message("Script 05 Complete.")
