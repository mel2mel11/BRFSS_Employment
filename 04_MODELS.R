# 04_MODELS.R
# BRFSS 2023-25: Primary regression models
#
# Population : working age adults 18-64
# Exposure   : employ_income (employ_education has been droped)
# Outcomes   : general health, mental health, physical inactivity,
#              heavy drinking, current smoking
# Reported   : imputed + weighted (PRIMARY result), plus
#              imputed-unweighted and unimputed-complete-case
#              versions as sensitivity comparisons
#
# Changes:
#             weight_adj = !LLCPWT / pooled years
#             scripts consistant with table 1c
#
#


# Load Packages
library(survey)
library(mice)
library(dplyr)
library(broom)

# survey options
options(survey.lonely.psu = "adjust")

# load data
completed_data <- readRDS("data/processed/completed_data.rds")
brfss_clean <- readRDS("data/processed/brfss_clean.rds")


n_years <- n_distinct(brfss_clean$year)

# 18-64 only (no-op if 03 already restricted) + adjusted pooled-year weight
# CHECK: i think the data might all be filtered to 18-64 now
completed_data <- lapply(completed_data, function(df) {
  df %>%
    filter(`_AGEG5YR` %in% 1:9) %>%
    mutate(
      age_group  = droplevels(age_group),
      weight_adj = `_LLCPWT` / n_years
    )
})

# define the primary model covariates
covariate_vars <- c("employ_income", "age_group", "race_collapsed",
                    "sex", "marital", "children", "educ_group", "year")
primary_covariates <- paste(covariate_vars, collapse = " + ")

# define the outcomes
outcomes <- c(
  health = "health_binary",
  mental = "mental_binary",
  physical_activity = "inactive_binary",
  drinking = "drinking_binary",
  smoking = "smoking_binary"
)

# sanity check here?

# 1. IMPUTED, UNWEIGHTED MODELS
# pooled across m =5imputations
run_unweighted_model <- function(data, outcome) {
  formula <- as.formula(paste(outcome, "~", primary_covariates))
  glm(formula, family = quasipoisson(link = "log"), data = data)
}

unweighted_models <- lapply(names(outcomes), function(outcome_name) {
    outcome_var <- outcomes[[outcome_name]]
    models <- lapply(completed_data, run_unweighted_model, outcome = outcome_var)
    pool(as.mira(models))
  }
)
names(unweighted_models) <- names(outcomes)

# 2. IMPUTED, WEIGHTED MODELS
# primary result, pooled across m = 5imputations

# weighted survey design (weight_adj = _LLCPWT / number of pooled years)
"sex", "marital", "children", "educ_group", "year")
primary_covariates <- paste(covariate_vars, collapse = " + ")

make_survey_design <- function(data) {
  svydesign(
    id = ~`_PSU`,
    strata = ~`_STSTR`,
    weights = ~weight_adj,
    data = data,
    nest = TRUE
  )
}

survey_designs <- lapply(completed_data,  make_survey_design)

# weighted imputed models function
run_weighted_model <- function(design, outcome) {
  formula <- as.formula(paste(outcome, "~", primary_covariates))
  svyglm(formula, design = design, family = quasipoisson(link = "log"))
}

weighted_models <- lapply(names(outcomes), function(outcome_name) {
  message("Weighted model: ", outcome_name)
  outcome_var <- outcomes[[outcome_name]]
  models <- lapply(survey_designs, run_weighted_model, outcome = outcome_var)
  pool(as.mira(models))
})
names(weighted_models) <- names(outcomes)

# 3. Sensitivity: Unimputed, Complete-Case Models
# drop missing instead of imputing, same 18-64 population
brfss_cc <- brfss_clean %>%
  filter(!is.na(age_strata)) %>%      # 18-64 only (age_strata is NA for 65+ / missing age). i think we can remove this line though
  mutate(
    # prepare outcomes for regression
    health_binary = case_when(
      GENHLTH %in% c(4, 5) ~ 1,
      GENHLTH %in% c(1, 2, 3) ~ 0
    ),
    mental_binary = case_when(
      `_MENT14D` == 3 ~ 1,
      `_MENT14D` %in% c(1, 2) ~ 0
    ),
    inactive_binary = case_when(
      `_TOTINDA` == 2 ~ 1,
      `_TOTINDA` == 1 ~ 0
    ),
    drinking_binary = case_when(
      `_RFDRHV9` == 2 ~ 1,
      `_RFDRHV9` == 1 ~ 0
    ),
    smoking_binary = case_when(
      `_SMOKER3` %in% c(1, 2) ~ 1,
      `_SMOKER3` %in% c(3, 4) ~ 0
    ),
    race_collapsed = factor(
      case_when(
        `_IMPRACE` == 1 ~ "NH White",
        `_IMPRACE` == 2 ~ "NH Black",
        `_IMPRACE` == 3 ~ "NH Asian",
        `_IMPRACE` == 5 ~ "Hispanic",
        TRUE ~ "Other/Multiracial"
      ),
      levels = c("NH White", "NH Black", "NH Asian", "Hispanic", "Other/Multiracial")
    )
  ) %>%
  filter( # drop missingness (WW/SE missing gone)
    !is.na(employ_income),
    employ_income %in% c("High Income WW", "Low Income WW",
                         "High Income SE", "Low Income SE",
                         "Unemployed", "Not in Labor Force"),
    marital != "Missing",
    children != "Missing",
    educ_group != "Missing"
  ) %>%
  mutate(
    employ_income  = droplevels(employ_income),
    employ_income  = relevel(employ_income, ref = "High Income WW"),
    race_collapsed = relevel(race_collapsed, ref = "NH White"),
    age_group      = droplevels(age_group),
    age_group      = relevel(age_group, ref = "18-29"),
    marital        = droplevels(marital),
    marital        = relevel(marital, ref = "Married/Partnered"),
    children       = droplevels(children),
    children       = relevel(children, ref = "No Children"),
    educ_group     = droplevels(educ_group),
    educ_group     = relevel(educ_group, ref = "College+")
  )

cat("Complete-case covariate N (18-64):",
    format(nrow(brfss_cc), big.mark = ","), "\n")

run_cc_model <- function(outcome) {
  formula <- as.formula(paste(outcome, "~", primary_covariates))
  data <- brfss_cc %>% filter(!is.na(.data[[outcome]]))
  list(
    unweighted = glm(formula, family = quasipoisson(link = "log"), data = data),
    weighted   = svyglm(
      formula,
      design = make_survey_design(data),
      family = quasipoisson(link = "log")
    )
  )
}

cc_models <- lapply(outcomes, run_cc_model)
names(cc_models) <- names(outcomes)

# 4. View Primary (Weighted, Imputed) Results
extract_pr <- function(model) summary(model, conf.int = TRUE, exponentiate = TRUE)

extract_pr(weighted_models$health)
extract_pr(weighted_models$mental)
extract_pr(weighted_models$physical_activity)
extract_pr(weighted_models$drinking)
extract_pr(weighted_models$smoking)

# Save results
saveRDS(survey_designs, "data/processed/survey_designs.rds")
saveRDS(unweighted_models, "data/processed/unweighted_models.rds")
saveRDS(weighted_models, "data/processed/weighted_models.rds")
saveRDS(cc_models, "data/processed/complete_case_models.rds")
