# 05_EFFECT_MODIFICATION.R
# BRFSS 2023-25: Effect modification of the employment-income /
# health association, among WORKING-AGE ADULTS (18-64) only.
#
# For each candidate effect modifier (age, sex, race, YEAR) we:
#     - fit an interaction model, report the p-values, 
#       to see if stratification is right, then
#     - fit separate stratum-specific models to get
#       adjusted PRs and visual outputs (forest plots, tables).
#
# Outcomes examined: general health and mental health
#
# NOTE: need to edit this script to adjust
#


# load packages
library(survey)
library(mice)
library(dplyr)
library(broom)

# adjust
options(survey.lonely.psu = "adjust")

# import data
completed_data <- readRDS("data/processed/completed_data.rds")

# 1. restrict to working age adults
working_age_data <- lapply(completed_data, function(df) {
  df %>%
    filter(age_group != "65+") %>%
    mutate(
      age_group   = droplevels(age_group),
      age_strata  = droplevels(age_strata),
      sex_strata  = sex,
      year_strata = year
    )
})

working_age_design <- lapply(working_age_data, function(df) {
  svydesign(
    id = ~`_PSU`, strata = ~`_STSTR`, weights = ~`_LLCPWT`,
    data = df, nest = TRUE
  )
})

base_covariates <- c("race_collapsed", "sex", "marital", "children", "educ_group")

outcomes <- c(health = "health_binary", mental = "mental_binary")


# 2. Helper functions
fit_pooled <- function(designs, formula) {
  models <- lapply(designs, function(d) {
    svyglm(formula, design = d, family = quasipoisson(link = "log"))
  })
  pool(as.mira(models))
}

extract_interactions <- function(pooled_model, modifier_prefix) {
  summary(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl(paste0("employ_income.*:", modifier_prefix), term))
}

extract_stratum_pr <- function(pooled_model, stratum_label, outcome_label) {
  summary(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl("^employ_income", term)) %>%
    transmute(
      outcome  = outcome_label,
      stratum  = stratum_label,
      group    = sub("employ_income", "", term),
      PR       = round(estimate, 2),
      CI_low   = round(`2.5 %`, 2),
      CI_high  = round(`97.5 %`, 2),
      p_value  = round(p.value, 3)
    )
}


# 3. Effect Modification by AGE
age_interaction_models <- lapply(names(outcomes), function(nm) {
  formula <- as.formula(paste(
    outcomes[[nm]], "~ employ_income * age_strata +", paste(base_covariates, collapse = " + ")
  ))
  fit_pooled(working_age_design, formula)
})
names(age_interaction_models) <- names(outcomes)

age_interaction_results <- lapply(names(outcomes), function(nm) {
  extract_interactions(age_interaction_models[[nm]], "age_strata") %>%
    mutate(outcome = nm)
})
age_interaction_results <- bind_rows(age_interaction_results)
age_interaction_results

age_strata_levels <- levels(working_age_data[[1]]$age_strata)

age_stratified_results <- lapply(names(outcomes), function(nm) {
  lapply(age_strata_levels, function(a) {
    formula <- as.formula(paste(
      outcomes[[nm]], "~ employ_income +", paste(base_covariates, collapse = " + ")
    ))
    designs_a <- lapply(working_age_design, function(d) subset(d, age_strata == a))
    pooled <- fit_pooled(designs_a, formula)
    extract_stratum_pr(pooled, a, nm)
  }) %>% bind_rows()
}) %>% bind_rows()

age_stratified_results


# 4. Effect Modification by SEX
sex_interaction_models <- lapply(names(outcomes), function(nm) {
  formula <- as.formula(paste(
    outcomes[[nm]], "~ employ_income * sex_strata + age_group +",
    paste(base_covariates[base_covariates != "sex"], collapse = " + ")
  ))
  fit_pooled(working_age_design, formula)
})
names(sex_interaction_models) <- names(outcomes)

sex_interaction_results <- lapply(names(outcomes), function(nm) {
  extract_interactions(sex_interaction_models[[nm]], "sex_strata") %>%
    mutate(outcome = nm)
})
sex_interaction_results <- bind_rows(sex_interaction_results)
sex_interaction_results

sex_covariates <- c("race_collapsed", "age_group", "marital", "children", "educ_group")

sex_stratified_results <- lapply(names(outcomes), function(nm) {
  lapply(c("Male", "Female"), function(s) {
    formula <- as.formula(paste(
      outcomes[[nm]], "~ employ_income +", paste(sex_covariates, collapse = " + ")
    ))
    designs_s <- lapply(working_age_design, function(d) subset(d, sex_strata == s))
    pooled <- fit_pooled(designs_s, formula)
    extract_stratum_pr(pooled, s, nm)
  }) %>% bind_rows()
}) %>% bind_rows()

sex_stratified_results

# 5. Effect Modification by RACE/ethnicity
race_interaction_models <- lapply(names(outcomes), function(nm) {
  formula <- as.formula(paste(
    outcomes[[nm]], "~ employ_income * race_collapsed + age_group + sex +",
    paste(base_covariates[!base_covariates %in% c("race_collapsed", "sex")], collapse = " + ")
  ))
  fit_pooled(working_age_design, formula)
})
names(race_interaction_models) <- names(outcomes)

race_interaction_results <- lapply(names(outcomes), function(nm) {
  extract_interactions(race_interaction_models[[nm]], "race_collapsed") %>%
    mutate(outcome = nm)
})
race_interaction_results <- bind_rows(race_interaction_results)
race_interaction_results

# add maritial status

# Race strata are much smaller once split by 6 employment groups x
# 5 race categories - interpret stratum-specific PRs with caution.
race_covariates <- c("age_group", "sex", "marital", "children", "educ_group")
race_levels <- levels(working_age_data[[1]]$race_collapsed)

race_stratified_results <- lapply(names(outcomes), function(nm) {
  lapply(race_levels, function(r) {
    formula <- as.formula(paste(
      outcomes[[nm]], "~ employ_income +", paste(race_covariates, collapse = " + ")
    ))
    designs_r <- lapply(working_age_design, function(d) subset(d, race_collapsed == r))
    pooled <- tryCatch(fit_pooled(designs_r, formula), error = function(e) NULL)
    if (is.null(pooled)) return(NULL)
    extract_stratum_pr(pooled, r, nm)
  }) %>% bind_rows()
}) %>% bind_rows()

race_stratified_results

# 6. Effect Modification by YEAR (new)
# --- 6a. interaction test (screen) ---
year_interaction_models <- lapply(names(outcomes), function(nm) {
  formula <- as.formula(paste(
    outcomes[[nm]], "~ employ_income * year_strata + age_group + sex +",
    paste(base_covariates[!base_covariates %in% "sex"], collapse = " + ")
  ))
  fit_pooled(working_age_design, formula)
})
names(year_interaction_models) <- names(outcomes)

year_interaction_results <- lapply(names(outcomes), function(nm) {
  extract_interactions(year_interaction_models[[nm]], "year_strata") %>%
    mutate(outcome = nm)
})
year_interaction_results <- bind_rows(year_interaction_results)
year_interaction_results   # p-values for employ_income x year terms

# --- 6b. year-stratified PRs, for presentation ---
# Watch for wide CIs / convergence warnings once 2025 data is thin
# early in the year (partial-year collection), same caveat as race.
year_covariates <- c("age_group", "sex", "race_collapsed", "marital", "children", "educ_group")
year_levels <- levels(working_age_data[[1]]$year_strata)

year_stratified_results <- lapply(names(outcomes), function(nm) {
  lapply(year_levels, function(y) {
    formula <- as.formula(paste(
      outcomes[[nm]], "~ employ_income +", paste(year_covariates, collapse = " + ")
    ))
    designs_y <- lapply(working_age_design, function(d) subset(d, year_strata == y))
    pooled <- tryCatch(fit_pooled(designs_y, formula), error = function(e) NULL)
    if (is.null(pooled)) return(NULL)
    extract_stratum_pr(pooled, y, nm)
  }) %>% bind_rows()
}) %>% bind_rows()

year_stratified_results

# 7. Save everything
effect_modification_results <- list(
  age_interaction    = age_interaction_results,
  age_stratified     = age_stratified_results,
  sex_interaction    = sex_interaction_results,
  sex_stratified     = sex_stratified_results,
  race_interaction   = race_interaction_results,
  race_stratified    = race_stratified_results,
  year_interaction   = year_interaction_results,
  year_stratified    = year_stratified_results
)

saveRDS(working_age_data, "data/processed/working_age_data.rds")
saveRDS(effect_modification_results, "data/processed/effect_modification_results.rds")