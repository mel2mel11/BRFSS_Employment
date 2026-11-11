# 06_RESULTS.R
#BRFSS 2023-25: Results extraction and summary tables

# load packages
library(dplyr)
library(purrr)
library(gt)

# read in data
weighted_models   <- readRDS("data/processed/weighted_models.rds")
unweighted_models <- readRDS("data/processed/unweighted_models.rds")
cc_models         <- readRDS("data/processed/complete_case_models.rds")
effect_results    <- readRDS("data/processed/effect_modification_results.rds")

exposure_groups <- c(
  "Low Income WW", "High Income SE", "Low Income SE",
  "Unemployed", "Not in Labor Force"
)

# 1. extract employ_income terms from pooled model
extract_model_results <- function(model, outcome_name, model_type) {
  summary(model, conf.int = TRUE, exponentiate = TRUE) %>%
    as.data.frame() %>%
    filter(term %in% paste0("employ_income", exposure_groups)) %>%
    mutate(
      outcome  = outcome_name,
      model    = model_type,
      exposure = sub("employ_income", "", term),
      PR       = round(estimate, 2),
      CI       = paste0(round(`2.5 %`, 2), "\u2013", round(`97.5 %`, 2)),
      p_value  = round(p.value, 3)
    ) %>%
    select(outcome, model, exposure, PR, CI, p_value)
}


# 2. extract employ_income terms from non-pooled single survey object
extract_single_model_results <- function(model, outcome_name, model_type) {
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term %in% paste0("employ_income", exposure_groups)) %>%
    mutate(
      outcome  = outcome_name,
      model    = model_type,
      exposure = sub("employ_income", "", term),
      PR       = round(estimate, 2),
      CI       = paste0(round(conf.low, 2), "\u2013", round(conf.high, 2)),
      p_value  = round(p.value, 3)
    ) %>%
    select(outcome, model, exposure, PR, CI, p_value)
}

# 3. Primary (Imputed) Results - weighted and unweighted

weighted_results <- map2_dfr(
  weighted_models, names(weighted_models),
  ~ extract_model_results(.x, .y, "Imputed, Weighted")
)

unweighted_results <- map2_dfr(
  unweighted_models, names(unweighted_models),
  ~ extract_model_results(.x, .y, "Imputed, Unweighted")
)

# 4. Sensitivity: Complete-Case, Unimputed Analysis
cc_weighted_results <- map2_dfr(
  cc_models, names(cc_models),
  ~ extract_single_model_results(.x$weighted, .y, "Complete Case, Weighted")
)

cc_unweighted_results <- map2_dfr(
  cc_models, names(cc_models),
  ~ extract_single_model_results(.x$unweighted, .y, "Complete Case, Unweighted")
)

# 5. Combine Primary-model results
primary_results <- bind_rows(
  weighted_results, unweighted_results,
  cc_weighted_results, cc_unweighted_results
) %>%
  arrange(outcome, exposure, model)

primary_results

# 6. Save Primary results
dir.create("results", showWarnings = FALSE, recursive = TRUE)

saveRDS(primary_results, "data/processed/primary_results.rds")
write.csv(primary_results, "results/primary_model_results.csv", row.names = FALSE)

primary_table_gt <- primary_results %>%
  gt() %>%
  cols_label(
    outcome = "Outcome", model = "Model", exposure = "Exposure Group",
    PR = "PR", CI = "95% CI", p_value = "P-value"
  ) %>%
  tab_header(
    title = "Associations Between Employment-Income Group and Health Outcomes",
    subtitle = "Reference group: High Income Wage Workers"
  )

gtsave(primary_table_gt, "results/primary_model_results.html")

# 7. Effect modification Results
# from script 05
effect_results$age_interaction
effect_results$age_stratified
effect_results$sex_interaction
effect_results$sex_stratified
effect_results$race_interaction
effect_results$race_stratified

interaction_table <- bind_rows(
  effect_results$age_interaction  %>% mutate(modifier = "Age"),
  effect_results$sex_interaction  %>% mutate(modifier = "Sex"),
  effect_results$race_interaction %>% mutate(modifier = "Race/Ethnicity")
)

write.csv(interaction_table, "results/effect_modification_interaction_tests.csv", row.names = FALSE)

stratified_table <- bind_rows(
  effect_results$age_stratified  %>% mutate(modifier = "Age"),
  effect_results$sex_stratified  %>% mutate(modifier = "Sex"),
  effect_results$race_stratified %>% mutate(modifier = "Race/Ethnicity")
)

write.csv(stratified_table, "results/effect_modification_stratified_prs.csv", row.names = FALSE)

stratified_table_gt <- stratified_table %>%
  gt() %>%
  cols_label(
    outcome = "Outcome", modifier = "Modifier", stratum = "Stratum",
    group = "Exposure Group", PR = "PR",
    CI_low = "95% CI Low", CI_high = "95% CI High", p_value = "P-value"
  ) %>%
  tab_header(
    title = "Employment-Income Associations Stratified by Age, Sex, and Race",
    subtitle = "Working-age adults (18-64); reference group: High Income Wage Workers"
  )

gtsave(stratified_table_gt, "results/effect_modification_stratified_prs.html")
