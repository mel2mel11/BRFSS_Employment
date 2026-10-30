# 06_MODELS.R
#
# Primary Analysis ONLY:
#   Imputed & Weighted
#
# Model 1: Minimally adjusted
#   Age, sex, race, marital status, year
#
# Model 2: Fully adjusted
#   Age, sex, race, marital status, year, smoking, drinking
#
# Outcomes:
#   1. Poor general health
#   2. Poor mental health (14+ days)
#
# Method:
#   Weighted Poisson regression
#   Pool all m=5 (for now) imputations
#
# Reference group:
#   High-income wage worker (PR = 1)


# 1. Load

library(dplyr)
library(survey)
library(mice)
library(tidyr)
library(purrr)

options(survey.lonely.psu = "adjust")

imputed <- readRDS("data/processed/04_imputed_datasets.rds")

stopifnot(is.list(imputed), length(imputed) == 5) # 5 for now

cat("\n# completed datasets:", length(imputed), "\n"
)

# 2. Define Outcomes and Exposure

outcomes <- c(
  health = "poor_general_health",
  mental = "poor_mental_health_14d"
)

exposure <- "employment_income"

exposure_levels <- c(
  "High-income wage worker",
  "Low-income wage worker",
  "High-income self-employed",
  "Low-income self-employed",
  "Unemployed",
  "Not in labor force"
)


# 3. Create Model Sets

# Model 1: Minimally adjusted
covariates_model1 <- c(
  "age_model",
  "sex_group",
  "race_group",
  "marital_group",
  "year_group"
)

# Model 2: Fully adjusted
covariates_model2 <- c(
  covariates_model1,
  "smoking_group",
  "alcohol_group"
)


adjustment_sets <- list(model1 = covariates_model1, model2 = covariates_model2)

# 4. Make sure each has the required varaibles 
needed <- c(
  exposure,
  covariates_model2,
  unname(outcomes),
  "psu_year",
  "strata_year",
  "weight_3yr"
)

for (i in seq_along(imputed)) {
  
  missing_cols <- setdiff(
    needed,
    names(imputed[[i]])
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Dataset ", i,
      " is missing variables: ",
      paste(missing_cols, collapse = ", ")
    )
  }
}


# 5. Set Reference Categories
prepare_model_data <- function(df) {
  
  df %>%
    mutate(
      
      employment_income = factor(employment_income, levels = exposure_levels),
      
      age_model = relevel(factor(age_model), ref = "18-24"),
      
      sex_group = relevel(factor(sex_group), ref = "Male"),
      
      race_group = relevel(factor(race_group), ref = "NH White"),
      
      marital_group = relevel(factor(marital_group), ref = "Married/Partnered"),
      
      year_group = relevel(factor(year_group), ref = "2023"),
      
      smoking_group = relevel(factor(smoking_group), ref = "Never"),
      
      alcohol_group = relevel(factor(alcohol_group), ref = "No")
    )
}

imputed <- lapply(imputed, prepare_model_data)



# 6. Verify Imputed Covarities

for (i in seq_along(imputed)) {

  df <- imputed[[i]]
  
  cat("\nChecking imputed dataset", i, "\n")
  
  print(
    colSums(
      is.na(
        df[, c(
          "employment_income",
          covariates_model2
        ), drop = FALSE]
      )
    )
  )
}

# 7. Survey Design

make_design <- function(df) {
  
  svydesign(
    ids = ~psu_year,
    strata = ~strata_year,
    weights = ~weight_3yr,
    data = df,
    nest = TRUE
  )
}


# 8. Model formula function
make_formula <- function(outcome, covariates) {
  
  reformulate(
    c(exposure, covariates),
    response = outcome
  )
}


# 9. Fit models across all 5 (for now) Imputations

fit_imputed <- function(outcome, covariates) {
  
  f <- make_formula(
    outcome,
    covariates
  )
  
  fits <- lapply(
    seq_along(imputed),
    function(i) {
      
      df <- imputed[[i]]
      
      # Exclude missing outcomes
      
      vars <- all.vars(f)
      
      df <- df[
        complete.cases(
          df[, vars, drop = FALSE]
        ),
        ,
        drop = FALSE
      ]
      
      # survey design
      
      design <- make_design(df)
      
      fit <- svyglm(
        f,
        design = design,
        family = quasipoisson(link = "log")
      )
      
      fit
    }
  )
  
  # POOL COEFICCIENTS AND VARIENCES
  
  pooled <- mice::pool(
    mice::as.mira(fits)
  )
  
  pooled
}


# 10. Run 4 Primary Models
models <- list()

for (outcome_name in names(outcomes)) {
  
  outcome <- outcomes[[outcome_name]]
  
  models[[outcome_name]] <- list()
  
  for (model_name in names(adjustment_sets)) {
    
    covariates <- adjustment_sets[[model_name]]
    
    message(
      "\nRunning ",
      outcome_name,
      " / ",
      model_name
    )
    
    models[[outcome_name]][[model_name]] <-
      fit_imputed(
        outcome,
        covariates
      )
    
    message(
      "Completed ",
      outcome_name,
      " / ",
      model_name
    )
  }
}


# 11. Extract PRs
extract_pr <- function(
    pooled,
    outcome_name,
    model_name
) {
  
  s <- summary(
    pooled,
    conf.int = TRUE,
    exponentiate = TRUE
  )
  
  tibble(
    outcome = outcome_name,
    model = model_name,
    term = s$term,
    PR = s$estimate,
    CI_low = s$`2.5 %`,
    CI_high = s$`97.5 %`,
    p_value = s$p.value
  )
}


# 12. Combine model results

results <- list()

for (outcome_name in names(models)) {
  
  for (model_name in names(models[[outcome_name]])) {
    
    results[[paste(
      outcome_name,
      model_name,
      sep = "_"
    )]] <- extract_pr(
      models[[outcome_name]][[model_name]],
      outcome_name,
      model_name
    )
  }
}

results_table <- bind_rows(results)


# 13. reference group

reference_rows <- expand.grid(
  outcome = names(outcomes),
  model = names(adjustment_sets),
  stringsAsFactors = FALSE
) %>%
  mutate(
    term = "employment_incomeHigh-income wage worker",
    PR = 1,
    CI_low = 1,
    CI_high = 1,
    p_value = NA_real_
  )

results_table <- bind_rows(
  results_table,
  reference_rows
)


# 14. Format PRs into a table

results_table <- results_table %>%
  mutate(
    PR_CI = sprintf(
      "%.2f (%.2f-%.2f)",
      PR,
      CI_low,
      CI_high
    )
  )


# 15. Save Results

saveRDS(models, "data/processed/primary_models.rds")

write.csv(results_table, 
          "results/tables/primary_prevalence_ratios.csv", row.names = FALSE)

saveRDS(results_table, "data/processed/primary_prevalence_ratios.rds")


# 16. Print main results
print("PRIMARY PREVALENCE RATIOS\n")

print(
  results_table %>%
    filter(
      grepl("^employment_income", term)
    ) %>%
    select(outcome, model, term, PR_CI, p_value)
)

# Final Message
message("Script 06 complete.")
