# 03_IMPUTE.R
#
# multiple imputation using MICE
# looking into employment, income, health for 2023-2025
#
# IMPUTED VARIABLES:
#   1. EMPLOY1       Employment status
#   2. INCOME3       Household income
#   3. EDUCA         Education
#   4. MARITAL       Marital status
#   5. CHILDREN_CAT  Children in household (Yes/No)
#   6. SMOKING       Smoking status
#   7. ALCOHOL       Heavy drinking (Yes/No)
#
# NOT IMPUTED:(outcomes)
#   General health (GENHLTH)
#   Mental health (MENTHLTH)
#
# Current imputation notes:
#   - m = 5 imputations - NEED TO INCREASE TO 10
#   - maxit = 5 iterations


# 1. packages and import cleaned data
library(mice)
library(dplyr)

d <- readRDS("data/processed/02_clean.rds")
cat("# of Respondents:", nrow(d), "\n")


# 2. CREATE AUXILIARY HEALTH PREDICTORS
# Mental and physical health can help predict
# missing employment, income, and other covariates,
# but the health outcomes are NOT imputed.
#
# missing auxiliary values are temporarily filled with
# the observed median, while a separate missingness
# indicator records which values were originally missing.

aux <- function(x) {
  
  miss <- as.integer(is.na(x))
  
  fill <- median(x, na.rm = TRUE)
  
  if (!is.finite(fill)) {
    stop("An auxiliary variable has no observed values.")
  }
  
  list(
    value = replace(x, is.na(x), fill),
    missing = miss
  )
}

mental <- aux(d$MENTHLTH)
physical <- aux(d$PHYSHLTH)


# 3. PREPARE NEW IMPUTATION VALUES

# Children:
# 0 = No children
# 1 = One or more children
# NA = Missing
children_binary <- ifelse(
  is.na(d$CHILDREN),
  NA_integer_,
  as.integer(d$CHILDREN > 0)
)


# Smoking:
# 1 = Current smoker, every day
# 2 = Current smoker, some days
# 3 = Former smoker
# 4 = Never smoked
#
# Script 04 will combine 1 and 2 into Current.
smoking_original <- d$smoking_code


# Heavy drinking:
# 1 = No
# 2 = Yes
# NA = Missing
alcohol_original <- d$alcohol_code


# 4. BUILD MICE DATA
# Keep the same predictors,
# Age, sex, race, year, state, log survey weight,
# general health, mental health, and physical health.

mi <- data.frame(
  
  # ORIGINAL IMPUTATION TARGETS
  
  EMPLOY1 = factor(
    d$EMPLOY1,
    levels = 1:8
  ),
  
  INCOME3 = ordered(
    d$INCOME3,
    levels = 1:11
  ),
  
  EDUCA = ordered(
    d$EDUCA,
    levels = 1:6
  ),
  
  
  # ADDITIONAL IMPUTATION TARGETS
  
  MARITAL = factor(
    d$MARITAL,
    levels = 1:6
  ),
  
  CHILDREN_CAT = factor(
    children_binary,
    levels = 0:1
  ),
  
  SMOKING = factor(
    smoking_original,
    levels = 1:4
  ),
  
  ALCOHOL = factor(
    alcohol_original,
    levels = 1:2
  ),
  
  
  # OBSERVED HEALTH PREDICTOR
  
  health = factor(
    d$GENHLTH,
    levels = 1:5
  ),
  
  
  # DEMOGRAPHIC PREDICTORS
  
  age = d$age,
  
  sex = factor(d$sex),
  
  race = factor(d$race),
  
  year = factor(d$year),
  
  state = factor(d$state),
  
  
  # SURVEY WEIGHT PREDICTOR
  
  log_weight = d$log_weight,
  
  
  # AUXILIARY HEALTH PREDICTORS
  
  mental_days = mental$value,
  
  mental_missing = mental$missing,
  
  physical_days = physical$value,
  
  physical_missing = physical$missing
)


# 5. MISSING GEN HEALTH
# auxiliary predictor, do NOT impute.
# # give missing gen health its own category so respondents are
# not dropped from MICE

mi$health <- addNA(
  mi$health,
  ifany = TRUE
)


# 6. DEFINE IMPUTATION TARGETS

targets <- c(
  "EMPLOY1",
  "INCOME3",
  "EDUCA",
  "MARITAL",
  "CHILDREN_CAT",
  "SMOKING",
  "ALCOHOL"
)


# 7. CHECK MISSINGNESS PRE IMPUTATION

missing_counts <- colSums(
  is.na(mi[, targets, drop = FALSE])
)

missing_percent <- round(100 * missing_counts / nrow(mi), 2)

missing_summary <- data.frame(
  variable = targets,
  missing_n = as.integer(missing_counts),
  missing_percent = as.numeric(missing_percent)
)

print(missing_summary)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  missing_summary,
  "data/processed/03_missingness_before_imputation.csv",
  row.names = FALSE
)


# 8.CREATE MICE METHODS
methods <- mice::make.method(mi)
methods[] <- ""

# Unordered categorical variables: polyreg
methods["EMPLOY1"] <- "polyreg"
methods["MARITAL"] <- "polyreg"
methods["SMOKING"] <- "polyreg"

# Ordered categorical variables: polr
methods["INCOME3"] <- "polr"
methods["EDUCA"] <- "polr"

# Binary categorical variables: logreg
methods["CHILDREN_CAT"] <- "logreg"
methods["ALCOHOL"] <- "logreg"

# no missing observations
methods[targets[missing_counts == 0]] <- ""

# 9. CREATE PREDICTOR MATRIX
pred <- mice::make.predictorMatrix(mi)
pred[,] <- 0

# variables with active imputation method
active_targets <- targets[methods[targets] != ""]

for (target in active_targets) {
  
  pred[
    target,
    setdiff(names(mi), target)
  ] <- 1
  
}

# 10. IMPUTATION SETTINGS CHECK

# make sure neither health outcome is an imputation target.
stopifnot(
  methods["health"] == "",
  methods["mental_days"] == ""
)

# Confirm predictor-only variables are complete
predictor_only <- setdiff(
  names(mi),
  targets
)

stopifnot(
  all(
    colSums(
      is.na(mi[, predictor_only, drop = FALSE])
    ) == 0
  )
)

cat("\nImputation methods:\n")
print(methods[targets])


# 11. RUN MICE
# Takes a long time to run, prepare yourself!!

set.seed(12345)

imp <- mice(
  mi,
  m = 5,
  maxit = 5,
  method = methods,
  predictorMatrix = pred,
  seed = 20261012,
  printFlag = TRUE
)


# 12. CHECK IMPUTATION DIAGNOSTICS
#
cat("\nMICE summary:\n")
print(imp)

cat("\nLogged events:\n")
print(imp$loggedEvents)


# Check if missing values remain, using completed dataset #1

completed_check <- mice::complete(
  imp,
  action = 1
)

cat("\nMissing values after imputation:\n")

print(
  colSums(
    is.na(
      completed_check[, targets, drop = FALSE]
    )
  )
)


# 13. SAVE IMPUTATION
saveRDS(imp, "data/processed/03_mice.rds")

cat("\nImputation object saved.\n")


# 14. Check convergence through trace plots

plot(imp)
plot(imp, c("EMPLOY1"))

# 15. Script Complete Message

message("Script 03 complete.")
