# 02_TABLE1.R
# BRFSS 2023-2025: Descriptive Table 1
#
# Population 18-64, working class only
# RUN AFTER IMPUTATION
#
# Produces:
# TABLE 1: PRE_IMPUATION UNWEIGHTED
#   Built from brfss_clean (before imputation). "WW Missing Income"
#   and "SE Missing Income" are kept as their OWN columns here on
#   purpose - this table is showing what was actually observed,
#   missingness included, not papering over it.
# 
# TABLE 2: POST-IMPUTATION, UNWEIGHTED N
#   Built from ONE completed dataset (completed_data[[1]]). No more
#   "Missing Income" columns - imputation resolved them into
#   High/Low. NOTE: Rubin's pooling rules are for point estimates/SEs
#   out of a model (like 04_MODELS.R), not really an "average a
#   crosstab across 5 datasets" operation with an agreed-upon
#   standard - using a single completed dataset for a DESCRIPTIVE
#   table is common practice post-MI. Flag if you want it done
#   differently (e.g., averaging cell %s across all 5 imputations).
#
# TABLE 3: POST-IMPUTATION, WEIGHTED N
#   Same completed_data[[1]] as Table 2, but using the survey design
#   (weight_adj) so percentages reflect the population-representative
#   picture rather than the raw sample.

# Load Packages
library(dplyr)
library(gtsummary)
library(gt)
library(survey)

options(survey.lonely.psu = "adjust")

# Load data
brfss_clean <- readRDS("data/processed/brfss_clean.rds")
completed_data <- readRDS("data/processed/completed_data.rds")

n_years  <- n_distinct(brfss_clean$year)
year_rng <- paste(range(as.character(brfss_clean$year)), collapse = "-")
fmt      <- function(x) formatC(x, format = "d", big.mark = ",")

# 1. SAMPLE FLOW (pre-imputation) - every exclusion is counted
n_all_ages <- nrow(brfss_clean)
n_65plus   <- sum(brfss_clean$age_group == "65+", na.rm = TRUE)
n_age_miss <- sum(is.na(brfss_clean$age_group))

brfss_working <- brfss_clean %>% filter(!is.na(age_strata))   # 18-64
n_total <- nrow(brfss_working)

# Table 1A population: 18-64 with valid employment status
table1A_data <- brfss_working %>%
  filter(!is.na(employ_income)) %>%
  mutate(age_group = droplevels(age_group))   # drops the empty "65+" level
n_excl_employ <- n_total - nrow(table1A_data)
n_ww_missinc  <- sum(table1A_data$employ_income == "WW Missing Income")
n_se_missinc  <- sum(table1A_data$employ_income == "SE Missing Income")

cat("\nSample flow:\n",
    "  All respondents:                ", fmt(n_all_ages), "\n",
    "  Excluded, aged 65+:             ", fmt(n_65plus), "\n",
    "  Excluded, missing age:          ", fmt(n_age_miss), "\n",
    "  Working-age (18-64):            ", fmt(n_total), "\n",
    "  Excluded, missing employment:   ", fmt(n_excl_employ), "\n",
    "  Table 1A N:                     ", fmt(nrow(table1A_data)), "\n")


# 2. Post-imputation data: add the labeled versions of each variable
# completed_data only has the numeric codes / analysis variables from 03, so
# the labeled factors used in Table 1 are rebuilt here with the SAME labels
# and level order as 01_CLEAN.R.
prep_imputed <- function(df, n_years) {
  df %>%
    mutate(across(c(`_AGEG5YR`, `_IMPRACE`, GENHLTH, `_MENT14D`,
                    `_TOTINDA`, `_RFDRHV9`, `_SMOKER3`),
                  ~ as.numeric(as.character(.x)))) %>%
    filter(`_AGEG5YR` %in% 1:9) %>%      # works whether or not 03 already filtered
    mutate(
      weight_adj = `_LLCPWT` / n_years,  # not carried through 03, rebuilt here
      
      age_group = factor(
        case_when(
          `_AGEG5YR` %in% c(1, 2)       ~ "18-29",
          `_AGEG5YR` %in% c(3, 4, 5)    ~ "30-44",
          `_AGEG5YR` %in% c(6, 7, 8, 9) ~ "45-64"
        ),
        levels = c("18-29", "30-44", "45-64")
      ),
      
      # full 6-level race (same as Table 1A); _IMPRACE is complete (CDC-imputed)
      race = factor(
        case_when(
          `_IMPRACE` == 1 ~ "NH White",
          `_IMPRACE` == 2 ~ "NH Black",
          `_IMPRACE` == 3 ~ "NH Asian",
          `_IMPRACE` == 4 ~ "NH AI/AN",
          `_IMPRACE` == 5 ~ "Hispanic",
          `_IMPRACE` == 6 ~ "Other/Multiracial"
        ),
        levels = c("NH White", "NH Black", "NH Asian", "NH AI/AN",
                   "Hispanic", "Other/Multiracial")
      ),
      
      health_label = factor(
        case_when(
          GENHLTH %in% c(1, 2, 3) ~ "Good/Very Good/Excellent",
          GENHLTH %in% c(4, 5)    ~ "Fair/Poor"
        ),
        levels = c("Good/Very Good/Excellent", "Fair/Poor")
      ),
      
      mental_cat = factor(
        case_when(
          `_MENT14D` == 1 ~ "0 days",
          `_MENT14D` == 2 ~ "1-13 days",
          `_MENT14D` == 3 ~ "14+ days"
        ),
        levels = c("0 days", "1-13 days", "14+ days")
      ),
      
      exercise = factor(
        case_when(`_TOTINDA` == 1 ~ "Yes", `_TOTINDA` == 2 ~ "No"),
        levels = c("Yes", "No")
      ),
      
      alcohol = factor(
        case_when(`_RFDRHV9` == 1 ~ "No", `_RFDRHV9` == 2 ~ "Yes"),
        levels = c("No", "Yes")
      ),
      
      smoking = factor(
        case_when(
          `_SMOKER3` %in% c(1, 2) ~ "Current Smoker",
          `_SMOKER3` == 3         ~ "Former Smoker",
          `_SMOKER3` == 4         ~ "Never Smoked"
        ),
        levels = c("Current Smoker", "Former Smoker", "Never Smoked")
      ),
      
      # 03 releveled these for modeling (reference first); put them back in
      # the same display order as Table 1A
      income_group = factor(as.character(income_group),
                            levels = c("Low (<$50k)", "High ($50k+)")),
      educ_group   = factor(as.character(educ_group),
                            levels = c("HS or Less", "Some College", "College+")),
      employ_income = factor(
        as.character(employ_income),
        levels = c("High Income WW", "Low Income WW",
                   "High Income SE", "Low Income SE",
                   "Unemployed", "Not in Labor Force")
      )
    )
}

imputed_working <- prep_imputed(completed_data[[1]], n_years)

# variables shown in every version of the table
var_table <- c("sex", "age_group", "race", "marital", "children",
               "income_group", "educ_group", "health_label",
               "mental_cat", "exercise", "alcohol", "smoking")

var_labels <- list(
  sex ~ "Sex",
  age_group ~ "Age Group",
  race ~ "Race/Ethnicity",
  marital ~ "Marital Status",
  children ~ "Children in Household",
  income_group ~ "Household Income",
  educ_group ~ "Education",
  health_label ~ "General Health",
  mental_cat ~ "Poor Mental Health Days",
  exercise ~ "Physical Activity",
  alcohol ~ "Heavy Drinkers",
  smoking ~ "Smoker Status"
)

# Checks: imputed data should be complete and the same size as the 18-64 sample
na_counts <- colSums(is.na(imputed_working[, c("employ_income", var_table)]))
if (any(na_counts > 0)) {
  warning("completed_data still has NAs in table variables (they will show as ",
          "'Unknown' rows). The imputation is incomplete - re-run 03.\n",
          paste(names(na_counts[na_counts > 0]), na_counts[na_counts > 0],
                sep = " = ", collapse = ", "))
}
if (nrow(imputed_working) != n_total) {
  warning("Imputed N (", fmt(nrow(imputed_working)), ") does not match the ",
          "18-64 sample in brfss_clean (", fmt(n_total), "). Check that 03 ",
          "was run on the same brfss_clean.rds.")
}


# 3. FOOTNOTE TEXT
# Plain text (no "$"): see formatting notes at the top.
defs <- paste(
  "Household income: Low = under 50,000 USD (_INCOMG1 codes 1-4); High =",
  "50,000 USD or more (codes 5-7).",
  "Education: HS or Less = did not graduate high school or high school",
  "graduate only (_EDUCAG 1-2); Some College = attended college or technical",
  "school (3); College+ = graduated college or technical school (4).",
  "Employment x income groups: wage workers (WW) and self-employed (SE) are",
  "split into High vs Low using the income cutoffs above; Unemployed = out",
  "of work for less than 1 year or 1+ years (EMPLOY1 3-4); Not in Labor",
  "Force = homemaker, student, retired, or unable to work (EMPLOY1 5-8).",
  "General Health: Good/Very Good/Excellent vs Fair/Poor (GENHLTH).",
  "Poor Mental Health Days: number of days in the past 30 that mental health",
  "was not good (_MENT14D).",
  "Physical Activity: any leisure-time physical activity in the past 30 days",
  "(_TOTINDA).",
  "Heavy Drinkers: heavy drinking by BRFSS sex-specific thresholds (_RFDRHV9,",
  "standardized across years in 00_IMPORT_COMBINE.R).",
  "Smoker Status: Current = smokes every day or some days; Former = smoked",
  "100+ cigarettes but quit; Never = fewer than 100 cigarettes in lifetime",
  "(_SMOKER3)."
)

notes_1A <- c(
  paste0(
    "BRFSS ", year_rng, ", pooled. Of ", fmt(n_all_ages),
    " total respondents, ", fmt(n_65plus), " aged 65+, leaving ",
    fmt(n_total), " working-age (18-64) respondents. A further ",
    fmt(n_excl_employ), " with missing or refused employment status ",
    "(EMPLOY1 = 9) were excluded, leaving the ",
    fmt(nrow(table1A_data)), " respondents shown here."
  ),
  paste0(
    "Respondents with valid employment status but missing or refused ",
    "household income are NOT dropped: they are shown as their own columns, ",
    "'WW Missing Income' (wage workers, n = ", fmt(n_ww_missinc), ") and ",
    "'SE Missing Income' (self-employed, n = ", fmt(n_se_missinc), "). ",
    "Tables 1B and 1C use imputed income instead. Missing values on other ",
    "characteristics are shown as 'Missing' rows. Values are unweighted ",
    "counts and column percentages. Sex, age, and race have no missing ",
    "values because CDC/BRFSS imputes them."
  ),
  defs
)

notes_1B <- c(
  paste0(
    "BRFSS ", year_rng, ", pooled; working-age (18-64) respondents, n = ",
    fmt(nrow(imputed_working)), ". Built from one of the 5 imputed datasets ",
    "(imputation #1); a single completed dataset is used for this ",
    "descriptive table. Missing employment status, household income, ",
    "education, marital status, children, and the health/behavior ",
    "variables were multiply imputed (mice); sex, age, and race were not ",
    "imputed (complete from CDC/BRFSS). No missing categories remain, so ",
    "every respondent is assigned to a High or Low income group."
  ),
  paste0(
    "N is larger than Table 1A (", fmt(nrow(table1A_data)), ") because ",
    "employment status was also imputed, so the ", fmt(n_excl_employ),
    " respondents with missing employment status that were excluded from ",
    "Table 1A are included here. Values are unweighted counts and column ",
    "percentages."
  ),
  defs
)

unw_n <- imputed_working %>% count(employ_income)
unw_txt <- paste0(unw_n$employ_income, " = ", fmt(unw_n$n), collapse = "; ")

notes_1C <- c(
  paste0(
    "BRFSS ", year_rng, ", pooled; working-age (18-64) respondents. Same ",
    "completed dataset as Table 1B (imputation #1). Cell values are ",
    "survey-weighted n (%) using weight_adj (_LLCPWT divided by ", n_years,
    " for pooling ", n_years, " years, per CDC guidance), so counts and ",
    "the N in each column header are weighted and represent the average ",
    "annual working-age US adult population, not the sample size."
  ),
  paste0(
    "Unweighted analytic sample = ", fmt(nrow(imputed_working)),
    " (by group: ", unw_txt, ")."
  ),
  defs
)


# 4. BUILD THE TABLES

# TABLE 1A: pre-imputation, unweighted
table1A <- table1A_data %>%
  select(employ_income, all_of(var_table)) %>%
  tbl_summary(
    by = employ_income,
    missing = "ifany",
    missing_text = "Missing",
    label = var_labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Employment and Income Group**"
  )

# TABLE 1B: post-imputation, unweighted
table1B <- imputed_working %>%
  select(employ_income, all_of(var_table)) %>%
  tbl_summary(
    by = employ_income,
    missing = "ifany",      # should be none; any leftover NA shows up as "Unknown"
    label = var_labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Employment and Income Group**"
  )

# TABLE 1C: post-imputation, survey-weighted
design_imputed <- svydesign(
  id = ~`_PSU`, strata = ~`_STSTR`, weights = ~weight_adj,
  data = imputed_working, nest = TRUE
)

table1C <- design_imputed %>%
  tbl_svysummary(
    by = employ_income,
    include = all_of(var_table),
    missing = "ifany",
    label = var_labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)")
    # {n} and {p} are WEIGHTED; column header N is the weighted N.
    # Unweighted n per group is given in the footnote.
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Employment and Income Group**"
  )


# 5. FORMAT (compact, fixed width, notes wrap inside the table)
# 5. FORMAT (compact, fixed width, notes wrap inside the table)
finish_gt <- function(tbl, title, notes, label_w = 190, stat_w = 105) {
  n_stat <- sum(grepl("^stat_", names(tbl$table_body)))
  
  g <- tbl %>%
    as_gt() %>%
    gt::tab_header(title = gt::md(paste0("**", title, "**"))) %>%
    gt::opt_align_table_header(align = "left") %>%
    # widths are written into the formulas as literal numbers: cols_width()
    # evaluates the right-hand side outside this function, so it can't see
    # the variables label_w / stat_w
    gt::cols_width(
      stats::as.formula(paste0("label ~ gt::px(", label_w, ")")),
      stats::as.formula(paste0("starts_with('stat_') ~ gt::px(", stat_w, ")"))
    ) %>%
    gt::tab_options(
      table.width = gt::px(label_w + n_stat * stat_w),
      table.font.names = c("Arial", "Helvetica", "sans-serif"),
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(15),
      column_labels.font.size = gt::px(12),
      data_row.padding = gt::px(2.5),
      source_notes.font.size = gt::px(10.5),
      source_notes.padding = gt::px(3)
    )
  
  for (nt in notes) g <- gt::tab_source_note(g, source_note = nt)
  g
}

table1A_gt <- finish_gt(
  table1A,
  "Table 1A. Sociodemographic Characteristics by Employment and Income Group, Pre-Imputation (Unweighted), n (%)",
  notes_1A, stat_w = 100          # 9 stat columns (incl. 2 missing-income columns)
)

table1B_gt <- finish_gt(
  table1B,
  "Table 1B. Sociodemographic Characteristics by Employment and Income Group, Post-Imputation (Unweighted), n (%)",
  notes_1B, stat_w = 115
)

if (have_survey) table1C_gt <- finish_gt(
  table1C,
  "Table 1C. Sociodemographic Characteristics by Employment and Income Group, Post-Imputation (Survey-Weighted), n (%)",
  notes_1C, stat_w = 125          # weighted counts are long numbers
)

# View
table1A_gt
table1B_gt
table1C_gt

# 6. SAVE
gt::gtsave(table1A_gt, "outputs/table1A_preimputation_unweighted.html")
gt::gtsave(table1B_gt, "outputs/table1B_postimputation_unweighted.html")
gt::gtsave(table1C_gt, "outputs/table1C_postimputation_weighted.html")

# Optional PNGs (needs the webshot2 package + Chrome installed):
# gt::gtsave(table1A_gt, "outputs/table1A_preimputation_unweighted.png", vwidth = 1300)
# gt::gtsave(table1B_gt, "outputs/table1B_postimputation_unweighted.png", vwidth = 1300)
# gt::gtsave(table1C_gt, "outputs/table1C_postimputation_weighted.png", vwidth = 1300)