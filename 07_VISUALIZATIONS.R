# 07_VISUALIZATIONS.R
# BRFSS 2024: Descriptive bar charts + forest plots

# load packages
library(dplyr)
library(ggplot2)
library(patchwork)

# ============================================================
# PART A: DESCRIPTIVE STACKED BAR CHARTS (pre-imputation)
# ============================================================

brfss_clean <- readRDS("data/processed/brfss_clean.rds")

income_order <- c("High Income WW", "Low Income WW", "WW Missing Income",
                  "High Income SE", "Low Income SE", "SE Missing Income",
                  "Unemployed", "Not in Labor Force")

educ_order <- c("High Educ WW", "Low Educ WW",
                "High Educ SE", "Low Educ SE",
                "Unemployed", "Not in Labor Force")

plot_income <- brfss_clean %>%
  filter(!is.na(employ_income)) %>%
  mutate(employ_income = factor(employ_income, levels = income_order))

plot_educ <- brfss_clean %>%
  filter(
    !is.na(employ_educ),
    !employ_educ %in% c("WW Missing Educ", "SE Missing Educ")
  ) %>%
  mutate(employ_educ = factor(employ_educ, levels = educ_order))

make_stacked_bar <- function(data, group_var, fill_var, fill_colors, title, legend_title) {
  data %>%
    filter(.data[[fill_var]] != "Missing") %>%
    count(.data[[group_var]], .data[[fill_var]]) %>%
    group_by(.data[[group_var]]) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = .data[[group_var]], y = pct, fill = .data[[fill_var]])) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = fill_colors) +
    labs(title = title, x = NULL, y = "Proportion (%)", fill = legend_title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

health_colors <- c("Good/Very Good/Excellent" = "seagreen", "Fair/Poor" = "indianred")
mental_colors <- c("0 days" = "seagreen", "1-13 days" = "chocolate2", "14+ days" = "indianred")
activity_colors <- c("Yes" = "seagreen", "No" = "indianred")
alcohol_colors <- c("Yes" = "steelblue2", "No" = "grey34")
smoking_colors <- c("Current Smoker" = "indianred", "Former Smoker" = "chocolate2", "Never Smoked" = "seagreen")

income_plots <- (
  make_stacked_bar(plot_income, "employ_income", "health_label", health_colors,
                   "General Health", "General Health") |
    make_stacked_bar(plot_income, "employ_income", "mental_cat", mental_colors,
                     "Poor Mental Health Days", "Mental Health Days")
) / (
  make_stacked_bar(plot_income, "employ_income", "exercise", activity_colors,
                   "Physical Activity", "Physical Activity") |
    make_stacked_bar(plot_income, "employ_income", "alcohol", alcohol_colors,
                     "Alcohol Consumption", "Alcohol")
) / (
  make_stacked_bar(plot_income, "employ_income", "smoking", smoking_colors,
                   "Smoking Status", "Smoked 100+ Cigarettes") | plot_spacer()
) +
  plot_annotation(
    title = "Health Outcomes by Employment and Income Group",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

ggsave("results/figures/income_plots.pdf", income_plots, width = 14, height = 12)
income_plots

education_plots <- (
  make_stacked_bar(plot_educ, "employ_educ", "health_label", health_colors,
                   "General Health", "General Health") |
    make_stacked_bar(plot_educ, "employ_educ", "mental_cat", mental_colors,
                     "Poor Mental Health Days", "Mental Health Days")
) / (
  make_stacked_bar(plot_educ, "employ_educ", "exercise", activity_colors,
                   "Physical Activity", "Physical Activity") |
    make_stacked_bar(plot_educ, "employ_educ", "alcohol", alcohol_colors,
                     "Alcohol Consumption", "Alcohol")
) / (
  make_stacked_bar(plot_educ, "employ_educ", "smoking", smoking_colors,
                   "Smoking Status", "Smoked 100+ Cigarettes") | plot_spacer()
) +
  plot_annotation(
    title = "Health Outcomes by Employment and Education Group",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

ggsave("results/figures/education_plots.pdf", education_plots, width = 14, height = 12)
education_plots

# ============================================================
# PART B: FOREST PLOTS - PRIMARY MODELS
#   Compares Imputed-Weighted / Imputed-Unweighted /
#   Complete Case-Weighted / Complete Case-Unweighted
# ============================================================

primary_results <- readRDS("data/processed/primary_results.rds")

group_order <- rev(c("Low Income WW", "High Income SE", "Low Income SE",
                     "Unemployed", "Not in Labor Force"))

model_order <- c("Complete Case, Unweighted", "Complete Case, Weighted",
                 "Imputed, Unweighted", "Imputed, Weighted")

make_forest_plot <- function(results, outcome_name, plot_title) {
  df <- results %>%
    filter(outcome == outcome_name) %>%
    mutate(
      exposure = factor(exposure, levels = group_order),
      model = factor(model, levels = model_order),
      CI_low = as.numeric(sub("\u2013.*", "", CI)),
      CI_high = as.numeric(sub(".*\u2013", "", CI))
    )
  
  ggplot(df, aes(x = PR, y = exposure, color = model, shape = model)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2,
                   position = position_dodge(width = 0.6), linewidth = 0.7) +
    geom_point(size = 2.8, position = position_dodge(width = 0.6)) +
    scale_x_log10() +
    labs(
      title = plot_title,
      subtitle = "Reference: High Income Wage Workers",
      x = "Prevalence Ratio", y = NULL, color = "Model", shape = "Model"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )
}

plot_health_forest <- make_forest_plot(primary_results, "health", "Prevalence Ratios for Fair/Poor Health")
plot_mental_forest <- make_forest_plot(primary_results, "mental", "Prevalence Ratios for Poor Mental Health")

ggsave("results/figures/forest_health.png", plot_health_forest, width = 10, height = 5.5, dpi = 300)
ggsave("results/figures/forest_mental.png", plot_mental_forest, width = 10, height = 5.5, dpi = 300)

plot_health_forest
plot_mental_forest

# ============================================================
# PART C: FOREST PLOTS - EFFECT MODIFICATION (stratified PRs)
# ============================================================

stratified_table <- read.csv("results/effect_modification_stratified_prs.csv")

make_stratified_forest <- function(data, modifier_name, outcome_name, plot_title) {
  df <- data %>%
    filter(modifier == modifier_name, outcome == outcome_name) %>%
    mutate(group = factor(group, levels = rev(unique(group))))
  
  ggplot(df, aes(x = PR, y = group, color = stratum, shape = stratum)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2,
                   position = position_dodge(width = 0.6), linewidth = 0.7) +
    geom_point(size = 2.8, position = position_dodge(width = 0.6)) +
    scale_x_log10() +
    labs(
      title = plot_title,
      subtitle = paste0("Stratified by ", modifier_name, " | Reference: High Income Wage Workers"),
      x = "Prevalence Ratio", y = NULL, color = modifier_name, shape = modifier_name
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )
}

for (mod in c("Age", "Sex", "Race/Ethnicity")) {
  for (out in c("health", "mental")) {
    p <- make_stratified_forest(
      stratified_table, mod, out,
      paste0("Prevalence Ratios for ",
             ifelse(out == "health", "Fair/Poor Health", "Poor Mental Health"))
    )
    fname <- paste0("results/figures/forest_", out, "_by_", tolower(gsub("/", "", mod)), ".png")
    ggsave(fname, p, width = 8, height = 5.5, dpi = 300)
    print(p)
  }
}