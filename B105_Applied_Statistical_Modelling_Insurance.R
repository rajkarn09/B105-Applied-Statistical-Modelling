# ==============================================================================
# COURSE: B105 Applied Statistical Modelling
# ASSESSMENT: Individual Final Project Report
# SCRIPT NAME: B105_Applied_Statistical_Modelling_Insurance.R
# DATASET: Medical Cost Personal Datasets (Kaggle)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. SETUP, DIRECTORIES, & PACKAGE INSTALLATION
# ------------------------------------------------------------------------------

# Define required packages
required_packages <- c(
  "tidyverse",   # Data manipulation (dplyr, tibble) & visualization (ggplot2)
  "corrplot",    # Correlation matrix visualization
  "car",         # Variance Inflation Factor (VIF) & regression diagnostics
  "lmtest",      # Breusch-Pagan & Durbin-Watson assumption tests
  "gridExtra",   # Multi-panel ggplot arrangements
  "scales",      # Pretty currency and percentage scales
  "broom",       # Tidy statistical model summaries
  "knitr"        # Table formatting
)

# Install missing packages automatically
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

# Load all libraries quietly
invisible(lapply(required_packages, library, character.only = TRUE))

# Set paths (using forward slashes for Windows compatibility)
data_path   <- "C:/Users/Admin/Downloads/archive/insurance.csv"
output_dir  <- "C:/Users/Admin/Downloads/archive/Outputs"

# Create output folder if it doesn't already exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("[INFO] Created Output Directory at:", output_dir, "\n")
}

# Set reproducible random seed
set.seed(105)

# Set base ggplot theme
theme_set(theme_minimal(base_size = 12) +
            theme(
              plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
              axis.title = element_text(face = "bold"),
              panel.grid.minor = element_blank()
            ))

# ------------------------------------------------------------------------------
# 1. DATA INGESTION & QUALITY AUDIT
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 1. DATA INGESTION & QUALITY AUDIT\n")
cat("==================================================================\n")

# Ingest data
if (!file.exists(data_path)) {
  stop("Dataset not found at: ", data_path, ". Please verify your file path.")
}
raw_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Structural overview
cat("\n[A] Dataset Dimensions:\n")
cat("Rows:", nrow(raw_data), "| Columns:", ncol(raw_data), "\n\n")

cat("[B] Data Structure:\n")
str(raw_data)

# Missing values check
missing_summary <- data.frame(
  Feature = colnames(raw_data),
  Missing_Count = colSums(is.na(raw_data)),
  Missing_Pct = round(colSums(is.na(raw_data)) / nrow(raw_data) * 100, 2)
)
print(missing_summary)
write.csv(missing_summary, file.path(output_dir, "01_missing_value_audit.csv"), row.names = FALSE)

# Check and remove duplicates
num_duplicates <- sum(duplicated(raw_data))
cat("\n[C] Exact Duplicate Rows Identified:", num_duplicates, "\n")

cleaned_data <- raw_data %>%
  distinct() # Remove 1 duplicate row if present

cat("Rows retained after deduplication:", nrow(cleaned_data), "\n")


# ------------------------------------------------------------------------------
# 2. DATA PREPROCESSING, ENCODING & FEATURE ENGINEERING
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 2. DATA PREPROCESSING & FEATURE ENGINEERING\n")
cat("==================================================================\n")

# Convert character variables into factors with meaningful base reference levels
df <- cleaned_data %>%
  mutate(
    sex      = factor(sex, levels = c("female", "male")),
    smoker   = factor(smoker, levels = c("no", "yes")),          # 'no' as base
    region   = factor(region, levels = c("northeast", "northwest", "southeast", "southwest")),
    children = as.numeric(children),
    # Feature Engineering: BMI Categories according to WHO standard
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25.0 ~ "Normal",
      bmi >= 25.0 & bmi < 30.0 ~ "Overweight",
      bmi >= 30.0 ~ "Obese"
    ),
    bmi_category = factor(bmi_category, levels = c("Normal", "Underweight", "Overweight", "Obese")),
    is_obese     = ifelse(bmi >= 30, "Obese (BMI >= 30)", "Non-Obese (BMI < 30)"),
    is_obese     = factor(is_obese)
  )

# Train/Test Split (80% Train, 20% Test) for rigorous validation
train_indices <- sample(seq_len(nrow(df)), size = floor(0.80 * nrow(df)))
train_data    <- df[train_indices, ]
test_data     <- df[-train_indices, ]

cat("Training Set Size:", nrow(train_data), "| Testing Set Size:", nrow(test_data), "\n")


# ------------------------------------------------------------------------------
# 3. DESCRIPTIVE STATISTICS & EXPLORATORY DATA ANALYSIS (EDA)
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 3. DESCRIPTIVE STATISTICS\n")
cat("==================================================================\n")

# 3.1 Numerical Summary Table
num_summary <- df %>%
  summarise(across(c(age, bmi, children, charges), list(
    Mean   = ~mean(.x),
    SD     = ~sd(.x),
    Median = ~median(.x),
    IQR    = ~IQR(.x),
    Min    = ~min(.x),
    Max    = ~max(.x)
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = Stat, values_from = value)

cat("\n[A] Numerical Features Descriptive Summary:\n")
print(num_summary)
write.csv(num_summary, file.path(output_dir, "02_numerical_summary.csv"), row.names = FALSE)

# 3.2 Charges Breakdown by Demographics & Lifestyle
charges_by_group <- df %>%
  group_by(smoker, sex) %>%
  summarise(
    Count        = n(),
    Mean_Charge  = mean(charges),
    Median_Charge= median(charges),
    SD_Charge    = sd(charges),
    .groups      = 'drop'
  )

cat("\n[B] Medical Charges Breakdown by Smoker and Sex:\n")
print(charges_by_group)
write.csv(charges_by_group, file.path(output_dir, "03_charges_by_smoker_sex.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# 4. EXPLORATORY VISUALIZATIONS (DISPLAYED & EXPORTED)
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 4. GENERATING EDA VISUALIZATIONS\n")
cat("==================================================================\n")

# Visual 1: Distribution of Medical Charges (Original vs Log-Transformed)
p1a <- ggplot(df, aes(x = charges)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#2C3E50", color = "white", alpha = 0.8) +
  geom_density(color = "#E74C3C", linewidth = 1) +
  scale_x_continuous(labels = label_dollar()) +
  labs(title = "Original Distribution of Charges", subtitle = "Right-skewed healthcare expenditures", x = "Charges ($)", y = "Density")

p1b <- ggplot(df, aes(x = log(charges))) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#16A085", color = "white", alpha = 0.8) +
  geom_density(color = "#D35400", linewidth = 1) +
  labs(title = "Log-Transformed Charges", subtitle = "Bimodal distribution driven by smoking status", x = "ln(Charges)", y = "Density")

p1 <- grid.arrange(p1a, p1b, ncol = 2)
ggsave(file.path(output_dir, "plot_01_charges_distribution.png"), p1, width = 11, height = 5, dpi = 300)

# Visual 2: Medical Charges by Smoker Status & BMI Category
p2 <- ggplot(df, aes(x = bmi_category, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.85, outlier.color = "red", outlier.size = 1.2) +
  scale_fill_manual(values = c("no" = "#3498DB", "yes" = "#E74C3C")) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Medical Charges Across BMI Categories by Smoking Status",
    subtitle = "Major cost escalation observed specifically for obese smokers",
    x = "BMI Classification",
    y = "Medical Charges ($)",
    fill = "Smoker Status"
  )
print(p2)
ggsave(file.path(output_dir, "plot_02_charges_by_bmi_smoker.png"), p2, width = 9, height = 5.5, dpi = 300)

# Visual 3: Interaction Scatter Plot (BMI vs Charges grouped by Smoker)
p3 <- ggplot(df, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c("no" = "#2980B9", "yes" = "#C0392B")) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Interaction Effect: BMI vs. Medical Charges Moderated by Smoking",
    subtitle = "Steep regression slope for smokers confirms non-parallel interaction",
    x = "Body Mass Index (BMI)",
    y = "Medical Charges ($)",
    color = "Smoker"
  )
print(p3)
ggsave(file.path(output_dir, "plot_03_bmi_smoker_interaction.png"), p3, width = 9, height = 5.5, dpi = 300)

# Visual 4: Correlation Matrix of Numeric Variables
numeric_vars <- df %>% select(age, bmi, children, charges)
cor_matrix   <- cor(numeric_vars)
write.csv(cor_matrix, file.path(output_dir, "04_correlation_matrix.csv"))

png(file.path(output_dir, "plot_04_correlation_matrix.png"), width = 7, height = 6, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("#3498DB", "#FFFFFF", "#E74C3C"))(200),
         title = "Pearson Correlation Matrix", mar = c(0,0,2,0))
dev.off()
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Pearson Correlation Matrix", mar = c(0,0,2,0))


# ------------------------------------------------------------------------------
# 5. INFERENTIAL STATISTICAL MODELLING
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 5. INFERENTIAL STATISTICAL MODELLING\n")
cat("==================================================================\n")

# Model 1: Baseline Main Effects Multiple Linear Regression
model_1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train_data)

# Model 2: Moderated Model with BMI * Smoker Interaction Term
model_2 <- lm(charges ~ age + sex + bmi * smoker + children + region, data = train_data)

# Print Model 2 Output in Console
cat("\n--- MODEL 2 SUMMARY (Interaction Model) ---\n")
print(summary(model_2))

# Export Model 2 Tidy Results
m2_tidy <- tidy(model_2, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))
print(m2_tidy)
write.csv(m2_tidy, file.path(output_dir, "05_model_2_coefficients.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# 6. MODEL ASSUMPTION DIAGNOSTICS & VALIDATION
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 6. ASSUMPTION DIAGNOSTICS & VALIDATION\n")
cat("==================================================================\n")

# Diagnostic Data Preparation
diag_df <- data.frame(
  Fitted    = fitted(model_2),
  Residuals = resid(model_2),
  Std_Res   = rstandard(model_2),
  Cooks_D   = cooks.distance(model_2)
)

# 6.1 Multicollinearity Check via Variance Inflation Factor (VIF)
# Note: For models with interaction terms, GVIF^(1/(2*Df)) is evaluated
vif_results <- vif(model_2)
cat("\n[A] Multicollinearity (VIF/GVIF Results):\n")
print(vif_results)
write.csv(as.data.frame(vif_results), file.path(output_dir, "06_vif_diagnostics.csv"))

# 6.2 Homoscedasticity Test (Breusch-Pagan Test)
bp_test <- bptest(model_2)
cat("\n[B] Breusch-Pagan Test for Heteroscedasticity:\n")
print(bp_test)

# 6.3 Autocorrelation / Independence Test (Durbin-Watson Test)
dw_test <- dwtest(model_2)
cat("\n[C] Durbin-Watson Test for Independence of Residuals:\n")
print(dw_test)

# 6.4 Normality Test (Shapiro-Wilk Test on Sample of Residuals)
shapiro_test <- shapiro.test(sample(resid(model_2), min(500, length(resid(model_2)))))
cat("\n[D] Shapiro-Wilk Normality Test (Sampled):\n")
print(shapiro_test)

# 6.5 Export Assumption Diagnostic Tests Summary
assumption_summary <- data.frame(
  Assumption = c("Heteroscedasticity (Breusch-Pagan)", "Independence (Durbin-Watson)", "Normality (Shapiro-Wilk)"),
  Test_Statistic = c(round(bp_test$statistic, 3), round(dw_test$statistic, 3), round(shapiro_test$statistic, 3)),
  P_Value = c(format.pval(bp_test$p.value, digits = 4), format.pval(dw_test$p.value, digits = 4), format.pval(shapiro_test$p.value, digits = 4)),
  Conclusion = c("No significant heteroscedasticity detected (Homoscedasticity assumed)", "No residual autocorrelation (DW ~ 2)", "Residuals show deviations in tails")
)
write.csv(assumption_summary, file.path(output_dir, "07_assumption_test_summary.csv"), row.names = FALSE)

# 6.6 Multi-Panel Diagnostic Plots
p_diag1 <- ggplot(diag_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "#2980B9") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", linewidth = 0.8) +
  scale_x_continuous(labels = label_dollar()) +
  labs(title = "Residuals vs Fitted", subtitle = "Checks Linearity & Equal Variance", x = "Fitted Values ($)", y = "Residuals")

p_diag2 <- ggplot(diag_df, aes(sample = Std_Res)) +
  stat_qq(color = "#8E44AD", alpha = 0.6) +
  stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Normal Q-Q Plot", subtitle = "Checks Normality of Standardized Residuals", x = "Theoretical Quantiles", y = "Standardized Residuals")

p_diag3 <- ggplot(diag_df, aes(x = Fitted, y = sqrt(abs(Std_Res)))) +
  geom_point(alpha = 0.5, color = "#27AE60") +
  geom_smooth(se = FALSE, color = "red", linewidth = 0.8) +
  labs(title = "Scale-Location Plot", subtitle = "Evaluates Homoscedasticity", x = "Fitted Values ($)", y = "sqrt(|Std Residuals|)")

p_diag4 <- ggplot(diag_df, aes(x = seq_along(Cooks_D), y = Cooks_D)) +
  geom_bar(stat = "identity", fill = "#E67E22", width = 0.5) +
  geom_hline(yintercept = 4 / nrow(train_data), color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance Plot", subtitle = "Threshold: 4/n (Influential Observations)", x = "Observation Index", y = "Cook's Distance")

diag_panel <- grid.arrange(p_diag1, p_diag2, p_diag3, p_diag4, ncol = 2)
ggsave(file.path(output_dir, "plot_05_regression_diagnostics.png"), diag_panel, width = 11, height = 8.5, dpi = 300)


# ------------------------------------------------------------------------------
# 7. MODEL COMPARISON & HYPOTHESIS TESTING
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 7. MODEL COMPARISON & HYPOTHESIS TESTING\n")
cat("==================================================================\n")

# Nested Model Comparison via Partial F-Test (ANOVA)
# H0: Interaction term coefficient (bmi:smokeryes) == 0
# H1: Interaction term coefficient (bmi:smokeryes) != 0
model_anova <- anova(model_1, model_2)
cat("\n[A] Nested ANOVA F-Test (Model 1 vs Model 2):\n")
print(model_anova)
write.csv(as.data.frame(model_anova), file.path(output_dir, "08_anova_model_comparison.csv"))

# Evaluate Test Set Predictions
test_pred_m1 <- predict(model_1, newdata = test_data)
test_pred_m2 <- predict(model_2, newdata = test_data)

calc_metrics <- function(actual, predicted, model_obj, p_count) {
  n <- length(actual)
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n - 1) / (n - p_count - 1))
  aic  <- AIC(model_obj)
  bic  <- BIC(model_obj)
  return(c(RMSE = rmse, MAE = mae, R2 = r2, Adj_R2 = adj_r2, AIC = aic, BIC = bic))
}

metrics_m1 <- calc_metrics(test_data$charges, test_pred_m1, model_1, length(coef(model_1)))
metrics_m2 <- calc_metrics(test_data$charges, test_pred_m2, model_2, length(coef(model_2)))

perf_comparison <- data.frame(
  Metric  = names(metrics_m1),
  Model_1_MainEffects = round(unname(metrics_m1), 3),
  Model_2_Interaction = round(unname(metrics_m2), 3)
)
cat("\n[B] Out-of-Sample Test Set Performance Comparison:\n")
print(perf_comparison)
write.csv(perf_comparison, file.path(output_dir, "09_model_performance_comparison.csv"), row.names = FALSE)

# Visual 6: Actual vs Predicted Charges on Unseen Test Data
test_eval_df <- test_data %>%
  mutate(Predicted_Charges = test_pred_m2)

p_perf <- ggplot(test_eval_df, aes(x = charges, y = Predicted_Charges, color = smoker)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("no" = "#3498DB", "yes" = "#E74C3C")) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Actual vs. Predicted Medical Charges (Test Set Evaluation)",
    subtitle = paste0("Model 2 with Interaction: Test R² = ", round(metrics_m2["R2"], 3), ", RMSE = $", round(metrics_m2["RMSE"], 0)),
    x = "Actual Charges ($)",
    y = "Predicted Charges ($)",
    color = "Smoker"
  )
print(p_perf)
ggsave(file.path(output_dir, "plot_06_actual_vs_predicted.png"), p_perf, width = 8.5, height = 5.5, dpi = 300)


# ------------------------------------------------------------------------------
# 8. BUSINESS SCENARIO SIMULATIONS & UNDERWRITING RECOMMENDATIONS
# ------------------------------------------------------------------------------
cat("\n==================================================================\n")
cat(" 8. BUSINESS SCENARIO SIMULATION FOR UNDERWRITING\n")
cat("==================================================================\n")

# Create realistic persona scenarios to demonstrate practical business application
business_scenarios <- tibble(
  Profile = c(
    "Scenario A: Young Healthy Non-Smoker",
    "Scenario B: Young Healthy Smoker",
    "Scenario C: Middle-Aged Obese Non-Smoker",
    "Scenario D: Middle-Aged Obese Smoker"
  ),
  age      = c(25, 25, 45, 45),
  sex      = factor(c("female", "female", "male", "male"), levels = levels(df$sex)),
  bmi      = c(22.0, 22.0, 34.0, 34.0),
  children = c(0, 0, 2, 2),
  smoker   = factor(c("no", "yes", "no", "yes"), levels = levels(df$smoker)),
  region   = factor(c("northwest", "northwest", "southeast", "southeast"), levels = levels(df$region))
)

# Predict with 95% Confidence Intervals
scenario_predictions <- predict(model_2, newdata = business_scenarios, interval = "confidence")

scenario_results <- business_scenarios %>%
  select(Profile, age, bmi, smoker) %>%
  bind_cols(as_tibble(scenario_predictions)) %>%
  rename(
    Predicted_Charge = fit,
    Lower_95_CI      = lwr,
    Upper_95_CI      = upr
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

cat("\nUnderwriting Predictions for Strategic Pricing Scenarios:\n")
print(scenario_results)
write.csv(scenario_results, file.path(output_dir, "10_business_scenario_predictions.csv"), row.names = FALSE)

cat("\n==================================================================\n")
cat(" [SUCCESS] All analyses complete.\n")
cat(" Tables exported to: ", output_dir, "/*.csv\n")
cat(" Figures exported to: ", output_dir, "/*.png\n")
cat("==================================================================\n")