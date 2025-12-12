# COVID-19 Malaysia: Statistical Analysis
# 7COM1079 - Team Research and Development Project
#
# Group Members:
# 1. Muhammad Bilal Naeem Siddiqui (24088412) - Research Question 1
# 2. Malik Umer Munsab (24077433) - Research Question 2
# 3. Sobia Arshad (24096983) - Research Question 3

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Set working directory to script location
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)

# Create output directories
if (!dir.exists("output")) {
  dir.create("output")
}
if (!dir.exists("output/RQ1")) {
  dir.create("output/RQ1")
}
if (!dir.exists("output/RQ2")) {
  dir.create("output/RQ2")
}
if (!dir.exists("output/RQ3")) {
  dir.create("output/RQ3")
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("COVID-19 MALAYSIA: GROUP STATISTICAL ANALYSIS\n")
cat("7COM1079 - Team Research and Development Project\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")


# RESEARCH QUESTION 1 (Muhammad Bilal): CORRELATION ANALYSIS


cat(paste(rep("=", 80), collapse = ""), "\n")
cat("RESEARCH QUESTION 1: CORRELATION ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Is there a correlation between daily new COVID-19 cases and\n")
cat("daily vaccination doses administered in Malaysia?\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Load data for RQ1
cases_data <- read.csv("archive/epidemic/cases_malaysia.csv", stringsAsFactors = FALSE)
vax_data <- read.csv("archive/vaccination/vax_malaysia.csv", stringsAsFactors = FALSE)

# Convert dates
cases_data$date <- as.Date(cases_data$date)
vax_data$date <- as.Date(vax_data$date)

# Merge datasets
merged_data_rq1 <- merge(cases_data, vax_data, by = "date", all = FALSE)

# Prepare analysis data
analysis_rq1 <- merged_data_rq1 %>%
  select(date, cases_new, daily) %>%
  rename(daily_cases = cases_new, daily_vaccinations = daily) %>%
  filter(!is.na(daily_cases) & !is.na(daily_vaccinations)) %>%
  filter(daily_cases >= 0 & daily_vaccinations >= 0)

# Descriptive statistics RQ1
cat("Data Summary:\n")
cat("  Total observations:", nrow(analysis_rq1), "\n")
cat("  Date range:", as.character(min(analysis_rq1$date)), "to",
    as.character(max(analysis_rq1$date)), "\n\n")

cat("Daily Cases:\n")
cat("  Mean:", round(mean(analysis_rq1$daily_cases), 2), "\n")
cat("  Median:", round(median(analysis_rq1$daily_cases), 2), "\n")
cat("  SD:", round(sd(analysis_rq1$daily_cases), 2), "\n\n")

cat("Daily Vaccinations:\n")
cat("  Mean:", round(mean(analysis_rq1$daily_vaccinations), 2), "\n")
cat("  Median:", round(median(analysis_rq1$daily_vaccinations), 2), "\n")
cat("  SD:", round(sd(analysis_rq1$daily_vaccinations), 2), "\n\n")

# Visualization 1.1: Histogram - Daily Cases
png("output/RQ1/histogram_daily_cases.png", width = 800, height = 600, res = 100)
ggplot(analysis_rq1, aes(x = daily_cases)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Daily New COVID-19 Cases in Malaysia",
       x = "Daily New Cases", y = "Frequency") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 12))
dev.off()

# Visualization 1.2: Histogram - Daily Vaccinations
png("output/RQ1/histogram_daily_vaccinations.png", width = 800, height = 600, res = 100)
ggplot(analysis_rq1, aes(x = daily_vaccinations)) +
  geom_histogram(binwidth = 10000, fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Daily Vaccination Doses in Malaysia",
       x = "Daily Vaccination Doses", y = "Frequency") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 12))
dev.off()

# Visualization 1.3: Scatterplot
png("output/RQ1/scatterplot_correlation.png", width = 900, height = 600, res = 100)
ggplot(analysis_rq1, aes(x = daily_vaccinations, y = daily_cases)) +
  geom_point(alpha = 0.5, color = "darkblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(title = "Correlation between Daily Vaccinations and Daily Cases",
       x = "Daily Vaccination Doses", y = "Daily New COVID-19 Cases") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 12))
dev.off()

cat("Visualizations saved to output/RQ1/\n\n")

# Normality tests RQ1
cat("Normality Tests (Shapiro-Wilk):\n")
shapiro_cases_rq1 <- shapiro.test(sample(analysis_rq1$daily_cases, min(5000, nrow(analysis_rq1))))
shapiro_vax_rq1 <- shapiro.test(sample(analysis_rq1$daily_vaccinations, min(5000, nrow(analysis_rq1))))
cat("  Daily cases p-value:", format(shapiro_cases_rq1$p.value, scientific = TRUE), "\n")
cat("  Daily vaccinations p-value:", format(shapiro_vax_rq1$p.value, scientific = TRUE), "\n")

if (shapiro_cases_rq1$p.value < 0.05 | shapiro_vax_rq1$p.value < 0.05) {
  cat("  Method: Spearman's correlation (non-parametric)\n\n")
  cor_method_rq1 <- "spearman"
} else {
  cat("  Method: Pearson's correlation (parametric)\n\n")
  cor_method_rq1 <- "pearson"
}

# Correlation test RQ1
cor_test_rq1 <- cor.test(analysis_rq1$daily_vaccinations, analysis_rq1$daily_cases, method = cor_method_rq1)
r_squared_rq1 <- cor_test_rq1$estimate^2

cat("Correlation Test Results:\n")
cat("  Correlation coefficient:", round(cor_test_rq1$estimate, 4), "\n")
cat("  R-squared (R²):", round(r_squared_rq1, 4), "\n")
cat("  P-value:", format(cor_test_rq1$p.value, scientific = TRUE), "\n")

if (cor_test_rq1$p.value < 0.05) {
  cat("  Decision: REJECT null hypothesis\n")
  cat("  Conclusion: Significant correlation exists\n\n")
} else {
  cat("  Decision: FAIL TO REJECT null hypothesis\n")
  cat("  Conclusion: No significant correlation\n\n")
}

# RESEARCH QUESTION 2 (Malik Umer): COMPARISON OF MEANS


cat(paste(rep("=", 80), collapse = ""), "\n")
cat("RESEARCH QUESTION 2: COMPARISON OF MEANS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Is there a difference in the mean number of daily COVID-19 deaths\n")
cat("between periods of high and low vaccination rates in Malaysia?\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Load data for RQ2
deaths_data <- read.csv("archive/epidemic/deaths_malaysia.csv", stringsAsFactors = FALSE)
deaths_data$date <- as.Date(deaths_data$date)

# Merge deaths with vaccination data
merged_data_rq2 <- merge(deaths_data, vax_data, by = "date", all = FALSE)

# Categorize vaccination periods (threshold: 100,000 doses/day)
analysis_rq2 <- merged_data_rq2 %>%
  select(date, deaths_new, daily) %>%
  rename(daily_deaths = deaths_new, daily_vaccinations = daily) %>%
  filter(!is.na(daily_deaths) & !is.na(daily_vaccinations)) %>%
  filter(daily_deaths >= 0 & daily_vaccinations >= 0) %>%
  mutate(vaccination_period = ifelse(daily_vaccinations > 100000, "High", "Low"))

# Descriptive statistics RQ2
cat("Data Summary:\n")
cat("  Total observations:", nrow(analysis_rq2), "\n")
cat("  High vaccination period (>100,000 doses/day):",
    sum(analysis_rq2$vaccination_period == "High"), "days\n")
cat("  Low vaccination period (<=100,000 doses/day):",
    sum(analysis_rq2$vaccination_period == "Low"), "days\n\n")

cat("Daily Deaths by Vaccination Period:\n")
high_deaths <- analysis_rq2 %>% filter(vaccination_period == "High")
low_deaths <- analysis_rq2 %>% filter(vaccination_period == "Low")
cat("  High period - Mean:", round(mean(high_deaths$daily_deaths), 2),
    "SD:", round(sd(high_deaths$daily_deaths), 2), "\n")
cat("  Low period - Mean:", round(mean(low_deaths$daily_deaths), 2),
    "SD:", round(sd(low_deaths$daily_deaths), 2), "\n\n")

# Visualization 2.1: Histogram - Daily Deaths
png("output/RQ2/histogram_daily_deaths.png", width = 800, height = 600, res = 100)
ggplot(analysis_rq2, aes(x = daily_deaths)) +
  geom_histogram(binwidth = 10, fill = "darkred", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Daily COVID-19 Deaths in Malaysia",
       x = "Daily Deaths", y = "Frequency") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 12))
dev.off()

# Visualization 2.2: Histogram - Vaccination Periods
png("output/RQ2/histogram_vaccination_periods.png", width = 800, height = 600, res = 100)
ggplot(analysis_rq2, aes(x = daily_vaccinations, fill = vaccination_period)) +
  geom_histogram(binwidth = 10000, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("High" = "darkgreen", "Low" = "lightgreen")) +
  labs(title = "Distribution of Daily Vaccinations by Period",
       x = "Daily Vaccination Doses", y = "Frequency", fill = "Period") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 12),
        legend.position = "right")
dev.off()

# Visualization 2.3: Boxplot
png("output/RQ2/boxplot_deaths_by_period.png", width = 800, height = 600, res = 100)
ggplot(analysis_rq2, aes(x = vaccination_period, y = daily_deaths, fill = vaccination_period)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("High" = "darkgreen", "Low" = "lightgreen")) +
  labs(title = "Daily Deaths by Vaccination Period",
       x = "Vaccination Period", y = "Daily Deaths") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 12),
        legend.position = "none")
dev.off()

cat("Visualizations saved to output/RQ2/\n\n")

# Normality test RQ2
cat("Normality Tests (Shapiro-Wilk):\n")
shapiro_high <- shapiro.test(sample(high_deaths$daily_deaths, min(5000, nrow(high_deaths))))
shapiro_low <- shapiro.test(sample(low_deaths$daily_deaths, min(5000, nrow(low_deaths))))
cat("  High period p-value:", format(shapiro_high$p.value, scientific = TRUE), "\n")
cat("  Low period p-value:", format(shapiro_low$p.value, scientific = TRUE), "\n")

if (shapiro_high$p.value < 0.05 | shapiro_low$p.value < 0.05) {
  cat("  Method: Wilcoxon test (non-parametric)\n\n")
  comparison_test_rq2 <- wilcox.test(daily_deaths ~ vaccination_period, data = analysis_rq2)
  test_name_rq2 <- "Wilcoxon"
} else {
  cat("  Method: t-test (parametric)\n\n")
  comparison_test_rq2 <- t.test(daily_deaths ~ vaccination_period, data = analysis_rq2)
  test_name_rq2 <- "t-test"
}

# Statistical test RQ2
cat("Comparison Test Results (", test_name_rq2, "):\n", sep = "")
cat("  Test statistic:", round(comparison_test_rq2$statistic, 4), "\n")
cat("  P-value:", format(comparison_test_rq2$p.value, scientific = TRUE), "\n")

if (comparison_test_rq2$p.value < 0.05) {
  cat("  Decision: REJECT null hypothesis\n")
  cat("  Conclusion: Significant difference in mean deaths between periods\n\n")
} else {
  cat("  Decision: FAIL TO REJECT null hypothesis\n")
  cat("  Conclusion: No significant difference in mean deaths\n\n")
