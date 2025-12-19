############################################################
# Accounting II – Corporate Decision-Making and Quantitative Analysis
# Assignment 2
# Topic (4): Correlation between Price-to-Book and ROA
############################################################

# -----------------------------
# 0. File path (CSV ONLY)
# -----------------------------
file_path <- "/Users/zeng/Desktop/Humboldt/2024 WS/applied_ecx/data/ompustat_na_fy2023.csv"

# -----------------------------
# 1. Load packages
# -----------------------------
# Install once if needed:
# install.packages(c("dplyr", "ggplot2", "tibble"))

library(dplyr)
library(ggplot2)
library(tibble)

# -----------------------------
# 2. Read data
# -----------------------------
raw <- read.csv(file_path, stringsAsFactors = FALSE)

# Quick checks
dim(raw)
names(raw)

# -----------------------------
# 3. Helper function: winsorization
# -----------------------------
winsorize_vec <- function(x, p = 0.01) {
  qs <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  return(x)
}

# -----------------------------
# 4. Data cleaning
# -----------------------------
df <- raw %>%
  mutate(
    fyear  = as.integer(fyear),
    sic    = as.numeric(sic),
    at     = as.numeric(at),
    ni     = as.numeric(ni),
    seq    = as.numeric(seq),
    prcc_f = as.numeric(prcc_f),
    csho   = as.numeric(csho)
  ) %>%
  # keep fiscal year 2023 only
  filter(fyear == 2023) %>%
  # exclude financial firms (SIC 6000–6999)
  filter(is.na(sic) | !(sic >= 6000 & sic <= 6999)) %>%
  # drop missing values needed for ROA and P/B
  filter(
    !is.na(ni),
    !is.na(at),
    !is.na(seq),
    !is.na(prcc_f),
    !is.na(csho)
  ) %>%
  # economically meaningful constraints
  filter(at > 0, seq > 0) %>%
  # construct variables
  mutate(
    ROA = ni / at,
    MarketCap = prcc_f * csho,
    PB = MarketCap / seq
  ) %>%
  # remove infinite or nonsensical values
  filter(is.finite(ROA), is.finite(PB), PB > 0)

# Check remaining observations
nrow(df)

# -----------------------------
# 5. Winsorize ROA and P/B (1%–99%)
# -----------------------------
df <- df %>%
  mutate(
    ROA_w = winsorize_vec(ROA, 0.01),
    PB_w  = winsorize_vec(PB,  0.01)
  )

# -----------------------------
# 6. Descriptive statistics
# -----------------------------
desc_stats <- df %>%
  summarise(
    N = n(),
    ROA_mean = mean(ROA, na.rm = TRUE),
    ROA_median = median(ROA, na.rm = TRUE),
    PB_mean = mean(PB, na.rm = TRUE),
    PB_median = median(PB, na.rm = TRUE),
    ROA_w_mean = mean(ROA_w, na.rm = TRUE),
    PB_w_mean = mean(PB_w, na.rm = TRUE)
  )

print(desc_stats)

# -----------------------------
# 7. Correlation analysis
# -----------------------------
pearson_corr <- cor.test(df$PB_w, df$ROA_w, method = "pearson")
spearman_corr <- cor.test(df$PB_w, df$ROA_w, method = "spearman")

cor_results <- tibble(
  Method = c("Pearson", "Spearman"),
  Correlation = c(unname(pearson_corr$estimate),
                  unname(spearman_corr$estimate)),
  P_value = c(pearson_corr$p.value,
              spearman_corr$p.value)
)

print(cor_results)

# -----------------------------
# 8. Scatter plot
# -----------------------------
plot_pb_roa <- ggplot(df, aes(x = ROA_w, y = PB_w)) +
  geom_point(alpha = 0.35) +
  labs(
    title = "Correlation between ROA and Price-to-Book (FY 2023)",
    x = "ROA (winsorized at 1%–99%)",
    y = "Price-to-Book (winsorized at 1%–99%)"
  ) +
  theme_minimal()

print(plot_pb_roa)

# -----------------------------
# 9. Save outputs (optional but recommended)
# -----------------------------
output_dir <- "/Users/zeng/Desktop/Humboldt/2024 WS/applied_ecx/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(df,
          file = paste0(output_dir, "/cleaned_data_2023.csv"),
          row.names = FALSE)

write.csv(desc_stats,
          file = paste0(output_dir, "/descriptive_statistics.csv"),
          row.names = FALSE)

write.csv(cor_results,
          file = paste0(output_dir, "/correlation_results.csv"),
          row.names = FALSE)

ggsave(
  filename = paste0(output_dir, "/scatter_roa_pb.png"),
  plot = plot_pb_roa,
  width = 7,
  height = 5,
  dpi = 300
)

# --------------------------------
# 10. Linear regression (OLS)
# --------------------------------
lm_pb_roa <- lm(PB_w ~ ROA_w, data = df)



############################################################
# End of script
############################################################