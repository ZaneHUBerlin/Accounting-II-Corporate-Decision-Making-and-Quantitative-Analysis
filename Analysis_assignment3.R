############################################################
# Accounting II – Assignment 3
# Compustat Global panel (2022–2024): Fundamentals Annual + Security Monthly
# Output folder: "Assignment 3 Data Output"
############################################################

# -----------------------------
# 0) Packages
# -----------------------------
# install.packages(c("dplyr","lubridate","readr","ggplot2"))

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

############################################################
# 1. File Paths (GitHub-relative, NOT local paths)
############################################################

fund_file <- "data/Compustat_Fundamentals Annual 22-24.csv"
sec_file  <- "data/Compustat_Security Monthly 22-24.csv"

output_dir <- "Assignment 3 Data Output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
# -----------------------------
# 2) Read data
# -----------------------------
fund <- read_csv(fund_file, show_col_types = FALSE) %>%
  mutate(gvkey = as.character(gvkey))

sec <- read_csv(sec_file, show_col_types = FALSE) %>%
  mutate(gvkey = as.character(gvkey))

cat("Fund rows:", nrow(fund), "\n")
cat("Sec rows:", nrow(sec), "\n")

# -----------------------------
# 3) Security Monthly -> build one price per firm-year
#    Priority: Dec close; if missing, take last available month in that year
# -----------------------------
sec <- sec %>%
  mutate(
    datadate   = as.Date(datadate),
    cal_year   = year(datadate),
    cal_month  = month(datadate),
    prccm      = as.numeric(prccm)
  ) %>%
  filter(!is.na(cal_year), !is.na(cal_month))

sec_year_price <- sec %>%
  group_by(gvkey, cal_year) %>%
  # if Dec exists, keep Dec; else keep latest month
  arrange(desc(cal_month)) %>%
  mutate(has_dec = any(cal_month == 12)) %>%
  filter(ifelse(has_dec, cal_month == 12, cal_month == max(cal_month))) %>%
  slice(1) %>%
  ungroup() %>%
  select(gvkey, cal_year, prccm)

# -----------------------------
# 4) Merge fundamentals + price (gvkey + fiscal year)
# -----------------------------
if (!("fyear" %in% names(fund))) stop("Fundamentals file must contain 'fyear'.")

panel <- fund %>%
  mutate(fyear = as.integer(fyear)) %>%
  left_join(sec_year_price, by = c("gvkey" = "gvkey", "fyear" = "cal_year"))

cat("Panel rows after merge:", nrow(panel), "\n")
cat("Missing prccm after merge:", sum(is.na(panel$prccm)), "\n")

# -----------------------------
# 5) Choose columns robustly (shares + profitability)
# -----------------------------
# shares: prefer cshr, else csho, else cshoi
shares_col <- c("cshr", "csho", "cshoi")[c("cshr","csho","cshoi") %in% names(panel)][1]
if (is.na(shares_col)) stop("No shares column found. Need one of: cshr / csho / cshoi")

# profit: prefer ib, else ni
profit_col <- c("ib", "ni")[c("ib","ni") %in% names(panel)][1]
if (is.na(profit_col)) stop("No profit column found. Need one of: ib / ni")

# Required columns
need <- c("gvkey","fyear","at","seq","sic","fic","prccm", shares_col, profit_col)
miss <- setdiff(need, names(panel))
if (length(miss) > 0) stop(paste("Missing columns:", paste(miss, collapse = ", ")))

# -----------------------------
# 6) Construct variables
# -----------------------------
df <- panel %>%
  transmute(
    gvkey = as.character(gvkey),
    fyear = as.integer(fyear),
    fic   = as.character(fic),
    sic   = suppressWarnings(as.numeric(sic)),
    at    = suppressWarnings(as.numeric(at)),
    seq   = suppressWarnings(as.numeric(seq)),
    price = suppressWarnings(as.numeric(prccm)),
    shares = suppressWarnings(as.numeric(.data[[shares_col]])),
    profit = suppressWarnings(as.numeric(.data[[profit_col]]))
  ) %>%
  filter(fyear >= 2022, fyear <= 2024) %>%
  mutate(
    roa    = profit / at,
    mktcap = price * shares,
    pb     = mktcap / seq
  )

# -----------------------------
# 7) Cleaning rules (same logic as your Assignment 2, but panel)
# -----------------------------
df <- df %>%
  # exclude financials (SIC 6000–6999)
  filter(is.na(sic) | !(sic >= 6000 & sic <= 6999)) %>%
  # meaningful denominators & market data
  filter(at > 0, seq > 0, shares > 0, price > 0) %>%
  # finite ratios
  filter(is.finite(roa), is.finite(pb), pb > 0)

cat("Rows after cleaning:", nrow(df), "\n")
cat("Unique firms:", n_distinct(df$gvkey), "\n")
cat("Years:", paste(sort(unique(df$fyear)), collapse = ", "), "\n")

# -----------------------------
# 8) Winsorize helper + winsorize ROA/PB (1%–99%)
# -----------------------------
winsorize_vec <- function(x, p = 0.01) {
  qs <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  x <- pmax(x, qs[1])
  x <- pmin(x, qs[2])
  x
}

df <- df %>%
  mutate(
    roa_w = winsorize_vec(roa, 0.01),
    pb_w  = winsorize_vec(pb,  0.01)
  )

# -----------------------------
# 9) Common Law dummy (based on FIC)
# -----------------------------
common_law_countries <- c(
  "USA","GBR","CAN","AUS","NZL",
  "IRL","HKG","SGP","IND","MYS",
  "PAK","ZAF"
)

df <- df %>%
  mutate(
    common_law = ifelse(fic %in% common_law_countries, 1, 0)
  )

# -----------------------------
# 10) Industry control (2-digit SIC)
# -----------------------------
df <- df %>%
  mutate(
    sic2 = ifelse(is.na(sic), NA_real_, floor(sic / 100))
  )

# -----------------------------
# 11) Descriptives + sample split checks
# -----------------------------
descriptive_statistics <- df %>%
  summarise(
    N = n(),
    firms = n_distinct(gvkey),
    years = paste(sort(unique(fyear)), collapse = ", "),
    pb_mean = mean(pb, na.rm = TRUE),
    pb_median = median(pb, na.rm = TRUE),
    roa_mean = mean(roa, na.rm = TRUE),
    roa_median = median(roa, na.rm = TRUE),
    pb_w_mean = mean(pb_w, na.rm = TRUE),
    roa_w_mean = mean(roa_w, na.rm = TRUE),
    commonlaw_share = mean(common_law, na.rm = TRUE)
  )

print(descriptive_statistics)

mean_comparison_commonlaw <- df %>%
  group_by(common_law) %>%
  summarise(
    n = n(),
    firms = n_distinct(gvkey),
    mean_pb_w = mean(pb_w, na.rm = TRUE),
    median_pb_w = median(pb_w, na.rm = TRUE),
    mean_roa_w = mean(roa_w, na.rm = TRUE),
    median_roa_w = median(roa_w, na.rm = TRUE),
    .groups = "drop"
  )

print(mean_comparison_commonlaw)

# -----------------------------
# 12) Regressions (paper core analysis)
#   Model 1: pb_w ~ common_law + year FE
#   Model 2: + roa_w
#   Model 3: + industry FE (2-digit SIC)
# -----------------------------
m1 <- lm(pb_w ~ common_law + factor(fyear), data = df)
m2 <- lm(pb_w ~ common_law + roa_w + factor(fyear), data = df)
m3 <- lm(pb_w ~ common_law + roa_w + factor(fyear) + factor(sic2), data = df)

# save regression output to txt
sink(file.path(output_dir, "regression_summary.txt"))
cat("============================================================\n")
cat("Assignment 3 Regression Results (winsorized pb/roa, 2022–2024)\n")
cat("============================================================\n\n")

cat("Model 1: pb_w ~ common_law + year FE\n\n")
print(summary(m1))

cat("\n\nModel 2: pb_w ~ common_law + roa_w + year FE\n\n")
print(summary(m2))

cat("\n\nModel 3: pb_w ~ common_law + roa_w + year FE + industry FE (2-digit SIC)\n\n")
print(summary(m3))

sink()

# -----------------------------
# 13) Plot: ROA vs P/B with linear fit
# -----------------------------
p <- ggplot(df, aes(x = roa_w, y = pb_w)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "ROA vs Price-to-Book (Compustat Global, 2022–2024)",
    x = "ROA (winsorized 1%–99%)",
    y = "P/B (winsorized 1%–99%)"
  ) +
  theme_minimal()

print(p)

# -----------------------------
# 14) Export outputs to "Assignment 3 Data Output"
# -----------------------------
write_csv(df, file.path(output_dir, "panel_clean_2022_2024.csv"))
write_csv(descriptive_statistics, file.path(output_dir, "descriptive_statistics.csv"))
write_csv(mean_comparison_commonlaw, file.path(output_dir, "mean_comparison_commonlaw.csv"))

ggsave(
  filename = file.path(output_dir, "scatter_roa_pb_2022_2024.png"),
  plot = p,
  width = 7, height = 5, dpi = 300
)

cat("\nDONE.\nSaved to folder:\n", output_dir, "\n")
cat("Files:\n",
    "- panel_clean_2022_2024.csv\n",
    "- descriptive_statistics.csv\n",
    "- mean_comparison_commonlaw.csv\n",
    "- regression_summary.txt\n",
    "- scatter_roa_pb_2022_2024.png\n")


