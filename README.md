# Accounting-Based Valuation Across Legal Origins (2022–2024)

This repository contains the code and output files for Assignment 3 of the course  
**Accounting II – Corporate Decision-Making and Quantitative Analysis**.

## Research Question

Do accounting-based valuation metrics vary systematically across countries?  
Specifically, does legal origin (Common Law vs. Civil Law) explain cross-country variation in price-to-book ratios?

This study provides a conceptual replication of La Porta et al. (1998) using firm-level data from 2022–2024.

---

## Data Source

Firm-level data are retrieved from:

- Compustat Global (via WRDS)

Due to WRDS licensing restrictions, the repository link is shared exclusively in the submitted essay for evaluation purposes.
Raw data files are not publicly available and will be removed after grading.

Users with WRDS access can reproduce the results by running the provided R script.

---

## Sample Construction

- Period: 2022–2024
- Financial firms (SIC 6000–6999) excluded
- Positive assets, equity, shares, and price required
- All continuous variables winsorized at the 1% and 99% levels
- Legal origin classification follows La Porta et al. (1998)

Final sample: 22,871 firm-year observations.

---

## Empirical Design

Baseline regression:

P/B = α + β₁ CommonLaw + Year FE + ε

Extended model:

P/B = α + β₁ CommonLaw + γ ROA + Year FE + Industry FE + ε

---

## Repository Structure

analysis.R  
Contains the full data cleaning and regression pipeline.

output/  
Includes regression summaries, descriptive statistics, and figures.

Assignment_3_Qinggang_Zeng.pdf  
Final submitted paper.

---

## Main Finding

Contrary to La Porta et al. (1998), common law origin is negatively associated with price-to-book ratios in 2022–2024.

---

## Author

Qinggang Zeng  
Humboldt-Universität zu Berlin  
