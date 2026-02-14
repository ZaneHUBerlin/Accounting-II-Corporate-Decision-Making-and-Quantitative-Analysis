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
	•	Compustat Global (via WRDS)

The cleaned dataset (panel_clean_2022_2024.csv) is constructed by merging:
	1.	Compustat Global – Fundamentals Annual
	2.	Compustat Global – Security Monthly

using **GVKEY and fiscal year identifiers**.

For each firm-year observation, December closing prices (or, if unavailable, the last available monthly price within the fiscal year) are used to compute market capitalization and price-to-book ratios.

Due to WRDS licensing restrictions:
	•	Raw Compustat files are not publicly included in this repository.
	•	The repository link is shared exclusively in the submitted essay for evaluation purposes.
	•	Raw data files will be removed after grading.

Users with WRDS access can fully reproduce the dataset by:
	1.	Downloading the relevant Fundamentals Annual and Security Monthly files (2022–2024) from WRDS.
	2.	Placing them inside the /data/ folder.
	3.	Running analysis_assignment3.R.

The script performs all merging, cleaning, winsorization, and regression procedures.

---

## Variables Used
The merged dataset is constructed using the following variables:

From Compustat Global – Fundamentals Annual

	•	gvkey (firm identifier)
	•	fyear (fiscal year)
	•	fic (country of incorporation)
	•	sic (industry classification)
	•	at (total assets)
	•	seq (book value of equity)
	•	ib or ni (income before extraordinary items / net income)
	•	cshr / csho / cshoi (common shares outstanding, depending on availability)

From Compustat Global – Security Monthly

	•	gvkey
	•	datadate
	•	prccm (monthly closing price)

December closing prices (or the last available monthly price within the fiscal year if December is missing) are used to compute market capitalization.

---

## Constructed Variables

The following variables are constructed in the R script:
	•	Market Capitalization = Price × Shares
	•	Price-to-Book (P/B) = Market Capitalization / Book Equity
	•	Return on Assets (ROA) = Profit / Total Assets
	•	CommonLaw Dummy = 1 if country follows common law tradition (La Porta et al., 1998), 0 otherwise
	•	Industry FE = 2-digit SIC classification
	•	Winsorized variables (pb_w, roa_w) at 1% and 99%

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

analysis_assignment3.R  
Full data cleaning, merging, and regression pipeline.

data/  
Cleaned panel dataset (2022–2024) for replication.

Assignment 3 Data Output/  
Descriptive statistics, regression summaries, and figures.

Assignment_3_Qinggang_Zeng.pdf  
Final submitted paper.

---

## Main Finding

Contrary to La Porta et al. (1998), common law origin is negatively associated with price-to-book ratios in 2022–2024.

---

## Author

Qinggang Zeng  
Humboldt-Universität zu Berlin  
