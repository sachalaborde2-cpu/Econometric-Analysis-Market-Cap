# 📊 Econometric Analysis: Determinants of Market Capitalization

**Overview**
This repository contains an applied econometric research project developed in **R**. The objective is to identify and isolate the financial, organizational, and structural determinants of corporate market capitalization. The analysis leverages a panel dataset comprising 8,000 firm-year observations across 4,000 unique firms over a two-year period (2006-2007).

**Methodology & Econometric Strategy**
To rigorously evaluate the impact of variables such as profit, investment, and board composition, the project transitions from baseline Pooled OLS to advanced Fixed-Effects models.
* **Specification Testing:** Employed Breusch-Pagan and Hausman tests to confirm the presence of heteroskedasticity and validate the necessity of fixed effects over random effects.
* **Panel Data Models:** Implemented Firm-Level and Time Fixed-Effects to control for time-invariant unobserved heterogeneity and macroeconomic shocks.
* **Robust Inference:** Utilized firm-level clustered robust standard errors to correct for residual correlation and heteroskedasticity.

**Key Findings**
* **Dynamic Drivers:** Once firm-specific unobserved heterogeneity is controlled for, within-firm changes in **investment and profit** emerge as the only statistically significant dynamic drivers of market capitalization.
* **Structural Illusions:** Factors such as board size, gender diversity, and human capital, which appeared significant in initial OLS regressions, lose their significance in the Fixed-Effects model. This demonstrates that these variables drive cross-sectional differences between firms, rather than short-term within-firm valuation changes.
* **Heterogeneous Effects:** Interaction models reveal that the market does not value profit uniformly. Profit increases show no significant impact for young firms (< 5 years) but are highly valued for mature and older firms, highlighting the importance of firm life-cycle in investor sentiment.

**Repository Contents**
* `Script.R` : The complete R codebase for data cleaning, statistical testing, and model estimation.
* `Econometric_Report.pdf` : The comprehensive 11-page academic report detailing the theoretical framework, mathematical derivations, and economic interpretations.
