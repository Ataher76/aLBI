# aLBI: Estimating Length-Based Indicators for Fish Stock

[![CRAN status](https://www.r-pkg.org/badges/version/aLBI)](https://CRAN.R-project.org/package=aLBI)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## **Overview**
`aLBI` is a specialized R package for fish stock assessment in data-limited scenarios. It implements a simplified framework for estimating life-history parameters and assessing stock status using length-frequency data, following the methods of **Froese (2004)** and **Cope & Punt (2009)**.

### **Key Features in v0.1.9**
*   **Enhanced Reliability:** Direct user input for `Linf` and `Lmat` is now supported to improve model accuracy and allow for sensitivity analysis.
*   **Statistical Robustness:** Integrated **Monte Carlo** simulations and **non-parametric bootstrapping** for reliable parameter estimation.
*   **New Functionality:** 
    *   `LWR`: Fits and visualizes length-weight relationships.
    *   `FreqTM`: Handles multi-month length-frequency data structures.
    *   Updated `FrequencyTable`, `FishPar`, and `FishSS` with automated decision-making logic for selectivity.

---

## **Installation**

#installing pacakge from CRAN
#install.packages("aLBI")
## Key Features in v0.1.9
Enhanced Reliability: Direct user input for Linf and Lmat is now supported to improve model accuracy and allow for sensitivity analysis.

## Statistical Robustness: Integrated Monte Carlo simulations and non-parametric bootstrapping for reliable parameter estimation.

## New Functionality:

- LWR: Fits and visualizes length-weight relationships.

- FreqTM: Handles multi-month length-frequency data structures.

- Updated FrequencyTable, FishPar, and FishSS with automated decision-making logic for selectivity.

## Installation
Install the stable version from CRAN:

#install.packages("aLBI")

For the latest development version with advanced sensitivity features, install from GitHub:

## install.packages("devtools")
devtools::install_github("Ataher76/aLBI", force = TRUE)
Quick Start
R
library(aLBI)

# 1. Estimate length parameters and sustainability indicators
# Supports user-defined Linf and Lmat for sensitivity analysis
results <- FishPar(data = your_data, resample = 1000, Linf = NULL, Lmat = NULL)

# 2. View results
results$estimated_length_par
results$estimated_froese_par

# 3. Assess stock status
StockStatus <- FishSS(data = CPdata, 
                      LM_ratio = results$LM_ratio, 
                      Pmat = results$estimated_froese_par[1,2], 
                      Popt = results$estimated_froese_par[2,2])
How to Cite
If you use aLBI in your research, please cite both the software and the methodology paper:

Journal Article:

Ali, A., Sarker, M. R., & Alam, M. S. (2025). Development of a simple R package (aLBI) for the estimation of stock status from the length frequency data. Fisheries Research, 288, 107467. https://doi.org/10.1016/j.fishres.2025.107467

Software:

Ali, A., & Alam, M. S. (2024). aLBI: Estimating Length-Based Indicators for Fish Stock (Version 0.1.9).

Contact
Ataher Ali – University of Chittagong, Bangladesh

Email: ataher.cu.ms@gmail.com

GitHub: Ataher76
