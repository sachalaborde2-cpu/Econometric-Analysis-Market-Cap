# --- PART 3: PANEL DATA ANALYSIS (OLS, WITHIN, RANDOM & DIAGNOSTICS) ---

# Load necessary libraries (Assumed installed)
library(dplyr)
library(plm)      

# 1. DATA IMPORT AND TRANSFORMATION
# WARNING: Absolute path must be changed to "group34.csv" or a relative path in final document.
case3_dataframe <- read.csv("/Users/sacha/Documents/M1_MBFA/eco_appliquée/TD/group34.csv")
summary(case3_dataframe) # Display initial summary statistics.

case3_dataframe <- case3_dataframe %>%
  mutate(
    # Dependent Variable (DV): Log-transformed Market Capitalization
    log_mktcap = log(mktcap),
    # Control variable: Log-transformed employees (Size proxy)
    log_emp = log(emp),
    log_inv = log(inv),
    log_shortdebt =log(shortdebt),
    log_nboard = log(nboard),
    
    # Ratios (IVs)
    wboard_ratio = wboard / nboard,
    highdegree_emp_ratio = skemp / emp,
    # Return on Investment (for reference)
    ROI = profit / inv  
  )

# 2. DATA CLEANING: Handle Infinite Values and Missing Data
# Convert Inf/-Inf values (resulting from division by zero) to NA.
case3_dataframe[sapply(case3_dataframe, is.infinite)] <- NA
# Remove all rows containing NA for any variable used in the upcoming regressions.
case3_dataframe <- na.omit(case3_dataframe)


# 3. ESTIMATION OF POOLED OLS MODELS (Baseline & Time Fixed Effects)

# Pooled OLS (Classic MCO): No control for firm or time effects.
lm_pooled<-lm(log_mktcap ~ profit + inv + ROI + nboard + wboard_ratio + emp + highdegree_emp_ratio + shortdebt + sector, data = case3_dataframe)
summary(lm_pooled)

# Pooled OLS with Time Fixed Effects: Manually controls for time trends (factor(year)).
lm_fixed_effects<-lm(log_mktcap ~ profit + inv + ROI + nboard + wboard_ratio + emp + highdegree_emp_ratio + shortdebt + sector + factor(year), data = case3_dataframe)
summary(lm_fixed_effects)
# Comment: The effect of manually adding a time fixed effect is negligible in this specification.


# 4. ESTIMATION OF PANEL DATA MODELS (Within and Random)
# Define the common formula for the panel models.
panel_formula <- mktcap ~ profit + inv  + nboard + wboard_ratio + emp + highdegree_emp_ratio + shortdebt + factor(sector) + age

# Fixed Effects Model (Within Estimator): Controls for unobserved time-invariant firm heterogeneity.
model_within <- plm(panel_formula, 
                    data = case3_dataframe, 
                    model = "within", 
                    index = c("id","year"))
summary(model_within)
# Comment: Variables like 'emp' and 'sector' are omitted/absorbed because they are perfectly collinear 
# with the firm-specific intercept (they are time-invariant or near-invariant).

# Random Effects Model: Assumes individual effects are uncorrelated with regressors.
model_random <- plm(panel_formula, 
                    data = case3_dataframe, 
                    model = "random", 
                    index = c("id","year"))
summary(model_random)


# 5. DIAGNOSTIC TEST: HAUSMAN TEST (FE vs. RE Choice)
# Test H0: Random Effects (RE) are consistent (no correlation between individual effects and regressors).
pht <- phtest(model_random, model_within)
print(pht)

# Interpretation: Since p-value (0.994) is > 0.05 (or 0.1), we DO NOT reject H0.
# Conclusion: The Random Effects (RE) model is consistent and more efficient than FE.
# This suggests that the OVB issue related to time-invariant factors is less severe than expected
# or already accounted for, making the RE estimation the better choice.


