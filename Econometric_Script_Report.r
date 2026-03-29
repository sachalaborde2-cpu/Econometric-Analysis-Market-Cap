# Load necessary libraries 
library(dplyr)
library(plm)      
# a. DATA IMPORT
case3_dataframe <- read.csv("/Users/sacha/Documents/M1_MBFA/eco_appliquée/TD/group34.csv")
summary(case3_dataframe) # Display initial summary statistics

# NOTE: THIS IS ONLY FOR THE ECONOMETRICS PART; ADD STATISTICS PART WITH PLOTS JUST BEFORE



# 1 : Tableau pour Stats Des qui présente les variables 
library(dplyr)
library(moments)

vec_stats <- function(x) {
  c(
    Mean      = mean(x, na.rm = TRUE),
    Median    = median(x, na.rm = TRUE),
    Min       = min(x, na.rm = TRUE),
    Max       = max(x, na.rm = TRUE),
    SD        = sd(x, na.rm = TRUE),
    Skewness  = moments::skewness(x, na.rm = TRUE),
    Kurtosis  = moments::kurtosis(x, na.rm = TRUE) + 3
  )
}

# Sélectionner seulement les colonnes numériques
numeric_df <- case3_dataframe %>% select(where(is.numeric))

# Calculer stats pour chaque variable
desc_table <- t(apply(numeric_df, 2, vec_stats))

# Arrondir
desc_table <- round(desc_table, 3)

desc_table
# 2 : Histogramme Marketcap 2006 vs 2007 
library(dplyr)
library(ggplot2)

# Harmoniser le nom de la colonne marketcap
nm <- names(case3_dataframe)
marketcap_col <- intersect(c("marketcap", "mktcap", "mkt_cap"), nm)[1]
year_col <- intersect(c("year", "Year"), nm)[1]

if (is.na(marketcap_col) || is.na(year_col)) {
  stop("Colonnes requises (marketcap, year) non trouvées")
}

# Préparer le dataframe simplifié
df_std <- case3_dataframe %>%
  select(all_of(c(year_col, marketcap_col))) %>%
  setNames(c("year", "marketcap")) %>%
  filter(year %in% c(2006, 2007))

# Créer un facteur pour l'année
df_std$year <- factor(df_std$year)

# Tracer l'histogramme comparatif
ggplot(df_std, aes(x = marketcap, fill = year)) +
  geom_histogram(position = "dodge", bins = 30, color = "white", alpha = 0.8) +
  scale_fill_manual(values = c("2006" = "#90CAF9", "2007" = "#1E88E5")) +
  labs(title = "Histogramme de la Market Cap : 2006 vs 2007",
       x = "Market Capitalization",
       y = "Nombre d'observations",
       fill = "Année") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 3 : Scatter plot Market Cap vs Profit par âge des firmes
summary(case3_dataframe)

case3_dataframe <- case3_dataframe %>%
  mutate(
    firm_age = cut(
      age,
      breaks = c(-Inf, 5, 20, Inf),
      labels = c("Young (<5 years)", "Medium (5-20 years)", "Old (>20 years)")
    )
  )


p_mk_age <- ggplot(case3_dataframe, aes(x = profit, y = mktcap, color = firm_age)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Market Capitalization vs Profit by Firm Age",
       x = "Profit",
       y = "Market Capitalization",
       color = "Firm Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Afficher le plot
print(p_mk_age)

# Sauvegarder le plot
ggsave("scatter_mktcap_by_firm_age.png", plot = p_mk_age, width = 10, height = 7)







# ECONOMETRIC PART _ STEP 1 DATA TRANSFORMATION AND CLEANING 


# a. DATA TRANSFORMATION
case3_dataframe <- case3_dataframe %>%
  mutate(
    # Dependent Variable
    log_mktcap = log(mktcap),
    
    # Simple logs
    log_emp = log(emp),
    log_inv = log(inv),
    log_shortdebt = log(shortdebt),
    log_nboard = log(nboard),
    age_squared = age^2,
    
    # Ratios
    wboard_ratio = wboard / nboard,
    highdegree_emp_ratio = skemp / emp
  )
summary(case3_dataframe)

# b. DATA CLEANING: Handle Infinite Values and Missing Data
# Convert Inf/-Inf values (resulting from division by zero) to NA
case3_dataframe[sapply(case3_dataframe, is.infinite)] <- NA
# Remove all rows containing NA for any variable used in upcoming regressions
case3_dataframe <- na.omit(case3_dataframe)
summary(case3_dataframe)




# STEP 2. POOLED OLS

# Model 1: financial variables only
model1 <- lm(log_mktcap ~ profit + log_inv + log_shortdebt, data = case3_dataframe)

# Model 2: add firm size and governance
model2 <- lm(log_mktcap ~ profit + log_inv + log_shortdebt + log_emp + log_nboard + wboard_ratio, data = case3_dataframe)

# Model 3: add human capital ratio
model3 <- lm(log_mktcap ~ profit + log_inv + log_shortdebt + log_emp + log_nboard + wboard_ratio + highdegree_emp_ratio, data = case3_dataframe)

# Model 4: add structural effects: sector and age
model4 <- lm(log_mktcap ~ profit + log_inv + log_shortdebt + log_emp + log_nboard + wboard_ratio + highdegree_emp_ratio + age + age_squared + factor(sector), data = case3_dataframe)

# Display summary of models
summary(model1)
summary(model2)
summary(model3)
summary(model4)


# STEP 2bis: Clustered robust SE for Pooled OLS models (cluster by firm 'id')
library(sandwich)
library(lmtest)

# Function to calculate clustered VCOV
cluster_vcov <- function(model, cluster_var) {
  cluster_id <- model$model[[cluster_var]]  # extract clustering variable
  vcovCL(model, cluster = cluster_id)
}

# Apply function to each model
vcov_clustered_model1 <- cluster_vcov(model1, "id")
vcov_clustered_model2 <- cluster_vcov(model2, "id")
vcov_clustered_model3 <- cluster_vcov(model3, "id")
vcov_clustered_model4 <- cluster_vcov(model4, "id")

# Display coefficients with clustered SE
coeftest(model1, vcov. = vcov_clustered_model1)
coeftest(model2, vcov. = vcov_clustered_model2)
coeftest(model3, vcov. = vcov_clustered_model3)
coeftest(model4, vcov. = vcov_clustered_model4)






# STEP 3. PANEL DATA TESTS (Preliminary Tests for FE/RE)

required_packages <- c("plm", "lmtest", "sandwich")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(plm)
library(lmtest)
library(sandwich)

# a. Define data as panel
# Assume 'id' identifies firms and 'year' identifies years
panel_data <- pdata.frame(case3_dataframe, index = c("id", "year"))

# b. Pooled OLS as reference
pooled_model <- plm(log_mktcap ~ profit + log_inv + log_shortdebt + 
                      log_emp + log_nboard + wboard_ratio + highdegree_emp_ratio + age + age_squared + factor(sector), 
                    data = panel_data, model = "pooling")



# d. Fixed Effects vs Random Effects (Hausman test)
fe_model <- plm(log_mktcap ~ profit + log_inv + log_shortdebt + 
                  log_emp + log_nboard + wboard_ratio + highdegree_emp_ratio + age + age_squared + factor(sector),
                data = panel_data, model = "within")

re_model <- plm(log_mktcap ~ profit + log_inv + log_shortdebt + 
                  log_emp + log_nboard + wboard_ratio + highdegree_emp_ratio + age + age_squared + factor(sector),
                data = panel_data, model = "random")

hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

# f. Test for heteroskedasticity (Breusch-Pagan modified for panel) (in annex)
bptest_panel <- bptest(pooled_model, studentize = TRUE)
print(bptest_panel)


# g. Summary of tests
cat("These four tests check:\n")
cat("- Presence of significant individual effects\n")
cat("- Correlation of effects with explanatory variables (Hausman)\n")
cat("- Intra-firm residual autocorrelation\n")
cat("- Heteroskedasticity across firms\n")
cat("If tests are significant: justification for FE + clustered robust SE.\n")





# STEP 4: Individual Fixed Effects Model (within)
pdata <- pdata.frame(case3_dataframe, index = c("id", "year"))

# Remove sector and emp as they are constant within a firm
m_within <- plm(log_mktcap ~ profit + log_inv + log_shortdebt + log_nboard + 
                  wboard_ratio + highdegree_emp_ratio + age_squared + factor(sector), 
                data = pdata, model = "within", effect = "individual")
summary(m_within)





# STEP 5: Combined Fixed Effects (individual FE + time FE)
m_fe_combined <- plm(log_mktcap ~ profit + log_inv + log_shortdebt + log_emp + log_nboard + 
                       wboard_ratio + highdegree_emp_ratio + age_squared + factor(sector) + factor(year),
                     data = pdata, model = "within")

# Clustered robust SE
m_within_robust <- coeftest(m_within, vcov = vcovHC(m_within, type = "HC1", cluster = "group"))
m_fe_combined_robust <- coeftest(m_fe_combined, vcov = vcovHC(m_fe_combined, type = "HC1", cluster = "group"))

# Display results
m_within_robust
m_fe_combined_robust



cat("\n==================== POOLED OLS (Clustered SE) ====================\n")
cat("\n--- Model 1 ---\n")
print(coeftest(model1, vcov. = vcov_clustered_model1))

cat("\n--- Model 2 ---\n")
print(coeftest(model2, vcov. = vcov_clustered_model2))

cat("\n--- Model 3 ---\n")
print(coeftest(model3, vcov. = vcov_clustered_model3))

cat("\n--- Model 4 ---\n")
print(coeftest(model4, vcov. = vcov_clustered_model4))



cat("\n==================== FIXED EFFECTS (WITHIN) ====================\n")
print(m_within_robust)



cat("\n==================== FIXED EFFECTS + TIME EFFECTS ====================\n")
print(m_fe_combined_robust)












# A REVOIR POUR LE TABLEAU 

# Installer et charger modelsummary si ce n'est pas déjà fait
if (!require(modelsummary)) install.packages("modelsummary")
library(modelsummary)

# Préparer les modèles avec leurs SE robustes / clusterisées
# Clustered SE pour OLS
ols_models <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model4
)

ols_vcov <- list(
  vcov_clustered_model1,
  vcov_clustered_model2,
  vcov_clustered_model3,
  vcov_clustered_model4
)

# FE models (clustered robust)
fe_models <- list(
  "Within FE" = m_within,
  "FE + Time" = m_fe_combined
)

fe_vcov <- list(
  vcovHC(m_within, type = "HC1", cluster = "group"),
  vcovHC(m_fe_combined, type = "HC1", cluster = "group")
)

# Combiner tous les modèles
all_models <- c(ols_models, fe_models)
all_vcov <- c(ols_vcov, fe_vcov)

# Fonction pour arrondir à 5 chiffres après la virgule
round5 <- function(x) sprintf("%.5f", x)

# Créer le tableau sans logLik, RMSE, sigma, mais en gardant R2 et R2 ajusté
modelsummary(
  all_models,
  vcov = all_vcov,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = round5,  # coefficients à 5 décimales
  gof_omit = "AIC|BIC|logLik|RMSE|Sigma|Std",  # enlever seulement ces stats
  title = "Estimation des modèles Pooled OLS et Fixed Effects"
)

# Export LaTeX prêt pour Overleaf


modelsummary(
  all_models,
  vcov = all_vcov,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = round5, 
  gof_omit = "AIC|BIC|logLik|RMSE|Sigma|Std", 
  title = "Estimation des modèles Pooled OLS et Fixed Effects",
  # COMMANDE CLÉ: Spécifie le format de sortie et le nom du fichier LaTeX
  output = "comparative_panel_models.tex" 
)



# Des nouveaux models 
case3_dataframe <- case3_dataframe %>%
  mutate(
  
    
    # Simple logs
    emp_squared = emp**2,
    inv_squared = inv**2,
    shortdebt_squared = shortdebt**2
    
    
  )
summary(case3_dataframe)











