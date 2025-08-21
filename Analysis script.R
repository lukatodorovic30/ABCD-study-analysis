# Title: Bidirectional associations between mental health and problematic digital media use in adolescence
# Authors: L. Todorovic, H. Bozhar, S. de Rooij, A. Bogaerts, B. Boyer, H. Larsen
# Data: Amsterdam-Born Children and their Development (ABCD) cohort, waves 5 & 6 (2019, 2021)
# Preregistration: AsPredicted.org (156163)

# PACKAGES ####
library(haven)
library(psych)
library(dplyr)
library(VIM)
library(car)
library(gridExtra)
library(corrplot)
library(ggplot2)
library(semPlot)
library(naniar)
library(lavaan)
library(semTools)
library(kableExtra)
library(magrittr)
library(knitr)
library(mice)
library(semPower)
library(nortest)
library(lmtest)

# MERGING & SUBSETTING ####
## Load dataframes
gender_at_birth <- read_sav("geslacht-geboorte.sav")
phase_5 <- read_sav("35-Kind-VRAGENLIJST-FASE5.sav")
phase_6 <- read_sav("39-Kind-VRAGENLIJST-FASE6.sav")

## Investigate number of observations
### Find common IDs (shared between datasets)
common_ids <- intersect(phase_5$ID, phase_6$ID)
cat("Number of common IDs: ", length(common_ids), "\n")
### Find IDs unique to phase 5
unique_ids_phase_5 <- setdiff(phase_5$ID, phase_6$ID)
cat("Number of unique IDs in phase_5: ", length(unique_ids_phase_5), "\n")
### Find IDs unique to phase 6
unique_ids_phase_6 <- setdiff(phase_6$ID, phase_5$ID)
cat("Number of unique IDs in phase_6: ", length(unique_ids_phase_6), "\n")
### Total IDs (shared IDs + unique IDs phase 5 + unique IDs phase 6)
cat("Total unique IDs (combined): ", length(unique(c(phase_5$ID, phase_6$ID))), "\n")

## Merge dataframes 
merged_dataset <- merge(x = merge(x = phase_5, y = phase_6, by = "ID", all = TRUE),
                        y = gender_at_birth, by = "ID", all.x = TRUE)
### Add gender-at-birth to NA gender (NA is left from phase 5 where gender variable was not available)
merged_dataset$geslacht.x[is.na(merged_dataset$geslacht.x)] <- merged_dataset$geslacht.y[is.na(merged_dataset$geslacht.x)]

## Subsetting
## List of all variables in the dataset
variable_names <- names(merged_dataset)
for (i in seq_along(variable_names)) {
  cat(i, ": ", variable_names[i], "\n")
}
## Narrow down the dataframe
### Subset important variables (gender, age, school, digital media use, SDQ subscales, SMDS, IGDS, MSPSS, RSES)
narrowed_data <- merged_dataset[, c(1:2, 53:69, 80:82, 93:95, 103, 105:107, 109, 113, 116, 
                                    203:217, 228:240, 251, 259:262, 269, 271:272, 274)]
### List of variables in the narrowed dataset
variable_names <- names(narrowed_data)
for (i in seq_along(variable_names)) {
  cat(i, ": ", variable_names[i], "\n")
}        

### Check for missing values in each column
missing_summary <- colSums(is.na(narrowed_data)) / nrow(narrowed_data) * 100
print(missing_summary)
NA_summary_table <- data.frame(
  Variable = names(narrowed_data),
  Missing_Values = sapply(narrowed_data, function(x) sum(is.na(x))),
  Not_Missing_Values = sapply(narrowed_data, function(x) sum(!is.na(x))),
  Total_Values = sapply(narrowed_data, function(x) length(x)),
  Percentage_Missing = sapply(narrowed_data, function(x) mean(is.na(x)) * 100)
)
print(NA_summary_table)
#### Visualize missing data
aggr_plot <- aggr(narrowed_data, col=c('blue', 'red'), numbers=TRUE, sortVars=TRUE)

## Subset main analysis variables (age, emotions, conduct, hyper, prosocial, support, SMDS, IGDS, gender, x2 waves)
analysis_data <- narrowed_data[, c(2, 26:32, 62:69)]
### View variables in the dataset
variable_names <- names(analysis_data)
for (i in seq_along(variable_names)) {
  cat(i, ": ", variable_names[i], "\n")
} 


# MISSING DATA ####
## Explore missing data

## optional ## 
### Different filtering options to include only participants with complete data (on chosen variables) and show NA %
#### option 1 (complete cases)
filtered_dataset1 <- analysis_data[complete.cases(analysis_data$SMDS_som_f5, analysis_data$IGDS_som_f5, 
                                                  analysis_data$IGDS_som_calc_f6, analysis_data$SMDS_som_calc_f6), ]
colSums(is.na(filtered_dataset1)) / nrow(filtered_dataset1) * 100
#### option 2 (NA only on IGDS f6)
filtered_dataset2 <- analysis_data[complete.cases(analysis_data$SMDS_som_f5, analysis_data$IGDS_som_f5, 
                                                  analysis_data$SMDS_som_calc_f6), ]
colSums(is.na(filtered_dataset2)) / nrow(filtered_dataset2) * 100
#### option 3 (NA only in f6)
filtered_dataset3 <- analysis_data[complete.cases(analysis_data$SMDS_som_f5, analysis_data$IGDS_som_f5), ]
colSums(is.na(filtered_dataset3)) / nrow(filtered_dataset3) * 100


### check number of "fakeNA" on SMDS and IGDS phase 6 (by exclusion rules, item 75 & 90)
#### For SMDS_som_calc_f6 and F6PQ75
na_smds_with_1_f6pq75 <- sum(is.na(narrowed_data$SMDS_som_calc_f6) & narrowed_data$F6PQ75 == 1, na.rm = TRUE)
na_smds_with_not_1_f6pq75 <- sum(is.na(narrowed_data$SMDS_som_calc_f6) & narrowed_data$F6PQ75 != 1 & !is.na(narrowed_data$F6PQ75), na.rm = TRUE)

cat("NAs in SMDS_som_calc_f6 with a score of '1' on F6PQ75:", na_smds_with_1_f6pq75, "\n")
cat("NAs in SMDS_som_calc_f6 with a score different from '1' on F6PQ75:", na_smds_with_not_1_f6pq75, "\n")

# For IGDS_som_calc_f6 and F6PQ90
na_igds_with_1_f6pq90 <- sum(is.na(narrowed_data$IGDS_som_calc_f6) & narrowed_data$F6PQ90 == 1, na.rm = TRUE)
na_igds_with_not_1_f6pq90 <- sum(is.na(narrowed_data$IGDS_som_calc_f6) & narrowed_data$F6PQ90 != 1 & !is.na(narrowed_data$F6PQ90), na.rm = TRUE)

cat("NAs in IGDS_som_calc_f6 with a score of '1' on F6PQ90:", na_igds_with_1_f6pq90, "\n")
cat("NAs in IGDS_som_calc_f6 with a score different from '1' on F6PQ90:", na_igds_with_not_1_f6pq90, "\n")

### Check for missing values in each column
missing_summary <- colSums(is.na(analysis_data)) / nrow(analysis_data) * 100
print(missing_summary)
summary_table <- data.frame(
  Variable = names(analysis_data),
  Missing_Values = sapply(analysis_data, function(x) sum(is.na(x))),
  Not_Missing_Values = sapply(analysis_data, function(x) sum(!is.na(x))),
  Total_Values = sapply(analysis_data, function(x) length(x)),
  Percentage_Missing = sapply(analysis_data, function(x) mean(is.na(x)) * 100)
)
print(summary_table)
### Little's test for MCAR
mcar_test(analysis_data)

## Visualize missing data
### Missingness % and patterns
aggr_plot <- aggr(analysis_data, col=c('blue', 'red'), numbers=TRUE, sortVars=TRUE)
aggr(analysis_data, combined = TRUE, numbers = TRUE)
### Visualize with paired correlations
missing_correlations <- cor(analysis_data, use="pairwise.complete.obs", method="pearson")
corrplot(missing_correlations, method = "circle")
### Visualize with spinogram and/or spineplot 
spineMiss(analysis_data[, c("geslacht.y", "IGDS_som_calc_f6")])
spineMiss(analysis_data[, c("geslacht.y", "SMDS_som_calc_f6")])
spineMiss(analysis_data[, c("geslacht.y", "semotion_f6")])
spineMiss(analysis_data[, c("geslacht.y", "sconduct_f6")])
spineMiss(analysis_data[, c("geslacht.y", "shyper_f6")])
spineMiss(analysis_data[, c("geslacht.y", "sprosoc_f6")])
spineMiss(analysis_data[, c("geslacht.y", "SMDS_som_f5")])
spineMiss(analysis_data[, c("geslacht.y", "IGDS_som_f5")])
spineMiss(analysis_data[, c("geslacht.y", "MSPSS_totaal_som_f6")])

spineMiss(analysis_data[, c("F5leeftijd_kind", "IGDS_som_calc_f6")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "SMDS_som_calc_f6")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "semotion_f6")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "sconduct_f6")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "shyper_f6")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "sprosoc_f6")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "SMDS_som_f5")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "IGDS_som_f5")])
spineMiss(analysis_data[, c("F5leeftijd_kind", "MSPSS_totaal_som_f6")])
#### ...

### Visualise with boxplots
analysis_data_visualisations <- analysis_data %>%
  mutate(log_IGDS_som_calc_f6 = log(IGDS_som_calc_f6))
pbox(analysis_data_visualisations[, c("log_IGDS_som_calc_f6", "F5leeftijd_kind")])

### Visualise with parallel coordinate plots
parcoordMiss(analysis_data, highlight = 'IGDS_som_calc_f6', alpha = 0.6)
parcoordMiss(analysis_data, highlight = 'SMDS_som_calc_f6', alpha = 0.6)
parcoordMiss(analysis_data, highlight = 'semotion_f6', alpha = 0.6)
parcoordMiss(analysis_data, highlight = 'IGDS_som_f5', alpha = 0.6)
parcoordMiss(analysis_data, highlight = 'SMDS_som_f5', alpha = 0.6)
parcoordMiss(analysis_data, highlight = 'semotion_f5', alpha = 0.6)

### Visualise with a matrix plot
matrixplot(analysis_data, sortby = c('IGDS_som_calc_f6'))
matrixplot(analysis_data, sortby = c('semotion_f6'))
#### ...  


# POWER ANALYSES ####

### post-hoc power for CCA (CLPM Base model)
## Defining the model parameters
# number of waves
nWaves <- 2
# Autoregressive effects
autoregEffects <- c(0.666, 0.501, 0.679, 0.517, 0.307, 0.426)  # based on model output (CLPM Base)
# Cross-lagged effects
crossedEffects <- c(0.007, -0.029, 0.052, 0.039, 0.025, -0.029, -0.063, -0.055, 0.172, -0.002, 0.003, -0.034, 0.152, 0.043, 0.001, -0.018)  # based on model output
# Correlations for two waves (Adjusted to length 2)
rXY <- c(0.151, 0.349)  # 

## Run the post-hoc power analysis
post_hoc_power <- semPower.powerCLPM(
  type = 'post-hoc',
  N = 645,
  alpha = 0.05,
  nullEffect = 'crossedX = 0',  # hypothesis: cross-lagged effect of X on Y differs from zero
  nWaves = nWaves,
  autoregEffects = autoregEffects[1:2],  # Adjusting to match nWaves
  crossedEffects = crossedEffects[1:2],  # Adjusting to match nWaves
  rXY = rXY,  # Adjusted to length 2
  nIndicator = c(5, 3, 5, 3),  # Number of indicators for X and Y at each wave
  loadM = c(0.5, 0.6, 0.5, 0.6) # Loadings for X and Y at each wave
)
# Post-hoc power analysis results
summary(post_hoc_power)

### a-priori power for CLPM Base model
## Defining the model parameters
# number of waves
nWaves <- 2
# Autoregressive effects: define medium effect sizes
autoregEffects <- c(0.50, 0.50)  # Medium autoregressive effects
# Cross-lagged effects: define medium effect sizes
crossedEffects <- c(0.30, 0.30)  # Medium cross-lagged effects
# Correlations for two waves
rXY <- c(0.30, 0.30) # Medium correlations
# Define the number of indicators for X and Y at each wave
nIndicator <- c(5, 3, 5, 3)  # Based on model structure
# Define the loadings for X and Y at each wave
loadM <- c(0.5, 0.6, 0.5, 0.6)  # Based on model structure

## Run the a-priori power analysis
apriori_power <- semPower.powerCLPM(
  type = 'a-priori',
  power = 0.80,  # Desired power level
  alpha = 0.05,  # Alpha level
  nullEffect = 'crossedX = 0',  # Hypothesis: cross-lagged effect of X on Y differs from zero
  nWaves = nWaves,
  autoregEffects = autoregEffects,
  crossedEffects = crossedEffects,
  rXY = rXY,
  nIndicator = nIndicator,
  loadM = loadM
)
# A-priori power analysis results
summary(apriori_power)

### Sample size required for detecting small effects (0.1) with good power (80%)
## Defining the model parameters
# Number of waves
nWaves <- 2
# Autoregressive effects
autoregEffects <- c(0.50, 0.50) # medium
# Cross-lagged effects
crossedEffects <- c(0.10, 0.10)  # small to medium
# Correlations for two waves
rXY <- c(0.30, 0.30)  # medium
# Define the number of indicators for X and Y at each wave
nIndicator <- c(5, 3, 5, 3)
# Define the loadings for X and Y at each wave
loadM <- c(0.5, 0.6, 0.5, 0.6)

## Run the a-priori power analysis to determine detectable effect size
a_priori_effect_size <- semPower.powerCLPM(
  type = 'a-priori',
  power = 0.80,  # Desired power level
  alpha = 0.05,  # Alpha level
  nullEffect = 'crossedX = 0',  # Hypothesis: cross-lagged effect of X on Y differs from zero
  nWaves = nWaves,
  autoregEffects = autoregEffects,
  crossedEffects = crossedEffects,  
  rXY = rXY,
  nIndicator = nIndicator,
  loadM = loadM
)
# Print the power analysis results
summary(a_priori_effect_size)

### iterative run to get to  n = 645 with 80% power and see what effect size is detectable with it
# Function to find the smallest detectable effect size
find_effect_size <- function(target_n, power, alpha, nWaves, autoregEffects, rXY, nIndicator, loadM) {
  effect_size <- 0.05
  step <- 0.01
  while(TRUE) {
    power_analysis <- semPower.powerCLPM(
      type = 'a-priori',
      power = power,
      alpha = alpha,
      nullEffect = 'crossedX = 0',
      nWaves = nWaves,
      autoregEffects = autoregEffects,
      crossedEffects = c(effect_size, effect_size),
      rXY = rXY,
      nIndicator = nIndicator,
      loadM = loadM
    )
    if (power_analysis$requiredN <= target_n) {
      break
    }
    effect_size <- effect_size + step
  }
  return(effect_size)
}
# Find the smallest detectable effect size for N = 645
smallest_effect_size <- find_effect_size(
  target_n = 645,
  power = 0.80,
  alpha = 0.05,
  nWaves = nWaves,
  autoregEffects = autoregEffects,
  rXY = rXY,
  nIndicator = nIndicator,
  loadM = loadM
)
# Smallest detectable cross-lag effect size with power 80% and n = 645
print(smallest_effect_size)


# ASSUMPTION CHECKS ####
### Subset data with participants with no missing data
sum(apply(analysis_data, 1, function(row) all(!is.na(row))))
CCA_data <- analysis_data[apply(analysis_data, 1, function(row) all(!is.na(row))), ]
save(CCA_data, file = "CCA_data.RData")

## Measurement invariance 
# Define the configural model with intercepts
configural_model_with_intercepts <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c1*SMDS_som_f5 + c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5 + c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5 + c6*IGDS_som_f5
  sprosoc_f6 ~ c7*SMDS_som_f5 + c8*IGDS_som_f5
  SMDS_som_calc_f6 ~ c9*semotion_f5 + c10*sconduct_f5 + c11*shyper_f5 + c12*sprosoc_f5 
  IGDS_som_calc_f6 ~ c13*semotion_f5 + c14*sconduct_f5 + c15*shyper_f5 + c16*sprosoc_f5 

  # Intercepts
  semotion_f5 ~ 1
  sconduct_f5 ~ 1
  shyper_f5 ~ 1
  sprosoc_f5 ~ 1
  SMDS_som_f5 ~ 1
  IGDS_som_f5 ~ 1

  semotion_f6 ~ 1
  sconduct_f6 ~ 1
  shyper_f6 ~ 1
  sprosoc_f6 ~ 1
  SMDS_som_calc_f6 ~ 1
  IGDS_som_calc_f6 ~ 1

  # Residual correlations
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"

# Fit the modified configural model
fit_configural_with_intercepts <- sem(configural_model_with_intercepts, data = CCA_data)

# Get the summary of the configural model fit including fit indices
summary(fit_configural_with_intercepts, fit.measures = TRUE)


# Define the metric model by constraining loadings
metric_model_with_intercepts <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c1*SMDS_som_f5 + c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5 + c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5 + c6*IGDS_som_f5
  sprosoc_f6 ~ c7*SMDS_som_f5 + c8*IGDS_som_f5
  SMDS_som_calc_f6 ~ c9*semotion_f5 + c10*sconduct_f5 + c11*shyper_f5 + c12*sprosoc_f5 
  IGDS_som_calc_f6 ~ c13*semotion_f5 + c14*sconduct_f5 + c15*shyper_f5 + c16*sprosoc_f5 

  # Intercepts
  semotion_f5 ~ 1
  sconduct_f5 ~ 1
  shyper_f5 ~ 1
  sprosoc_f5 ~ 1
  SMDS_som_f5 ~ 1
  IGDS_som_f5 ~ 1

  semotion_f6 ~ 1
  sconduct_f6 ~ 1
  shyper_f6 ~ 1
  sprosoc_f6 ~ 1
  SMDS_som_calc_f6 ~ 1
  IGDS_som_calc_f6 ~ 1

  # Residual correlations
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"

# Fit the modified metric model
fit_metric_with_intercepts <- sem(metric_model_with_intercepts, data = CCA_data)

# Compare the modified configural and modified metric models
anova(fit_configural_with_intercepts, fit_metric_with_intercepts)

# Define the scalar invariance model by constraining intercepts and residuals
scalar_model_with_intercepts <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c1*SMDS_som_f5 + c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5 + c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5 + c6*IGDS_som_f5
  sprosoc_f6 ~ c7*SMDS_som_f5 + c8*IGDS_som_f5
  SMDS_som_calc_f6 ~ c9*semotion_f5 + c10*sconduct_f5 + c11*shyper_f5 + c12*sprosoc_f5 
  IGDS_som_calc_f6 ~ c13*semotion_f5 + c14*sconduct_f5 + c15*shyper_f5 + c16*sprosoc_f5 

  # Intercepts
  semotion_f5 ~ d1*1
  sconduct_f5 ~ d2*1
  shyper_f5 ~ d3*1
  sprosoc_f5 ~ d4*1
  SMDS_som_f5 ~ d5*1
  IGDS_som_f5 ~ d6*1

  semotion_f6 ~ d1*1
  sconduct_f6 ~ d2*1
  shyper_f6 ~ d3*1
  sprosoc_f6 ~ d4*1
  SMDS_som_calc_f6 ~ d5*1
  IGDS_som_calc_f6 ~ d6*1

  # Residual correlations
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"

# Fit the scalar model with intercepts
fit_scalar_with_intercepts <- sem(scalar_model_with_intercepts, data = CCA_data)

# Compare metric and scalar models
anova(fit_metric_with_intercepts, fit_scalar_with_intercepts)


### Multicollinearity
# Fit a linear model for each dependent variable to calculate VIF
fit_vif_emotion <- lm(semotion_f6 ~ semotion_f5 + sconduct_f5 + shyper_f5 + sprosoc_f5 + SMDS_som_f5 + IGDS_som_f5, data = CCA_data)
fit_vif_conduct <- lm(sconduct_f6 ~ semotion_f5 + sconduct_f5 + shyper_f5 + sprosoc_f5 + SMDS_som_f5 + IGDS_som_f5, data = CCA_data)
fit_vif_hyper <- lm(shyper_f6 ~ semotion_f5 + sconduct_f5 + shyper_f5 + sprosoc_f5 + SMDS_som_f5 + IGDS_som_f5, data = CCA_data)
fit_vif_prosoc <- lm(sprosoc_f6 ~ semotion_f5 + sconduct_f5 + shyper_f5 + sprosoc_f5 + SMDS_som_f5 + IGDS_som_f5, data = CCA_data)
fit_vif_smds <- lm(SMDS_som_calc_f6 ~ semotion_f5 + sconduct_f5 + shyper_f5 + sprosoc_f5 + SMDS_som_f5 + IGDS_som_f5, data = CCA_data)
fit_vif_igds <- lm(IGDS_som_calc_f6 ~ semotion_f5 + sconduct_f5 + shyper_f5 + sprosoc_f5 + SMDS_som_f5 + IGDS_som_f5, data = CCA_data)

# Calculate VIF for models
vif(fit_vif_emotion)
vif(fit_vif_conduct)
vif(fit_vif_hyper)
vif(fit_vif_prosoc)
vif(fit_vif_smds)
vif(fit_vif_igds)


### Linearity
# List of predictor-outcome pairs
pairs <- list(
  c("semotion_f5", "semotion_f6"),
  c("sconduct_f5", "sconduct_f6"),
  c("shyper_f5", "shyper_f6"),
  c("sprosoc_f5", "sprosoc_f6"),
  c("SMDS_som_f5", "SMDS_som_calc_f6"),
  c("IGDS_som_f5", "IGDS_som_calc_f6"),
  c("SMDS_som_f5", "semotion_f6"),
  c("IGDS_som_f5", "semotion_f6"),
  c("SMDS_som_f5", "sconduct_f6"),
  c("IGDS_som_f5", "sconduct_f6"),
  c("SMDS_som_f5", "shyper_f6"),
  c("IGDS_som_f5", "shyper_f6"),
  c("SMDS_som_f5", "sprosoc_f6"),
  c("IGDS_som_f5", "sprosoc_f6"),
  c("semotion_f5", "SMDS_som_calc_f6"),
  c("sconduct_f5", "SMDS_som_calc_f6"),
  c("shyper_f5", "SMDS_som_calc_f6"),
  c("sprosoc_f5", "SMDS_som_calc_f6"),
  c("semotion_f5", "IGDS_som_calc_f6"),
  c("sconduct_f5", "IGDS_som_calc_f6"),
  c("shyper_f5", "IGDS_som_calc_f6"),
  c("sprosoc_f5", "IGDS_som_calc_f6")
)

# Create scatter plots for each pair
plot_list <- lapply(pairs, function(pair) {
  ggplot(CCA_data, aes_string(x = pair[1], y = pair[2])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste(pair[1], "vs", pair[2]))
})

# Arrange plots in a grid
do.call(grid.arrange, c(plot_list, ncol = 3))


### Homoscedasticity
# List of predictor-outcome pairs
pairs <- list(
  c("semotion_f5", "semotion_f6"),
  c("sconduct_f5", "sconduct_f6"),
  c("shyper_f5", "shyper_f6"),
  c("sprosoc_f5", "sprosoc_f6"),
  c("SMDS_som_f5", "SMDS_som_calc_f6"),
  c("IGDS_som_f5", "IGDS_som_calc_f6"),
  c("SMDS_som_f5", "semotion_f6"),
  c("IGDS_som_f5", "semotion_f6"),
  c("SMDS_som_f5", "sconduct_f6"),
  c("IGDS_som_f5", "sconduct_f6"),
  c("SMDS_som_f5", "shyper_f6"),
  c("IGDS_som_f5", "shyper_f6"),
  c("SMDS_som_f5", "sprosoc_f6"),
  c("IGDS_som_f5", "sprosoc_f6"),
  c("semotion_f5", "SMDS_som_calc_f6"),
  c("sconduct_f5", "SMDS_som_calc_f6"),
  c("shyper_f5", "SMDS_som_calc_f6"),
  c("sprosoc_f5", "SMDS_som_calc_f6"),
  c("semotion_f5", "IGDS_som_calc_f6"),
  c("sconduct_f5", "IGDS_som_calc_f6"),
  c("shyper_f5", "IGDS_som_calc_f6"),
  c("sprosoc_f5", "IGDS_som_calc_f6")
)

# Fit linear models for each pair
models <- lapply(pairs, function(pair) {
  lm(as.formula(paste(pair[2], "~", pair[1])), data = CCA_data)
})

# Function to plot residuals vs fitted values
plot_residuals <- function(model, title) {
  residuals <- residuals(model)
  fitted <- fitted(model)
  ggplot(data = data.frame(fitted = fitted, residuals = residuals), aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = title, x = "Fitted Values", y = "Residuals")
}

# Plot for each model
plot_list <- lapply(1:length(models), function(i) {
  plot_residuals(models[[i]], paste("Residuals vs Fitted for Model", i))
})

# Arrange plots in a grid
do.call(grid.arrange, c(plot_list, ncol = 3))

# Perform Breusch-Pagan test for each model and store results
bp_tests <- lapply(models, function(model) {
  bptest(model)
})

# Print Breusch-Pagan test results
for (i in 1:length(bp_tests)) {
  cat(paste("Breusch-Pagan test for Model", i, ":\n"))
  print(bp_tests[[i]])
  cat("\n")
}


### Normality of Residuals
# Function to create Q-Q plot
plot_qq <- function(model, title) {
  residuals <- residuals(model)
  ggplot(data = data.frame(sample = residuals), aes(sample = sample)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles")
}

# Create Q-Q plots for each model
qq_plot_list <- lapply(1:length(models), function(i) {
  plot_qq(models[[i]], paste("Q-Q Plot for Model", i))
})

# Arrange Q-Q plots in a grid
do.call(grid.arrange, c(qq_plot_list, ncol = 3))

# Function to perform Shapiro-Wilk test
shapiro_test <- function(model) {
  residuals <- residuals(model)
  shapiro.test(residuals)
}

# Perform Shapiro-Wilk test for each model and store results
shapiro_tests <- lapply(models, function(model) {
  shapiro_test(model)
})

# Print Shapiro-Wilk test results
for (i in 1:length(shapiro_tests)) {
  cat(paste("Shapiro-Wilk test for Model", i, ":\n"))
  print(shapiro_tests[[i]])
  cat("\n")
}

# MAIN ANALYSIS ####
## Create a dataframe with complete cases only
### Number of participants with no missing values
sum(apply(analysis_data, 1, function(row) all(!is.na(row))))
CCA_data <- analysis_data[apply(analysis_data, 1, function(row) all(!is.na(row))), ]
save(CCA_data, file = "CCA_data.RData")


## Cross-lagged panel model (CLPM) with CCA data (n=645); main model
### Specify the model
clpm_model <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c1*SMDS_som_f5
  semotion_f6 ~ c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5
  sconduct_f6 ~ c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5
  shyper_f6 ~ c6*IGDS_som_f5
  sprosoc_f6 ~ c7*SMDS_som_f5
  sprosoc_f6 ~ c8*IGDS_som_f5
  SMDS_som_calc_f6 ~ c9*semotion_f5
  SMDS_som_calc_f6 ~ c10*sconduct_f5
  SMDS_som_calc_f6 ~ c11*shyper_f5
  SMDS_som_calc_f6 ~ c12*sprosoc_f5 
  IGDS_som_calc_f6 ~ c13*semotion_f5
  IGDS_som_calc_f6 ~ c14*sconduct_f5
  IGDS_som_calc_f6 ~ c15*shyper_f5
  IGDS_som_calc_f6 ~ c16*sprosoc_f5 

  # Residual correlations (if necessary)
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"
#### Fit the model
fit <- sem(clpm_model, data = CCA_data, estimator = "MLR")
#### Display results
summary(fit, standardized = TRUE)

### Explore the results
#...
#### Get fit indices
fit_indices <- fitMeasures(fit)
fit_indices
#### Modification indices
mod_indices <- modificationindices(fit)
mod_indices
#### AIC, BIC, and other fit statistics
fit_stats <- fitMeasures(fit, c("AIC", "BIC", "CFI", "RMSEA"))
fit_stats

### Graphs
#### Plot path diagrams
semPaths(fit, what = "std", layout = "tree")
semPaths(fit, what = "", layout = "tree")
semPaths(fit, whatLabels = "est.std", layout = "tree")
semPaths(fit, whatLabels = "est.std", layout = "tree", rotation = 2)


# note: PreReg: 1) CLPM 1 includes difficulties, 2) CLPM 2 adds strengths, 3) compare model fit
# note: PreReg: Explore the effect of sex with multi-group analysis

## Step 1: Run CLPM 1
### Specify the model for 1) CLPM 1 includes difficulties (emotion, conduct, hyper)
clpm_model_one <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  SMDS_som_calc_f6 ~ b4*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b5*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c1*SMDS_som_f5
  semotion_f6 ~ c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5
  sconduct_f6 ~ c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5
  shyper_f6 ~ c6*IGDS_som_f5
  SMDS_som_calc_f6 ~ c7*semotion_f5
  SMDS_som_calc_f6 ~ c8*sconduct_f5
  SMDS_som_calc_f6 ~ c9*shyper_f5
  IGDS_som_calc_f6 ~ c10*semotion_f5
  IGDS_som_calc_f6 ~ c11*sconduct_f5
  IGDS_som_calc_f6 ~ c12*shyper_f5

  # Residual correlations (if necessary)
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  sconduct_f5 ~~ shyper_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  sconduct_f6 ~~ shyper_f6
"
#### Fit the model
fit_one <- sem(clpm_model_one, data = CCA_data, estimator = "MLR")
#### Display results
summary(fit_one, standardized = TRUE)

### Explore the results
#...
#### Get fit indices
fit_indices_one <- fitMeasures(fit_one)
fit_indices_one
#### Modification indices
mod_indices_one <- modificationindices(fit_one)
mod_indices_one
#### AIC, BIC, and other fit statistics
fit_stats_one <- fitMeasures(fit_one, c("AIC", "BIC", "CFI", "RMSEA"))
fit_stats_one

### Graphs
#### Plot path diagram
semPaths(fit_one, what = "std", layout = "tree")
semPaths(fit_one, what = "std", layout = "tree")
semPaths(fit_one, whatLabels = "est.std", layout = "tree")
semPaths(fit_one, whatLabels = "est.std", layout = "tree", rotation = 2)

## Step 2: Run CLPM 2
### Specify the model for 2) CLPM 2 adds strengths (prosoc, *soc sup)
### *:soc sup added as strength, because we found no self-esteem in the data (the second planned moderator)
### [[the baseline model contains prosoc without socSup, so use that one if wanted to remove socSup]]
#### Specify the model
clpm_model_two <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 
  MSPSS_totaal_som_f6 ~ b7*MSPSS_totaal_som_f5

  # Cross-lagged paths
  semotion_f6 ~ c1*SMDS_som_f5
  semotion_f6 ~ c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5
  sconduct_f6 ~ c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5
  shyper_f6 ~ c6*IGDS_som_f5
  sprosoc_f6 ~ c7*SMDS_som_f5
  sprosoc_f6 ~ c8*IGDS_som_f5
  MSPSS_totaal_som_f6 ~ c9*SMDS_som_f5
  MSPSS_totaal_som_f6 ~ c10*IGDS_som_f5
  
  SMDS_som_calc_f6 ~ c11*semotion_f5
  SMDS_som_calc_f6 ~ c12*sconduct_f5
  SMDS_som_calc_f6 ~ c13*shyper_f5
  SMDS_som_calc_f6 ~ c14*sprosoc_f5 
  SMDS_som_calc_f6 ~ c15*MSPSS_totaal_som_f5
  IGDS_som_calc_f6 ~ c16*semotion_f5
  IGDS_som_calc_f6 ~ c17*sconduct_f5
  IGDS_som_calc_f6 ~ c18*shyper_f5
  IGDS_som_calc_f6 ~ c19*sprosoc_f5 
  IGDS_som_calc_f6 ~ c20*MSPSS_totaal_som_f5

  # Residual correlations (if necessary)
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  SMDS_som_f5 ~~ MSPSS_totaal_som_f5
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ MSPSS_totaal_som_f5
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ MSPSS_totaal_som_f5
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ MSPSS_totaal_som_f5
  shyper_f5 ~~ sprosoc_f5
  shyper_f5 ~~ MSPSS_totaal_som_f5
  sprosoc_f5 ~~ MSPSS_totaal_som_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  SMDS_som_calc_f6 ~~ MSPSS_totaal_som_f6
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ MSPSS_totaal_som_f6
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ MSPSS_totaal_som_f6
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ MSPSS_totaal_som_f6
  shyper_f6 ~~ sprosoc_f6
  shyper_f6 ~~ MSPSS_totaal_som_f6
  sprosoc_f6 ~~ MSPSS_totaal_som_f6
"
#### Fit the model
fit_two <- sem(clpm_model_two, data = CCA_data, estimator = "MLR")
#### Display results
summary(fit_two, standardized = TRUE)

### Explore the results
#...
#### Get fit indices
fit_indices_two <- fitMeasures(fit_two)
fit_indices_two
#### Modification indices
mod_indices_two <- modificationindices(fit_two)
mod_indices_two
#### AIC, BIC, and other fit statistics
fit_stats_two <- fitMeasures(fit_two, c("AIC", "BIC", "CFI", "RMSEA"))
fit_stats_two

### Graphs
#### Plot path diagram
semPaths(fit_two, what = "std", layout = "tree")
semPaths(fit_two, whatLabels = "est.std", layout = "tree")
semPaths(fit_two, whatLabels = "est.std", layout = "tree", rotation = 2)

# Model comparisons (from step 1 and step 2)
## Extract fit measures
fit_measures_base <- fitMeasures(fit) # fit measures of baseline CLPM (difficulties + prosoc)
fit_measures_one <- fitMeasures(fit_one) # fit measures of CLPM step 1 (difficulties)
fit_measures_two <- fitMeasures(fit_two) # fit measures of CLPM step 2 (difficulties + prosoc & socSupp)

### Create a data frame to compare fit indices
fit_comparison <- data.frame(
  Model = c("CLPM Base", "CLPM 1", "CLPM 2"),
  Chi_Square = c(fit_measures_base["chisq"], fit_measures_one["chisq"], fit_measures_two["chisq"]),
  DF = c(fit_measures_base["df"], fit_measures_one["df"], fit_measures_two["df"]),
  P_Value = c(fit_measures_base["pvalue"], fit_measures_one["pvalue"], fit_measures_two["pvalue"]),
  CFI = c(fit_measures_base["cfi"], fit_measures_one["cfi"], fit_measures_two["cfi"]),
  TLI = c(fit_measures_base["tli"], fit_measures_one["tli"], fit_measures_two["tli"]),
  RMSEA = c(fit_measures_base["rmsea"], fit_measures_one["rmsea"], fit_measures_two["rmsea"]),
  AIC = c(fit_measures_base["aic"], fit_measures_one["aic"], fit_measures_two["aic"]),
  BIC = c(fit_measures_base["bic"], fit_measures_one["bic"], fit_measures_two["bic"])
)
### View fit indices
print(fit_comparison)

## Statistical comparison of nested models (CLPM 1 and CLPM 2) using chi-square difference test
if (fit_measures_one["df"] < fit_measures_two["df"]) {
  chi_sq_diff <- fit_measures_two["chisq"] - fit_measures_one["chisq"]
  df_diff <- fit_measures_two["df"] - fit_measures_one["df"]
  p_value_diff <- pchisq(chi_sq_diff, df_diff, lower.tail = FALSE)
  
  cat("Chi-square Difference Test:\n")
  cat("Chi-square Difference:", chi_sq_diff, "\n")
  cat("Degrees of Freedom Difference:", df_diff, "\n")
  cat("P-value:", p_value_diff, "\n")
}
## significant result indicates that additional complexity of CLPM 2 (adding prosoc and socSupp)
## significantly improves the model fit

## Statistical comparison of nested models (CLPM Base and CLPM 1) using chi-square difference test
if (fit_measures_one["df"] < fit_measures_base["df"]) {
  chi_sq_diff <- fit_measures_base["chisq"] - fit_measures_one["chisq"]
  df_diff <- fit_measures_base["df"] - fit_measures_one["df"]
  p_value_diff <- pchisq(chi_sq_diff, df_diff, lower.tail = FALSE)
  
  cat("Chi-square Difference Test:\n")
  cat("Chi-square Difference:", chi_sq_diff, "\n")
  cat("Degrees of Freedom Difference:", df_diff, "\n")
  cat("P-value:", p_value_diff, "\n")
}
## non-significant result indicates that additional complexity of CLPM Base (adding prosoc) 
## does not significantly improve the model fit :: we proceed with CLPM Base in any case (for added informative value of prosoc)


# MULTIGROUP ANALYSIS (sex) ####

# Make gender into a factor variable (create new variable "gender" from "geslacht.y")
print(CCA_data$geslacht.y)
CCA_data$gender <- factor(CCA_data$geslacht.y, levels = c(1, 2), labels = c("male", "female"))
print(table(CCA_data$gender))  

# Specify the model (CLPM Base with sex added)
clpm_model_multigroup <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c(cm1, cf1)*SMDS_som_f5
  semotion_f6 ~ c(cm2, cf2)*IGDS_som_f5
  sconduct_f6 ~ c(cm3, cf3)*SMDS_som_f5
  sconduct_f6 ~ c(cm4, cf4)*IGDS_som_f5
  shyper_f6 ~ c(cm5, cf5)*SMDS_som_f5
  shyper_f6 ~ c(cm6, cf6)*IGDS_som_f5
  sprosoc_f6 ~ c(cm7, cf7)*SMDS_som_f5
  sprosoc_f6 ~ c(cm8, cf8)*IGDS_som_f5
  SMDS_som_calc_f6 ~ c(cm9, cf9)*semotion_f5
  SMDS_som_calc_f6 ~ c(cm10, cf10)*sconduct_f5
  SMDS_som_calc_f6 ~ c(cm11, cf11)*shyper_f5
  SMDS_som_calc_f6 ~ c(cm12, cf12)*sprosoc_f5 
  IGDS_som_calc_f6 ~ c(cm13, cf13)*semotion_f5
  IGDS_som_calc_f6 ~ c(cm14, cf14)*sconduct_f5
  IGDS_som_calc_f6 ~ c(cm15, cf15)*shyper_f5
  IGDS_som_calc_f6 ~ c(cm16, cf16)*sprosoc_f5 

  # Residual correlations
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"

# Fit the multigroup model
fit_multigroup <- sem(clpm_model_multigroup, data = CCA_data, group = "gender")

# Display results
summary(fit_multigroup, standardized = TRUE)

# Explore the results
fit_indices_multigroup <- fitMeasures(fit_multigroup)
fit_indices_multigroup

# Modification indices 
mod_indices_multigroup <- modificationindices(fit_multigroup)
mod_indices_multigroup

# Specific fit statistics AIC, BIC, CFI, RMSEA
fit_stats_multigroup <- fitMeasures(fit_multigroup, c("AIC", "BIC", "CFI", "RMSEA"))
fit_stats_multigroup

# Graphs
semPaths(fit_multigroup, what = "std", layout = "tree")
semPaths(fit_multigroup, what = "std", layout = "tree")
semPaths(fit_multigroup, whatLabels = "est.std", layout = "tree")
semPaths(fit_multigroup, whatLabels = "est.std", layout = "tree", rotation = 2)


# MULTIPLE IMPUTATION ANALYSIS (manual pooling) ####

# imputation with 5 datasets (each based on default 5 cycles)
imputed_data <- mice(analysis_data, method = 'rf', m = 50, maxit = 20, seed = 123)

# Placeholder for storing models fitted to each imputed dataset
fit_models <- vector("list", length = 50)

# Fit your CLPM to each imputed dataset
for (i in 1:50) {
  dataset_i <- complete(imputed_data, action = i)
  fit_models[[i]] <- sem(model = clpm_model, data = dataset_i)
}

# Examine each fitted model (skip with m = 50 imputation)
for (i in 1:length(fit_models)) {
  cat("Summary for Model", i, ":\n")
  print(summary(fit_models[[i]]))
  cat("\n\n")
}

## Direct inspection of parameter estimates for each model
print(parameterEstimates(fit_models[[1]]))
print(parameterEstimates(fit_models[[2]]))
print(parameterEstimates(fit_models[[3]]))
print(parameterEstimates(fit_models[[4]]))
print(parameterEstimates(fit_models[[x]]))


### Manual calculation (OLD; Skip until automated)
# Manual extraction of c1 estimates
estimates_c1 <- sapply(fit_models, function(model) {
  pe <- parameterEstimates(model)
  if ("c1" %in% pe$label) {
    return(pe[pe$label == "c1", "est"])
  } else {
    return(NA) # Return NA explicitly if c1 is not found
  }
})

# Manual extraction of c1 standard errors
ses_c1 <- sapply(fit_models, function(model) {
  pe <- parameterEstimates(model)
  if ("c1" %in% pe$label) {
    return(pe[pe$label == "c1", "se"])
  } else {
    return(NA) # Return NA explicitly if c1 is not found
  }
})

# Number of imputations
m <- length(fit_models)

# Pooled estimate (mean of the estimates)
pooled_estimate_c1 <- mean(estimates_c1, na.rm = TRUE)

# Within-imputation variance (mean of the squared SEs)
W_c1 <- mean(ses_c1^2, na.rm = TRUE)

# Between-imputation variance (variance of the estimates)
B_c1 <- var(estimates_c1, na.rm = TRUE)

# Total variance
T_c1 <- W_c1 + (1 + 1/m) * B_c1

# Pooled standard error (square root of total variance)
pooled_se_c1 <- sqrt(T_c1)

# Display the pooled estimate and standard error
cat("Pooled Estimate for c1:", pooled_estimate_c1, "\n")
cat("Pooled Standard Error for c1:", pooled_se_c1, "\n")

# Calculate 95% Confidence Interval for c1
CI_lower_c1 <- pooled_estimate_c1 - 1.96 * pooled_se_c1
CI_upper_c1 <- pooled_estimate_c1 + 1.96 * pooled_se_c1
cat("95% Confidence Interval for c1: [", CI_lower_c1, ",", CI_upper_c1, "]\n")

# Calculate Z-score for Hypothesis Testing (Null hypothesis: c1 = 0)
z_score <- pooled_estimate_c1 / pooled_se_c1
cat("Z-score for c1:", z_score, "\n")

# Calculate P-value for the test (two-tailed)
p_value <- 2 * (1 - pnorm(abs(z_score)))
cat("P-value for c1:", p_value, "\n")


### Automated calculation (RUN)
# Extract all parameter estimates and standardized solutions for each model
estimates_all <- lapply(fit_models, parameterEstimates)
standardized_all <- lapply(fit_models, standardizedSolution)

# Verify extraction for the first few imputed datasets (e.g., the first 3)
print(head(estimates_all[[1]], 10)) # Display first 10 parameter estimates of the first model
print(head(standardized_all[[1]], 10)) # Display first 10 standardized estimates of the first model

# Create a data frame to store pooled results for each parameter
pooled_results <- data.frame(
  Parameter = character(),
  Estimate_Unstd = numeric(),
  SE_Unstd = numeric(),
  CI_Lower_Unstd = numeric(),
  CI_Upper_Unstd = numeric(),
  Estimate_Std = numeric(),
  SE_Std = numeric(),
  CI_Lower_Std = numeric(),
  CI_Upper_Std = numeric(),
  Z_score = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# Number of imputations (m = 50)
m <- length(fit_models)
parameter_labels <- unique(unlist(lapply(estimates_all, function(pe) pe$label)))

# Loop over each parameter label
for (label in parameter_labels) {
  # Extract unstandardized estimates
  estimates_unstd <- sapply(estimates_all, function(pe) {
    if (label %in% pe$label) {
      return(pe[pe$label == label, "est"])
    } else {
      return(NA)
    }
  })
  
  ses_unstd <- sapply(estimates_all, function(pe) {
    if (label %in% pe$label) {
      return(pe[pe$label == label, "se"])
    } else {
      return(NA)
    }
  })
  
  # Extract standardized estimates using `standardizedSolution`
  estimates_std <- sapply(standardized_all, function(ss) {
    if (label %in% ss$label) {
      return(ss[ss$label == label, "est.std"])
    } else {
      return(NA)
    }
  })
  
  ses_std <- sapply(standardized_all, function(ss) {
    if (label %in% ss$label) {
      return(ss[ss$label == label, "se"])
    } else {
      return(NA)
    }
  })
  
  # Pool unstandardized estimates
  pooled_estimate_unstd <- mean(estimates_unstd, na.rm = TRUE)
  W_unstd <- mean(ses_unstd^2, na.rm = TRUE)
  B_unstd <- var(estimates_unstd, na.rm = TRUE)
  T_unstd <- W_unstd + (1 + 1/m) * B_unstd
  pooled_se_unstd <- sqrt(T_unstd)
  CI_lower_unstd <- pooled_estimate_unstd - 1.96 * pooled_se_unstd
  CI_upper_unstd <- pooled_estimate_unstd + 1.96 * pooled_se_unstd
  
  # Pool standardized estimates
  if (!all(is.na(estimates_std))) {
    pooled_estimate_std <- mean(estimates_std, na.rm = TRUE)
    W_std <- mean(ses_std^2, na.rm = TRUE)
    B_std <- var(estimates_std, na.rm = TRUE)
    T_std <- W_std + (1 + 1/m) * B_std
    pooled_se_std <- sqrt(T_std)
    CI_lower_std <- pooled_estimate_std - 1.96 * pooled_se_std
    CI_upper_std <- pooled_estimate_std + 1.96 * pooled_se_std
  } else {
    pooled_estimate_std <- NA
    pooled_se_std <- NA
    CI_lower_std <- NA
    CI_upper_std <- NA
  }
  
  # Calculate Z-score for unstandardized estimates
  z_score <- pooled_estimate_unstd / pooled_se_unstd
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # Add the results to the data frame
  pooled_results <- rbind(
    pooled_results,
    data.frame(
      Parameter = label,
      Estimate_Unstd = pooled_estimate_unstd,
      SE_Unstd = pooled_se_unstd,
      CI_Lower_Unstd = CI_lower_unstd,
      CI_Upper_Unstd = CI_upper_unstd,
      Estimate_Std = pooled_estimate_std,
      SE_Std = pooled_se_std,
      CI_Lower_Std = CI_lower_std,
      CI_Upper_Std = CI_upper_std,
      Z_score = z_score,
      P_value = p_value,
      stringsAsFactors = FALSE
    )
  )
}

# Display the pooled results for final verification
print(pooled_results)


## automated run (old version - non-standardised only)
# Extract all parameter estimates and standard errors for each imputed model
estimates_all <- lapply(fit_models, parameterEstimates)

# Get the list of unique parameter labels from all estimates
parameter_labels <- unique(unlist(lapply(estimates_all, function(pe) pe$label)))

# Create a data frame to store pooled results for each parameter
pooled_results <- data.frame(
  Parameter = character(),
  Estimate = numeric(),
  SE = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  Z_score = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# Number of imputations (m = 50)
m <- length(fit_models)

# Loop over each parameter and pool estimates across imputed datasets
for (label in parameter_labels) {
  # Extract estimates and SEs for the current parameter across all models
  estimates <- sapply(estimates_all, function(pe) {
    if (label %in% pe$label) {
      return(pe[pe$label == label, "est"])
    } else {
      return(NA) # Return NA if the parameter is not found
    }
  })
  
  ses <- sapply(estimates_all, function(pe) {
    if (label %in% pe$label) {
      return(pe[pe$label == label, "se"])
    } else {
      return(NA) # Return NA if the parameter is not found
    }
  })
  
  # Calculate pooled estimate, within-imputation variance, and between-imputation variance
  pooled_estimate <- mean(estimates, na.rm = TRUE)
  W <- mean(ses^2, na.rm = TRUE)
  B <- var(estimates, na.rm = TRUE)
  T <- W + (1 + 1/m) * B
  pooled_se <- sqrt(T)
  
  # Calculate 95% Confidence Interval
  CI_lower <- pooled_estimate - 1.96 * pooled_se
  CI_upper <- pooled_estimate + 1.96 * pooled_se
  
  # Calculate Z-score for hypothesis testing (Null hypothesis: parameter = 0)
  z_score <- pooled_estimate / pooled_se
  
  # Calculate two-tailed p-value
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # Add the results to the data frame
  pooled_results <- rbind(
    pooled_results,
    data.frame(
      Parameter = label,
      Estimate = pooled_estimate,
      SE = pooled_se,
      CI_Lower = CI_lower,
      CI_Upper = CI_upper,
      Z_score = z_score,
      P_value = p_value,
      stringsAsFactors = FALSE
    )
  )
}

# Display the results
print(pooled_results)


###### Correlation table ######
# Convert 'geslacht.y' to a binary numeric variable (0 for Jongen, 1 for Meisje)
CCA_data$geslacht_numeric <- ifelse(CCA_data$geslacht.y == 1, 0, 1)

# Select relevant columns, excluding original 'geslacht.y'
selected_vars <- CCA_data[, c("semotion_f5", "sconduct_f5", "shyper_f5",
                              "sprosoc_f5", "MSPSS_totaal_som_f5", "SMDS_som_f5", "IGDS_som_f5",  "semotion_f6",
                              "sconduct_f6", "shyper_f6", "sprosoc_f6", "MSPSS_totaal_som_f6",
                              "SMDS_som_calc_f6", "IGDS_som_calc_f6")]

# Compute the point-biserial correlations for 'geslacht_numeric' with each continuous variable
gender_correlations <- sapply(selected_vars, function(x) cor(x, CCA_data$geslacht_numeric, use = "complete.obs"))

# Compute the correlation matrix for continuous variables
correlation_matrix <- cor(selected_vars, use = "pairwise.complete.obs")

# Add gender correlations as a new row in the matrix
correlation_table <- rbind(correlation_matrix, Gender = gender_correlations)

# Display the final correlation table
print(correlation_table)

# Round values in the correlation table to 3 decimal places
correlation_table <- as.data.frame(round(correlation_table, 3))

# Save the correlation table to a CSV file with a semicolon separator for better Excel compatibility
write.csv2(correlation_table, file = "correlation_table.csv", row.names = TRUE)

# Confirmation message
print("The correlation table has been saved as 'correlation_table.csv' with values rounded to 3 decimals.")



#### additional paragraph on comparisons of analysis data and complete data
library(dplyr)
library(haven)

# Define the datasets:
# analysis_data is the full sample (n ≈ 2700) with missing values,
# filtered_dataset1 is the complete-case sample (n = 645).
full_data <- analysis_data
complete_sample <- filtered_dataset1

# ---- Compare Age ----
age_full <- full_data$F5leeftijd_kind
age_complete <- complete_sample$F5leeftijd_kind

cat("Age (Full Sample): Mean =", round(mean(age_full, na.rm = TRUE), 2),
    "SD =", round(sd(age_full, na.rm = TRUE), 2), "\n")
cat("Age (Filtered Sample): Mean =", round(mean(age_complete, na.rm = TRUE), 2),
    "SD =", round(sd(age_complete, na.rm = TRUE), 2), "\n")

age_ttest <- t.test(age_full, age_complete)
print(age_ttest)

# ---- Compare Gender ----
# Convert gender variable using original SPSS labels
gender_full <- as_factor(full_data$geslacht.y, levels = "labels")
gender_complete <- as_factor(complete_sample$geslacht.y, levels = "labels")

cat("\nGender Distribution (Full Sample):\n")
print(table(gender_full))
cat("\nGender Distribution (Filtered Sample):\n")
print(table(gender_complete))

gender_table <- rbind(Full = table(gender_full),
                      Filtered = table(gender_complete))
print(gender_table)
gender_chisq <- chisq.test(gender_table)
print(gender_chisq)

# ---- Descriptive Statistics for Analysis Variables ----
analysis_vars <- names(complete_sample)
full_subset <- full_data[, analysis_vars]

cat("\n--- Descriptive Summary: Filtered Sample (Complete Cases) ---\n")
complete_summary <- complete_sample %>% 
  summarise_all(list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))
print(round(complete_summary, 2))

cat("\n--- Descriptive Summary: Full Sample (subset on analysis vars) ---\n")
full_subset_summary <- full_subset %>% 
  summarise_all(list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))
print(round(full_subset_summary, 2))

# ---- T-tests for Each Analysis Variable ----
cat("\n--- T-tests for Each Analysis Variable (Full vs. Filtered) ---\n")
for (var in analysis_vars) {
  cat("\nT-test for", var, ":\n")
  t_result <- t.test(full_subset[[var]], complete_sample[[var]])
  print(t_result)
}

# MULTIGROUP ANALYSIS (Social Support after review) ######
# Check basic distribution of MSPSS at T1
summary(CCA_data$MSPSS_totaal_som_f5)
sd(CCA_data$MSPSS_totaal_som_f5, na.rm = TRUE)
hist(CCA_data$MSPSS_totaal_som_f5, breaks = 20, col = "lightblue", main = "Histogram of MSPSS (T1)", xlab = "MSPSS_totaal_som_f5")

# Density plot
ggplot(CCA_data, aes(x = MSPSS_totaal_som_f5)) +
  geom_density(fill = "lightblue") +
  labs(title = "Density Plot of MSPSS at T1", x = "MSPSS_totaal_som_f5", y = "Density")



# Filter non-missing MSPSS values
mspss_non_missing <- CCA_data[!is.na(CCA_data$MSPSS_totaal_som_f5), ]

# Median split
median_val <- median(mspss_non_missing$MSPSS_totaal_som_f5)
mspss_non_missing$MSPSS_median_group <- ifelse(mspss_non_missing$MSPSS_totaal_som_f5 < median_val, "low", "high")

# Tertile split (bottom 33% vs. rest)
tertiles <- quantile(mspss_non_missing$MSPSS_totaal_som_f5, probs = c(0.33, 0.66))
low_cut <- tertiles[1]
mspss_non_missing$MSPSS_tertile_group <- ifelse(mspss_non_missing$MSPSS_totaal_som_f5 <= low_cut, "low", "high")

# -1 SD split
mean_val <- mean(mspss_non_missing$MSPSS_totaal_som_f5)
sd_val <- sd(mspss_non_missing$MSPSS_totaal_som_f5)
sd_cut <- mean_val - sd_val
mspss_non_missing$MSPSS_sd_group <- ifelse(mspss_non_missing$MSPSS_totaal_som_f5 < sd_cut, "low", "high")

# Summary of group sizes
summary_table <- data.frame(
  Split = c("Median", "Tertile", "-1 SD"),
  Low_n = c(
    sum(mspss_non_missing$MSPSS_median_group == "low"),
    sum(mspss_non_missing$MSPSS_tertile_group == "low"),
    sum(mspss_non_missing$MSPSS_sd_group == "low")
  ),
  High_n = c(
    sum(mspss_non_missing$MSPSS_median_group == "high"),
    sum(mspss_non_missing$MSPSS_tertile_group == "high"),
    sum(mspss_non_missing$MSPSS_sd_group == "high")
  )
)

# Display summary
print(summary_table)



# Add the tertile grouping back to CCA_data by matching IDs
CCA_data$MSPSS_tertile_group <- NA
CCA_data$MSPSS_tertile_group[!is.na(CCA_data$MSPSS_totaal_som_f5)] <- mspss_non_missing$MSPSS_tertile_group

# Load dplyr for summarizing
library(dplyr)

# Descriptive stats by group
descriptives_by_group <- CCA_data %>%
  filter(!is.na(MSPSS_tertile_group)) %>%
  group_by(MSPSS_tertile_group) %>%
  summarise(
    n = n(),
    MSPSS_mean = mean(MSPSS_totaal_som_f5, na.rm = TRUE),
    Emotional_T1 = mean(semotion_f5, na.rm = TRUE),
    Emotional_T2 = mean(semotion_f6, na.rm = TRUE),
    SMDS_T1 = mean(SMDS_som_f5, na.rm = TRUE),
    SMDS_T2 = mean(SMDS_som_calc_f6, na.rm = TRUE),
    IGDS_T1 = mean(IGDS_som_f5, na.rm = TRUE),
    IGDS_T2 = mean(IGDS_som_calc_f6, na.rm = TRUE)
  )

print(descriptives_by_group)


# Create new tertile grouping based on MSPSS T1 (bottom 33% = "low", rest = "high")

# Step 1: Calculate tertile cutoff
tertiles <- quantile(CCA_data$MSPSS_totaal_som_f5, probs = c(0.33), na.rm = TRUE)
low_cut <- tertiles[1]

# Step 2: Create new variable for grouping
CCA_data$MSPSS_tertile_f5_group <- NA
CCA_data$MSPSS_tertile_f5_group[!is.na(CCA_data$MSPSS_totaal_som_f5)] <-
  ifelse(CCA_data$MSPSS_totaal_som_f5[!is.na(CCA_data$MSPSS_totaal_som_f5)] <= low_cut, "low", "high")

# Step 3: Check distribution
table(CCA_data$MSPSS_tertile_f5_group, useNA = "ifany")

# Show a sample of the new grouping variable alongside the original MSPSS scores
head(CCA_data[, c("MSPSS_totaal_som_f5", "MSPSS_tertile_f5_group")], 20)


#### MULTIGROUP SocSup ANALYSIS
# Make MSPSS tertile group a factor
CCA_data$MSPSS_group <- factor(CCA_data$MSPSS_tertile_f5_group, levels = c("low", "high"))
print(table(CCA_data$MSPSS_group))

# Specify the multigroup CLPM model for MSPSS
clpm_model_mspss <- "
  # Autoregressive paths
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths
  semotion_f6 ~ c(cl1, ch1)*SMDS_som_f5
  semotion_f6 ~ c(cl2, ch2)*IGDS_som_f5
  sconduct_f6 ~ c(cl3, ch3)*SMDS_som_f5
  sconduct_f6 ~ c(cl4, ch4)*IGDS_som_f5
  shyper_f6 ~ c(cl5, ch5)*SMDS_som_f5
  shyper_f6 ~ c(cl6, ch6)*IGDS_som_f5
  sprosoc_f6 ~ c(cl7, ch7)*SMDS_som_f5
  sprosoc_f6 ~ c(cl8, ch8)*IGDS_som_f5
  SMDS_som_calc_f6 ~ c(cl9, ch9)*semotion_f5
  SMDS_som_calc_f6 ~ c(cl10, ch10)*sconduct_f5
  SMDS_som_calc_f6 ~ c(cl11, ch11)*shyper_f5
  SMDS_som_calc_f6 ~ c(cl12, ch12)*sprosoc_f5 
  IGDS_som_calc_f6 ~ c(cl13, ch13)*semotion_f5
  IGDS_som_calc_f6 ~ c(cl14, ch14)*sconduct_f5
  IGDS_som_calc_f6 ~ c(cl15, ch15)*shyper_f5
  IGDS_som_calc_f6 ~ c(cl16, ch16)*sprosoc_f5 

  # Residual correlations
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"

# Fit the model by MSPSS group
fit_mspss_multigroup <- sem(clpm_model_mspss, data = CCA_data, group = "MSPSS_group")

# Summary output
summary(fit_mspss_multigroup, standardized = TRUE)

# Fit indices
fit_indices_mspss <- fitMeasures(fit_mspss_multigroup)
fit_indices_mspss

# Modification indices
mod_indices_mspss <- modificationindices(fit_mspss_multigroup)
mod_indices_mspss

# Main fit statistics
fit_stats_mspss <- fitMeasures(fit_mspss_multigroup, c("AIC", "BIC", "CFI", "RMSEA"))
fit_stats_mspss

# Visual path diagrams
semPaths(fit_mspss_multigroup, what = "std", layout = "tree")
semPaths(fit_mspss_multigroup, whatLabels = "est.std", layout = "tree")
semPaths(fit_mspss_multigroup, whatLabels = "est.std", layout = "tree", rotation = 2)


### chi-square test
clpm_model_mspss_constrained <- "
  # Autoregressive paths (constrained)
  semotion_f6 ~ b1*semotion_f5 
  sconduct_f6 ~ b2*sconduct_f5 
  shyper_f6 ~ b3*shyper_f5 
  sprosoc_f6 ~ b4*sprosoc_f5 
  SMDS_som_calc_f6 ~ b5*SMDS_som_f5 
  IGDS_som_calc_f6 ~ b6*IGDS_som_f5 

  # Cross-lagged paths (constrained across groups)
  semotion_f6 ~ c1*SMDS_som_f5 + c2*IGDS_som_f5
  sconduct_f6 ~ c3*SMDS_som_f5 + c4*IGDS_som_f5
  shyper_f6 ~ c5*SMDS_som_f5 + c6*IGDS_som_f5
  sprosoc_f6 ~ c7*SMDS_som_f5 + c8*IGDS_som_f5
  SMDS_som_calc_f6 ~ c9*semotion_f5 + c10*sconduct_f5 + c11*shyper_f5 + c12*sprosoc_f5 
  IGDS_som_calc_f6 ~ c13*semotion_f5 + c14*sconduct_f5 + c15*shyper_f5 + c16*sprosoc_f5 

  # Residual correlations (same as before)
  SMDS_som_f5 ~~ IGDS_som_f5
  SMDS_som_f5 ~~ semotion_f5
  SMDS_som_f5 ~~ sconduct_f5
  SMDS_som_f5 ~~ shyper_f5
  SMDS_som_f5 ~~ sprosoc_f5 
  IGDS_som_f5 ~~ semotion_f5
  IGDS_som_f5 ~~ sconduct_f5
  IGDS_som_f5 ~~ shyper_f5
  IGDS_som_f5 ~~ sprosoc_f5 
  semotion_f5 ~~ sconduct_f5
  semotion_f5 ~~ shyper_f5
  semotion_f5 ~~ sprosoc_f5 
  sconduct_f5 ~~ shyper_f5
  sconduct_f5 ~~ sprosoc_f5 
  shyper_f5 ~~ sprosoc_f5
  SMDS_som_calc_f6 ~~ IGDS_som_calc_f6
  SMDS_som_calc_f6 ~~ semotion_f6
  SMDS_som_calc_f6 ~~ sconduct_f6
  SMDS_som_calc_f6 ~~ shyper_f6
  SMDS_som_calc_f6 ~~ sprosoc_f6 
  IGDS_som_calc_f6 ~~ semotion_f6
  IGDS_som_calc_f6 ~~ sconduct_f6
  IGDS_som_calc_f6 ~~ shyper_f6
  IGDS_som_calc_f6 ~~ sprosoc_f6 
  semotion_f6 ~~ sconduct_f6
  semotion_f6 ~~ shyper_f6
  semotion_f6 ~~ sprosoc_f6 
  sconduct_f6 ~~ shyper_f6
  sconduct_f6 ~~ sprosoc_f6 
  shyper_f6 ~~ sprosoc_f6
"

fit_mspss_constrained <- sem(clpm_model_mspss_constrained, data = CCA_data, group = "MSPSS_group")

anova(fit_mspss_constrained, fit_mspss_multigroup)


library(dplyr)

# Descriptives for low and high MSPSS tertile groups
CCA_data %>%
  filter(MSPSS_tertile_f5_group %in% c("low", "high")) %>%
  group_by(MSPSS_tertile_f5_group) %>%
  summarise(
    n = n(),
    mean_MSPSS = mean(MSPSS_totaal_som_f5, na.rm = TRUE),
    sd_MSPSS = sd(MSPSS_totaal_som_f5, na.rm = TRUE),
    min_MSPSS = min(MSPSS_totaal_som_f5, na.rm = TRUE),
    max_MSPSS = max(MSPSS_totaal_som_f5, na.rm = TRUE)
  )
