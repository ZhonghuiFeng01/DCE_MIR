# ################################################################# #
#### MIXED MULTINOMIAL LOGIT MODEL WITH ASC-TREATMENT INTERACTIONS ####
# ################################################################# #

rm(list = ls())

setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

library(apollo)
library(readr)
library(dplyr)
library(tibble)

# ================================================================ #
# 1. Initialise Apollo
# ================================================================ #

apollo_initialise()

apollo_control = list(
  modelName       = "MMNL_ASC_treatment_interactions",
  modelDescr      = "Mixed multinomial logit with ASC-treatment interactions",
  indivID         = "ID",
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output_mmnl_asc_treatment"
)

# ================================================================ #
# 2. Load database
# ================================================================ #

database <- read_csv("apollo_database.csv", show_col_types = FALSE)

database <- database %>%
  mutate(
    ID     = as.numeric(ID),
    choice = as.numeric(choice),
    T2     = as.numeric(T2),
    T3     = as.numeric(T3),
    
    red1   = as.numeric(red1),
    red2   = as.numeric(red2),
    red3   = as.numeric(red3),
    
    cost1  = as.numeric(cost1),
    cost2  = as.numeric(cost2),
    cost3  = as.numeric(cost3),
    
    # Alternative-type indicators
    sq1  = as.numeric(red1 == 0 & cost1 == 0),
    sq2  = as.numeric(red2 == 0 & cost2 == 0),
    sq3  = as.numeric(red3 == 0 & cost3 == 0),
    
    ban1 = as.numeric(red1 == 100 & cost1 == 6),
    ban2 = as.numeric(red2 == 100 & cost2 == 6),
    ban3 = as.numeric(red3 == 100 & cost3 == 6)
  )

# Optional checks
cat("\nTreatment distribution:\n")
print(table(database$T2, database$T3))

cat("\nChoice distribution:\n")
print(table(database$choice))

cat("\nNumber of individuals:", length(unique(database$ID)), "\n")
cat("Number of choice observations:", nrow(database), "\n")

# ================================================================ #
# 3. Define parameters
# ================================================================ #

# Starting values:
# asc_sq, asc_ban, beta_red, beta_cost, sigma_red, sigma_cost
# can be based on your previous MMNL results.
# ASC-treatment interaction starting values are set to 0.

apollo_beta = c(
  # Base ASC in S1
  asc_sq       = -1.158,
  asc_ban      = -0.274,
  
  # ASC-treatment interactions
  asc_sq_s2    =  0.000,
  asc_sq_s3    =  0.000,
  asc_ban_s2   =  0.000,
  asc_ban_s3   =  0.000,
  
  # Mean coefficients of random parameters
  beta_red     =  0.046,
  beta_cost    = -0.444,
  
  # Standard deviations of random parameters
  sigma_red    =  0.029,
  sigma_cost   =  0.178
)

apollo_fixed = c()

# ================================================================ #
# 4. Define simulation draws
# ================================================================ #

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interNormDraws = c("draw_red", "draw_cost")
)

# ================================================================ #
# 5. Define random coefficients
# ================================================================ #

apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  
  randcoeff = list()
  
  randcoeff[["beta_red_i"]] =
    beta_red + sigma_red * draw_red
  
  randcoeff[["beta_cost_i"]] =
    beta_cost + sigma_cost * draw_cost
  
  return(randcoeff)
}

# ================================================================ #
# 6. Validate inputs
# ================================================================ #

apollo_inputs = apollo_validateInputs()

# ================================================================ #
# 7. Define probabilities
# ================================================================ #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  # -------------------------------------------------------------- #
  # Utility functions
  # Tax alternatives are the reference category for ASC.
  #
  # Base ASC:
  #   asc_sq  = residual utility of SQ in S1
  #   asc_ban = residual utility of full ban in S1
  #
  # ASC-treatment interactions:
  #   asc_sq_s2  = additional SQ residual utility in S2
  #   asc_sq_s3  = additional SQ residual utility in S3
  #   asc_ban_s2 = additional full-ban residual utility in S2
  #   asc_ban_s3 = additional full-ban residual utility in S3
  # -------------------------------------------------------------- #
  
  V[["alt1"]] =
    asc_sq  * sq1 +
    asc_ban * ban1 +
    asc_sq_s2  * sq1  * T2 +
    asc_sq_s3  * sq1  * T3 +
    asc_ban_s2 * ban1 * T2 +
    asc_ban_s3 * ban1 * T3 +
    beta_red_i  * red1 +
    beta_cost_i * cost1
  
  V[["alt2"]] =
    asc_sq  * sq2 +
    asc_ban * ban2 +
    asc_sq_s2  * sq2  * T2 +
    asc_sq_s3  * sq2  * T3 +
    asc_ban_s2 * ban2 * T2 +
    asc_ban_s3 * ban2 * T3 +
    beta_red_i  * red2 +
    beta_cost_i * cost2
  
  V[["alt3"]] =
    asc_sq  * sq3 +
    asc_ban * ban3 +
    asc_sq_s2  * sq3  * T2 +
    asc_sq_s3  * sq3  * T3 +
    asc_ban_s2 * ban3 * T2 +
    asc_ban_s3 * ban3 * T3 +
    beta_red_i  * red3 +
    beta_cost_i * cost3
  
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    V            = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # Panel structure: repeated choices by respondent
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  # Average over inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

# ================================================================ #
# 8. Estimate model
# ================================================================ #

model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# ================================================================ #
# 9. Output results
# ================================================================ #

apollo_modelOutput(model)
apollo_saveOutput(model)

# ================================================================ #
# 10. Save coefficient table
# ================================================================ #

coef_table <- tibble(
  parameter = names(model$estimate),
  estimate  = as.numeric(model$estimate),
  se        = as.numeric(model$se),
  t_ratio   = estimate / se
)

# Add robust standard errors if available
if (!is.null(model$robse)) {
  coef_table <- coef_table %>%
    mutate(
      robust_se      = as.numeric(model$robse),
      robust_t_ratio = estimate / robust_se
    )
}

# Add p-values and stars based on robust SE if available, otherwise normal SE
coef_table <- coef_table %>%
  mutate(
    se_used = ifelse(!is.null(model$robse), robust_se, se),
    z_value = estimate / se_used,
    p_value = 2 * (1 - pnorm(abs(z_value))),
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    estimate_formatted = paste0(sprintf("%.4f", estimate), stars),
    se_formatted = paste0("(", sprintf("%.4f", se_used), ")")
  )

print(coef_table)

write_csv(coef_table, "mmnl_asc_treatment_interactions_coefficients.csv")

cat("\nSaved: mmnl_asc_treatment_interactions_coefficients.csv\n")

# ================================================================ #
# 11. Create table-ready output
# ================================================================ #

table_ready <- coef_table %>%
  mutate(
    label = case_when(
      parameter == "asc_sq"     ~ "ASC status quo (base)",
      parameter == "asc_sq_s2"  ~ "ASC status quo $\\times$ S2",
      parameter == "asc_sq_s3"  ~ "ASC status quo $\\times$ S3",
      parameter == "asc_ban"    ~ "ASC full ban (base)",
      parameter == "asc_ban_s2" ~ "ASC full ban $\\times$ S2",
      parameter == "asc_ban_s3" ~ "ASC full ban $\\times$ S3",
      parameter == "beta_red"   ~ "Pesticide reduction",
      parameter == "beta_cost"  ~ "Weekly food-cost increase",
      parameter == "sigma_red"  ~ "Std. dev. pesticide reduction",
      parameter == "sigma_cost" ~ "Std. dev. weekly food-cost increase",
      TRUE ~ parameter
    ),
    type = case_when(
      parameter %in% c("sigma_red", "sigma_cost") ~ "std_coefficient",
      TRUE ~ "mean_coefficient"
    )
  ) %>%
  select(label, parameter, type, estimate_formatted, se_formatted, p_value)

print(table_ready)

write_csv(table_ready, "mmnl_asc_treatment_interactions_table_ready.csv")

cat("Saved: mmnl_asc_treatment_interactions_table_ready.csv\n")

# ================================================================ #
# 12. Optional: WTP based on mean coefficients
# ================================================================ #

# In this ASC-treatment model, S2 and S3 are interacted with ASC only.
# Therefore, the marginal utility of reduction and cost is common across samples.
# The WTP is the same across treatment groups in this specific model.

coef_est <- model$estimate

beta_red_hat  <- coef_est["beta_red"]
beta_cost_hat <- coef_est["beta_cost"]

wtp <- - beta_red_hat / beta_cost_hat

wtp_table <- tibble(
  model = "MMNL with ASC-treatment interactions",
  wtp_per_percentage_point = as.numeric(wtp),
  wtp_per_25_percentage_points = as.numeric(wtp * 25)
)

print(wtp_table)

write_csv(wtp_table, "wtp_mmnl_asc_treatment_interactions.csv")

cat("Saved: wtp_mmnl_asc_treatment_interactions.csv\n")

# ================================================================ #
# 13. Model summary
# ================================================================ #

cat("\nModel summary:\n")
cat("Number of individuals:", length(unique(database$ID)), "\n")
cat("Choice observations:", nrow(database), "\n")
cat("Final log-likelihood:", model$maximum, "\n")