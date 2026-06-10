# ################################################################# #
#### 5.4 ECONOMETRIC RESULTS: BASELINE MNL AND INTERACTION MODEL ####
# ################################################################# #

rm(list = ls())

setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

library(apollo)
library(readr)
library(dplyr)

apollo_initialise()

# ################################################################# #
#### LOAD DATA                                                     ####
# ################################################################# #

database <- read_csv("apollo_database.csv", show_col_types = FALSE)

# Apollo needs a data.frame
database <- as.data.frame(database)

# Check
table(database$choice)
table(database$sample)
summary(database[, c("red1", "red2", "red3", "cost1", "cost2", "cost3")])

# Make sure choice is integer 1/2/3
database$choice <- as.integer(database$choice)

# ################################################################# #
#### MODEL 1: BASELINE MULTINOMIAL LOGIT                          ####
# ################################################################# #

apollo_control = list(
  modelName  = "MIR_MNL_baseline",
  modelDescr = "Baseline MNL model for MIR DCE",
  indivID    = "ID",
  nCores     = 1
)

apollo_beta = c(
  b_red  = 0.01,
  b_cost = -0.05
)

apollo_fixed = c()

apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  V[["alt1"]] = b_red * red1 + b_cost * cost1
  V[["alt2"]] = b_red * red2 + b_cost * cost2
  V[["alt3"]] = b_red * red3 + b_cost * cost3
  
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    V            = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # Panel product because each respondent answers 12 tasks
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_baseline <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

apollo_modelOutput(model_baseline)
apollo_saveOutput(model_baseline)

# Save estimates
baseline_estimates <- as.data.frame(model_baseline$estimate)
write_csv(
  data.frame(parameter = names(model_baseline$estimate),
             estimate = as.numeric(model_baseline$estimate)),
  "MIR_MNL_baseline_estimates.csv"
)

# WTP for pesticide reduction
b1 <- model_baseline$estimate

WTP_baseline <- - b1["b_red"] / b1["b_cost"]

cat("\nBaseline WTP per percentage point of pesticide reduction:", WTP_baseline, "\n")
cat("Baseline WTP per 25 percentage points:", WTP_baseline * 25, "\n")

# ################################################################# #
#### MODEL 2: MNL WITH TREATMENT INTERACTIONS                     ####
# ################################################################# #

rm(apollo_probabilities, apollo_beta, apollo_fixed, apollo_control, apollo_inputs)

apollo_control = list(
  modelName  = "MIR_MNL_treatment_interactions",
  modelDescr = "MNL model with treatment interactions for MIR DCE",
  indivID    = "ID",
  nCores     = 1
)

apollo_beta = c(
  b_red     = 0.01,
  b_cost    = -0.05,
  
  b_red_T2  = 0.001,
  b_cost_T2 = 0.001,
  
  b_red_T3  = 0.001,
  b_cost_T3 = 0.001
)

apollo_fixed = c()

apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  # Treatment-specific marginal utilities
  b_red_i  = b_red  + b_red_T2  * T2 + b_red_T3  * T3
  b_cost_i = b_cost + b_cost_T2 * T2 + b_cost_T3 * T3
  
  V[["alt1"]] = b_red_i * red1 + b_cost_i * cost1
  V[["alt2"]] = b_red_i * red2 + b_cost_i * cost2
  V[["alt3"]] = b_red_i * red3 + b_cost_i * cost3
  
  mnl_settings = list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    V            = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_interactions <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

apollo_modelOutput(model_interactions)
apollo_saveOutput(model_interactions)

# Save estimates
write_csv(
  data.frame(parameter = names(model_interactions$estimate),
             estimate = as.numeric(model_interactions$estimate)),
  "MIR_MNL_treatment_interaction_estimates.csv"
)

# ################################################################# #
#### WTP BY TREATMENT GROUP                                       ####
# ################################################################# #

b2 <- model_interactions$estimate

WTP_S1 <- - b2["b_red"] / b2["b_cost"]

WTP_S2 <- - (b2["b_red"] + b2["b_red_T2"]) /
  (b2["b_cost"] + b2["b_cost_T2"])

WTP_S3 <- - (b2["b_red"] + b2["b_red_T3"]) /
  (b2["b_cost"] + b2["b_cost_T3"])

wtp_table <- data.frame(
  sample = c("S1", "S2", "S3"),
  WTP_per_percentage_point = c(WTP_S1, WTP_S2, WTP_S3),
  WTP_per_25_percentage_points = c(WTP_S1, WTP_S2, WTP_S3) * 25
)

print(wtp_table)

write_csv(wtp_table, "MIR_WTP_by_treatment.csv")

cat("\nEconometric estimation completed.\n")