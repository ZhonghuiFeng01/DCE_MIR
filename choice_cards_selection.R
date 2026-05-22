
# ============================================================
# spdesign: D-efficient selection of 12 choice cards
# Stable version: SQ has red = 0 and cost = 0
#
# Logic:
# 1. Build 45 unique economic choice sets
# 2. Use spdesign to select 12 D-efficient cards
# 3. Randomize alt2 / alt3 presentation order after selection
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

# install.packages("spdesign")
# install.packages("dplyr")
# install.packages("writexl")

library(spdesign)
library(dplyr)
library(writexl)


# ------------------------------------------------------------
# 1. Define admissible policy profiles
# ------------------------------------------------------------
# Intermediate tax profiles:
# red = 25, 50, 75
# cost = 1, 2, 3, 4, 5
#
# BAN profile:
# red = 100
# cost = 6

tax_profiles <- expand.grid(
  red = c(25, 50, 75),
  cost = 1:5
) %>%
  as_tibble() %>%
  mutate(
    label = paste0("Taxe ", red, " ", cost)
  )

ban_profile <- tibble(
  red = 100,
  cost = 6,
  label = "BAN"
)

profiles <- bind_rows(tax_profiles, ban_profile) %>%
  mutate(profile_id = row_number()) %>%
  select(profile_id, label, red, cost)

cat("Number of admissible profiles:", nrow(profiles), "\n")
print(profiles)


# ------------------------------------------------------------
# 2. Build 45 unique economic choice sets
# ------------------------------------------------------------
# Here, order is normalized only to avoid mirror duplicates.
# This is NOT the final presentation order.
#
# We keep:
# lower red / lower cost in alt2
# higher red / higher cost in alt3

candidate_45 <- expand.grid(
  alt2_id = profiles$profile_id,
  alt3_id = profiles$profile_id
) %>%
  as_tibble() %>%
  filter(alt2_id != alt3_id) %>%
  left_join(
    profiles %>%
      select(
        alt2_id = profile_id,
        alt2_label = label,
        alt2_red = red,
        alt2_cost = cost
      ),
    by = "alt2_id"
  ) %>%
  left_join(
    profiles %>%
      select(
        alt3_id = profile_id,
        alt3_label = label,
        alt3_red = red,
        alt3_cost = cost
      ),
    by = "alt3_id"
  ) %>%
  filter(
    alt2_red < alt3_red,
    alt2_cost < alt3_cost
  ) %>%
  mutate(
    card_type = if_else(
      alt3_red == 100,
      "SQ + Taxe + BAN",
      "SQ + Taxe + Taxe"
    ),
    pair_id = paste(alt2_red, alt2_cost, alt3_red, alt3_cost, sep = "_"),
    card_id = row_number()
  ) %>%
  select(
    card_id, pair_id, card_type,
    alt2_label, alt2_red, alt2_cost,
    alt3_label, alt3_red, alt3_cost
  )

cat("Number of unique candidate choice sets:", nrow(candidate_45), "\n")
print(candidate_45 %>% count(card_type))


# ------------------------------------------------------------
# 3. Candidate set for spdesign
# ------------------------------------------------------------
# Wide format required by spdesign:
# <alternative>_<attribute>
#
# SQ is fixed at red = 0 and cost = 0.
# Therefore U(SQ) = b_red * 0 + b_cost * 0 = 0.

candidate_for_spdesign <- candidate_45 %>%
  transmute(
    SQ_red = 0,
    SQ_cost = 0,
    alt2_red = alt2_red,
    alt2_cost = alt2_cost,
    alt3_red = alt3_red,
    alt3_cost = alt3_cost
  ) %>%
  as.data.frame()

cat("Candidate set columns:\n")
print(names(candidate_for_spdesign))
head(candidate_for_spdesign)


# ------------------------------------------------------------
# 4. Utility functions
# ------------------------------------------------------------
# IMPORTANT:
# - Parameters must start with b_
# - Attribute levels are specified only once
# - SQ uses the same generic coefficients, but its attributes are fixed at 0
#
# Thus:
# U(SQ)   = b_red * 0 + b_cost * 0 = 0
# U(alt2) = b_red * red + b_cost * cost
# U(alt3) = b_red * red + b_cost * cost

utility <- list(
  SQ   = "b_red[0.01] * red[c(0,25,50,75,100)] + b_cost[-0.05] * cost[0:6]",
  alt2 = "b_red * red + b_cost * cost",
  alt3 = "b_red * red + b_cost * cost"
)


# ------------------------------------------------------------
# 5. Generate D-efficient design
# ------------------------------------------------------------

set.seed(123)

design_sp <- generate_design(
  utility = utility,
  rows = 12,
  model = "mnl",
  efficiency_criteria = "d-error",
  algorithm = "federov",
  draws = "pseudo-random",
  candidate_set = candidate_for_spdesign,
  control = list(
    cores = 1,
    max_iter = 20000,
    max_relabel = 10000,
    max_no_improve = 100000,
    efficiency_threshold = 0.1,
    sample_with_replacement = FALSE
  )
)

print(design_sp)
summary(design_sp)


# ------------------------------------------------------------
# 6. Extract selected design
# ------------------------------------------------------------

cat("Names in design_sp:\n")
print(names(design_sp))

selected_raw <- as.data.frame(design_sp$design)

View(selected_raw)


# ------------------------------------------------------------
# 7. Match selected rows back to candidate_45
# ------------------------------------------------------------

selected_12_normalized <- selected_raw %>%
  left_join(
    candidate_45,
    by = c(
      "alt2_red" = "alt2_red",
      "alt2_cost" = "alt2_cost",
      "alt3_red" = "alt3_red",
      "alt3_cost" = "alt3_cost"
    )
  ) %>%
  arrange(card_id) %>%
  select(
    card_id, pair_id, card_type,
    alt2_label, alt2_red, alt2_cost,
    alt3_label, alt3_red, alt3_cost
  )

View(selected_12_normalized)

cat("Number of selected cards:", nrow(selected_12_normalized), "\n")
print(selected_12_normalized %>% count(card_type))


# ------------------------------------------------------------
# 8. Randomize presentation order of alt2 and alt3
# ------------------------------------------------------------
# This avoids imposing a fixed display order in the questionnaire.
# SQ remains fixed, but alt2 and alt3 are randomized.

set.seed(2026)

selected_12_randomized <- selected_12_normalized %>%
  rowwise() %>%
  mutate(
    swap_order = sample(c(TRUE, FALSE), size = 1),
    
    final_alt2_label = ifelse(swap_order, alt3_label, alt2_label),
    final_alt2_red = ifelse(swap_order, alt3_red, alt2_red),
    final_alt2_cost = ifelse(swap_order, alt3_cost, alt2_cost),
    
    final_alt3_label = ifelse(swap_order, alt2_label, alt3_label),
    final_alt3_red = ifelse(swap_order, alt2_red, alt3_red),
    final_alt3_cost = ifelse(swap_order, alt2_cost, alt3_cost)
  ) %>%
  ungroup() %>%
  mutate(
    SQ_label = "SQ",
    SQ_red = 0,
    SQ_cost = 0
  ) %>%
  select(
    card_id, pair_id, card_type,
    SQ_label, SQ_red, SQ_cost,
    alt2_label = final_alt2_label,
    alt2_red = final_alt2_red,
    alt2_cost = final_alt2_cost,
    alt3_label = final_alt3_label,
    alt3_red = final_alt3_red,
    alt3_cost = final_alt3_cost,
    swap_order
  )

View(selected_12_randomized)


# ------------------------------------------------------------
# 9. Add utilities and MNL probabilities
# ------------------------------------------------------------

b_red <- 0.01
b_cost <- -0.05

selected_12_randomized <- selected_12_randomized %>%
  mutate(
    U_SQ = b_red * SQ_red + b_cost * SQ_cost,
    U_alt2 = b_red * alt2_red + b_cost * alt2_cost,
    U_alt3 = b_red * alt3_red + b_cost * alt3_cost,
    
    exp_SQ = exp(U_SQ),
    exp_alt2 = exp(U_alt2),
    exp_alt3 = exp(U_alt3),
    denom = exp_SQ + exp_alt2 + exp_alt3,
    
    P_SQ = exp_SQ / denom,
    P_alt2 = exp_alt2 / denom,
    P_alt3 = exp_alt3 / denom
  ) %>%
  select(
    card_id, pair_id, card_type,
    SQ_label, SQ_red, SQ_cost, U_SQ, P_SQ,
    alt2_label, alt2_red, alt2_cost, U_alt2, P_alt2,
    alt3_label, alt3_red, alt3_cost, U_alt3, P_alt3,
    swap_order
  )

View(selected_12_randomized)


# ------------------------------------------------------------
# 10. Export results
# ------------------------------------------------------------

write_xlsx(
  list(
    candidate_45_unique_choice_sets = candidate_45,
    candidate_for_spdesign = candidate_for_spdesign,
    selected_12_spdesign_normalized = selected_12_normalized,
    selected_12_spdesign_randomized = selected_12_randomized
  ),
  "~/Desktop/spdesign_selected_12_cards_final.xlsx"
)

cat("Done. File exported to Desktop: spdesign_selected_12_cards_final.xlsx\n")
