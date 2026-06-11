# ============================================================
# D-error comparison using the same method as selection.R
# Compare 8, 10, 12, and 15 selected cards
#
# Important:
# - The 12-card design uses exactly the same method as selection.R
# - seed = 123
# - same utility
# - same candidate set
# - same spdesign control parameters
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

library(spdesign)
library(dplyr)
library(writexl)
library(ggplot2)

output_dir <- "~/Desktop"

# ------------------------------------------------------------
# 1. Define admissible policy profiles
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 4. Utility functions
# ------------------------------------------------------------

utility <- list(
  SQ   = "b_red[0.01] * red[c(0,25,50,75,100)] + b_cost[-0.05] * cost[0:6]",
  alt2 = "b_red * red + b_cost * cost",
  alt3 = "b_red * red + b_cost * cost"
)

# ------------------------------------------------------------
# 5. Manual D-error functions
# ------------------------------------------------------------

b_red <- 0.01
b_cost <- -0.05
K <- 2

compute_card_information <- function(row_data) {
  
  X <- matrix(
    c(
      row_data$SQ_red,   row_data$SQ_cost,
      row_data$alt2_red, row_data$alt2_cost,
      row_data$alt3_red, row_data$alt3_cost
    ),
    nrow = 3,
    ncol = 2,
    byrow = TRUE
  )
  
  V <- c(
    b_red * row_data$SQ_red   + b_cost * row_data$SQ_cost,
    b_red * row_data$alt2_red + b_cost * row_data$alt2_cost,
    b_red * row_data$alt3_red + b_cost * row_data$alt3_cost
  )
  
  p <- exp(V) / sum(exp(V))
  
  W <- diag(as.numeric(p)) - tcrossprod(as.numeric(p))
  
  M <- t(X) %*% W %*% X
  
  return(M)
}

compute_design_derror <- function(design_data) {
  
  M_total <- matrix(0, nrow = K, ncol = K)
  
  for (i in 1:nrow(design_data)) {
    M_total <- M_total + compute_card_information(design_data[i, ])
  }
  
  det_M <- det(M_total)
  
  if (det_M <= 0 || is.na(det_M)) {
    return(NA_real_)
  }
  
  d_error <- det_M^(-1 / K)
  
  return(d_error)
}

# ------------------------------------------------------------
# 6. Function: run spdesign using selection.R method
# ------------------------------------------------------------

run_selection_method <- function(n_cards) {
  
  set.seed(123)
  
  design_sp <- generate_design(
    utility = utility,
    rows = n_cards,
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
  
  return(design_sp)
}

# ------------------------------------------------------------
# 7. Compare 8, 10, 12, and 15 cards
# ------------------------------------------------------------

design_sizes <- c(8, 10, 12, 15)

design_objects <- list()
selected_designs <- list()
derror_results <- data.frame()

for (n in design_sizes) {
  
  cat("\n========================================\n")
  cat("Generating design with", n, "cards using selection.R method\n")
  cat("========================================\n")
  
  design_n <- run_selection_method(n)
  
  print(design_n)
  print(summary(design_n))
  
  selected_raw <- as.data.frame(design_n$design)
  
  d_error_n <- compute_design_derror(selected_raw)
  
  selected_normalized <- selected_raw %>%
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
  
  design_objects[[paste0("design_", n, "_cards")]] <- design_n
  selected_designs[[paste0("selected_", n, "_cards")]] <- selected_normalized
  
  derror_results <- bind_rows(
    derror_results,
    data.frame(
      n_cards = n,
      d_error = d_error_n
    )
  )
  
  cat("Manual D-error for", n, "cards:", d_error_n, "\n")
}

# ------------------------------------------------------------
# 8. Add percentage reduction from previous design size
# ------------------------------------------------------------

derror_results <- derror_results %>%
  arrange(n_cards) %>%
  mutate(
    reduction_from_previous_percent =
      100 * (lag(d_error) - d_error) / lag(d_error)
  )

cat("\nD-error comparison:\n")
print(derror_results)

# ------------------------------------------------------------
# 9. Plot D-error comparison
# ------------------------------------------------------------

p <- ggplot(
  derror_results,
  aes(x = n_cards, y = d_error)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = design_sizes) +
  labs(
    title = "D-error comparison: 8, 10, 12, and 15 cards",
    x = "Number of selected cards",
    y = "D-error"
  ) +
  theme_minimal(base_size = 13)

print(p)

ggsave(
  filename = file.path(output_dir, "derror_comparison_selection_method.png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "derror_comparison_selection_method.pdf"),
  plot = p,
  width = 7,
  height = 5
)

# Base R version
png(
  filename = file.path(output_dir, "derror_comparison_selection_method_baseR.png"),
  width = 900,
  height = 700,
  res = 120
)

plot(
  derror_results$n_cards,
  derror_results$d_error,
  type = "b",
  pch = 1,
  lwd = 1.5,
  xlab = "Number of selected cards",
  ylab = "D-error",
  main = "D-error comparison: 8, 10, 12, and 15 cards",
  xaxt = "n"
)

axis(1, at = derror_results$n_cards)

dev.off()

# ------------------------------------------------------------
# 10. Export Excel
# ------------------------------------------------------------

write_xlsx(
  c(
    list(
      candidate_45_unique_choice_sets = candidate_45,
      candidate_for_spdesign = candidate_for_spdesign,
      derror_comparison = derror_results
    ),
    selected_designs
  ),
  file.path(output_dir, "spdesign_derror_comparison_selection_method.xlsx")
)

cat("\nDone. Files exported to Desktop:\n")
cat("- spdesign_derror_comparison_selection_method.xlsx\n")
cat("- derror_comparison_selection_method.png\n")
cat("- derror_comparison_selection_method.pdf\n")
cat("- derror_comparison_selection_method_baseR.png\n")