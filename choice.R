# ################################################################# #
#### 5.3 CHOICE PATTERNS ACROSS TREATMENTS                       ####
# ################################################################# #

rm(list = ls())

setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

library(readr)
library(dplyr)
library(tidyr)

# Load cleaned databases
apollo_database <- read_csv("apollo_database.csv", show_col_types = FALSE)
database_long   <- read_csv("database_long.csv", show_col_types = FALSE)

# ================================================================ #
# 1. Option choice distribution by treatment
# ================================================================ #

choice_by_sample <- apollo_database %>%
  count(sample, choice, name = "n_choices") %>%
  group_by(sample) %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  ungroup()

print(choice_by_sample)

choice_by_sample_wide <- choice_by_sample %>%
  select(sample, choice, share_percent) %>%
  mutate(choice = paste0("Option ", choice)) %>%
  pivot_wider(names_from = choice, values_from = share_percent)

print(choice_by_sample_wide)

# ================================================================ #
# 2. Chosen reduction and chosen cost by treatment
# ================================================================ #

chosen_data <- database_long %>%
  filter(chosen == 1)

chosen_attr_by_sample <- chosen_data %>%
  group_by(sample) %>%
  summarise(
    n_choices = n(),
    mean_red = mean(red, na.rm = TRUE),
    sd_red = sd(red, na.rm = TRUE),
    mean_cost = mean(cost, na.rm = TRUE),
    sd_cost = sd(cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_red = round(mean_red, 2),
    sd_red = round(sd_red, 2),
    mean_cost = round(mean_cost, 2),
    sd_cost = round(sd_cost, 2)
  )

print(chosen_attr_by_sample)

# ================================================================ #
# 3. SQ and BAN choice shares by treatment
# ================================================================ #

sq_ban_by_sample <- chosen_data %>%
  mutate(
    choose_sq = ifelse(red == 0 & cost == 0, 1, 0),
    choose_ban = ifelse(red == 100 & cost == 6, 1, 0)
  ) %>%
  group_by(sample) %>%
  summarise(
    n_choices = n(),
    share_sq = mean(choose_sq, na.rm = TRUE),
    share_ban = mean(choose_ban, na.rm = TRUE),
    share_sq_percent = round(100 * share_sq, 2),
    share_ban_percent = round(100 * share_ban, 2),
    .groups = "drop"
  )

print(sq_ban_by_sample)

# ================================================================ #
# 4. Distribution of chosen reduction levels by treatment
# ================================================================ #

chosen_red_by_sample <- chosen_data %>%
  count(sample, red, name = "n_choices") %>%
  group_by(sample) %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  ungroup() %>%
  arrange(sample, red)

print(chosen_red_by_sample)

chosen_red_by_sample_wide <- chosen_red_by_sample %>%
  select(sample, red, share_percent) %>%
  mutate(red = paste0(red, "%")) %>%
  pivot_wider(names_from = red, values_from = share_percent)

print(chosen_red_by_sample_wide)

# ================================================================ #
# 5. Distribution of chosen cost levels by treatment
# ================================================================ #

chosen_cost_by_sample <- chosen_data %>%
  count(sample, cost, name = "n_choices") %>%
  group_by(sample) %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  ungroup() %>%
  arrange(sample, cost)

print(chosen_cost_by_sample)

chosen_cost_by_sample_wide <- chosen_cost_by_sample %>%
  select(sample, cost, share_percent) %>%
  mutate(cost = paste0("€", cost)) %>%
  pivot_wider(names_from = cost, values_from = share_percent)

print(chosen_cost_by_sample_wide)

# ================================================================ #
# 6. Save tables
# ================================================================ #

write_csv(choice_by_sample_wide, "choice_patterns_option_by_sample.csv")
write_csv(chosen_attr_by_sample, "choice_patterns_chosen_attr_by_sample.csv")
write_csv(sq_ban_by_sample, "choice_patterns_sq_ban_by_sample.csv")
write_csv(chosen_red_by_sample_wide, "choice_patterns_red_by_sample.csv")
write_csv(chosen_cost_by_sample_wide, "choice_patterns_cost_by_sample.csv")

cat("\nChoice pattern tables saved successfully.\n")