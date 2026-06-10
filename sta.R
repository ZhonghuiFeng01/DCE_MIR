# ################################################################# #
#### DESCRIPTIVE STATISTICS FOR MIR DCE PILOT SURVEY              ####
# ################################################################# #

rm(list = ls())

setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ################################################################# #
#### 1. LOAD CLEANED DCE DATABASES                                ####
# ################################################################# #

apollo_database <- read_csv("apollo_database.csv", show_col_types = FALSE)
database_long   <- read_csv("database_long.csv", show_col_types = FALSE)

# Check structure
cat("Rows in apollo_database:", nrow(apollo_database), "\n")
cat("Rows in database_long:", nrow(database_long), "\n")
cat("Number of respondents:", n_distinct(apollo_database$ID), "\n")

# ################################################################# #
#### 2. SAMPLE SIZE AND TREATMENT DISTRIBUTION                    ####
# ################################################################# #

sample_distribution <- apollo_database %>%
  distinct(ID, sample) %>%
  count(sample, name = "n_respondents") %>%
  mutate(
    share = n_respondents / sum(n_respondents),
    share_percent = round(100 * share, 2)
  )

cat("\nTreatment distribution:\n")
print(sample_distribution)

# Number of choice observations
choice_obs_summary <- tibble(
  n_respondents = n_distinct(apollo_database$ID),
  n_choice_tasks = nrow(apollo_database),
  n_alternative_level_obs = nrow(database_long)
)

cat("\nChoice observations summary:\n")
print(choice_obs_summary)

# ################################################################# #
#### 3. OPTION CHOICE DISTRIBUTION                                ####
# ################################################################# #

choice_distribution <- apollo_database %>%
  count(choice, name = "n_choices") %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  )

cat("\nOverall option choice distribution:\n")
print(choice_distribution)

choice_by_sample <- apollo_database %>%
  count(sample, choice, name = "n_choices") %>%
  group_by(sample) %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  ungroup()

cat("\nOption choice distribution by sample:\n")
print(choice_by_sample)

# ################################################################# #
#### 4. CHOSEN ATTRIBUTES: REDUCTION AND COST                     ####
# ################################################################# #

chosen_data <- database_long %>%
  filter(chosen == 1)

chosen_attr_summary <- chosen_data %>%
  summarise(
    n_choices = n(),
    mean_red = mean(red, na.rm = TRUE),
    sd_red = sd(red, na.rm = TRUE),
    mean_cost = mean(cost, na.rm = TRUE),
    sd_cost = sd(cost, na.rm = TRUE)
  )

cat("\nOverall chosen attribute summary:\n")
print(chosen_attr_summary)

chosen_attr_by_sample <- chosen_data %>%
  group_by(sample) %>%
  summarise(
    n_choices = n(),
    mean_red = mean(red, na.rm = TRUE),
    sd_red = sd(red, na.rm = TRUE),
    mean_cost = mean(cost, na.rm = TRUE),
    sd_cost = sd(cost, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nChosen attributes by sample:\n")
print(chosen_attr_by_sample)

# ################################################################# #
#### 5. SHARE OF STATUS QUO AND FULL BAN CHOICES                  ####
# ################################################################# #

sq_ban_summary <- chosen_data %>%
  mutate(
    choose_sq = ifelse(red == 0 & cost == 0, 1, 0),
    choose_ban = ifelse(red == 100 & cost == 6, 1, 0)
  ) %>%
  summarise(
    share_sq = mean(choose_sq, na.rm = TRUE),
    share_ban = mean(choose_ban, na.rm = TRUE),
    share_sq_percent = round(100 * share_sq, 2),
    share_ban_percent = round(100 * share_ban, 2)
  )

cat("\nOverall SQ and BAN choice shares:\n")
print(sq_ban_summary)

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

cat("\nSQ and BAN choice shares by sample:\n")
print(sq_ban_by_sample)

# ################################################################# #
#### 6. DISTRIBUTION OF CHOSEN REDUCTION LEVELS                   ####
# ################################################################# #

chosen_red_distribution <- chosen_data %>%
  count(red, name = "n_choices") %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  arrange(red)

cat("\nDistribution of chosen pesticide reduction levels:\n")
print(chosen_red_distribution)

chosen_red_by_sample <- chosen_data %>%
  count(sample, red, name = "n_choices") %>%
  group_by(sample) %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  ungroup() %>%
  arrange(sample, red)

cat("\nDistribution of chosen pesticide reduction levels by sample:\n")
print(chosen_red_by_sample)

# ################################################################# #
#### 7. DISTRIBUTION OF CHOSEN COST LEVELS                        ####
# ################################################################# #

chosen_cost_distribution <- chosen_data %>%
  count(cost, name = "n_choices") %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  arrange(cost)

cat("\nDistribution of chosen cost levels:\n")
print(chosen_cost_distribution)

chosen_cost_by_sample <- chosen_data %>%
  count(sample, cost, name = "n_choices") %>%
  group_by(sample) %>%
  mutate(
    share = n_choices / sum(n_choices),
    share_percent = round(100 * share, 2)
  ) %>%
  ungroup() %>%
  arrange(sample, cost)

cat("\nDistribution of chosen cost levels by sample:\n")
print(chosen_cost_by_sample)

# ################################################################# #
#### 8. RESPONDENT-LEVEL SOCIO-ECONOMIC VARIABLES                 ####
# ################################################################# #

# Load original Qualtrics data to extract respondent characteristics.
# This part may require adapting variable names depending on your Qualtrics export.

raw <- read_csv(
  "database.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

raw <- raw[-c(1, 2), ]

raw <- raw %>%
  mutate(ID = row_number())

if ("Progress" %in% names(raw)) {
  raw$Progress <- as.numeric(raw$Progress)
}

if ("Finished" %in% names(raw)) {
  raw$Finished <- as.numeric(raw$Finished)
}

if (all(c("Progress", "Finished") %in% names(raw))) {
  raw <- raw %>%
    filter(Finished == 1, Progress == 100)
}

# Print names to identify socio-economic variables
cat("\nColumn names in raw Qualtrics data:\n")
print(names(raw))

# You need to adapt these variable names after checking names(raw).
# Example placeholders:
# age_var <- "Q_age"
# household_var <- "Q_household_size"
# residence_var <- "Q_residence_area"
# food_exp_var <- "Q_food_expenditure"
# organic_var <- "Q_organic_frequency"

# Uncomment and adapt once variable names are identified:
#
# socio_data <- raw %>%
#   select(
#     ID,
#     age = Q_age,
#     household_size = Q_household_size,
#     residence_area = Q_residence_area,
#     food_expenditure = Q_food_expenditure,
#     organic_frequency = Q_organic_frequency
#   )
#
# socio_summary <- socio_data %>%
#   summarise(
#     mean_age = mean(as.numeric(age), na.rm = TRUE),
#     sd_age = sd(as.numeric(age), na.rm = TRUE),
#     mean_household_size = mean(as.numeric(household_size), na.rm = TRUE),
#     sd_household_size = sd(as.numeric(household_size), na.rm = TRUE),
#     mean_food_expenditure = mean(as.numeric(food_expenditure), na.rm = TRUE),
#     sd_food_expenditure = sd(as.numeric(food_expenditure), na.rm = TRUE)
#   )
#
# print(socio_summary)
#
# residence_distribution <- socio_data %>%
#   count(residence_area) %>%
#   mutate(share_percent = round(100 * n / sum(n), 2))
#
# organic_distribution <- socio_data %>%
#   count(organic_frequency) %>%
#   mutate(share_percent = round(100 * n / sum(n), 2))
#
# print(residence_distribution)
# print(organic_distribution)

# ################################################################# #
#### 9. SAVE DESCRIPTIVE TABLES                                   ####
# ################################################################# #

write_csv(sample_distribution, "desc_sample_distribution.csv")
write_csv(choice_obs_summary, "desc_choice_observations_summary.csv")
write_csv(choice_distribution, "desc_choice_distribution.csv")
write_csv(choice_by_sample, "desc_choice_by_sample.csv")
write_csv(chosen_attr_summary, "desc_chosen_attributes_summary.csv")
write_csv(chosen_attr_by_sample, "desc_chosen_attributes_by_sample.csv")
write_csv(sq_ban_summary, "desc_sq_ban_summary.csv")
write_csv(sq_ban_by_sample, "desc_sq_ban_by_sample.csv")
write_csv(chosen_red_distribution, "desc_chosen_red_distribution.csv")
write_csv(chosen_red_by_sample, "desc_chosen_red_by_sample.csv")
write_csv(chosen_cost_distribution, "desc_chosen_cost_distribution.csv")
write_csv(chosen_cost_by_sample, "desc_chosen_cost_by_sample.csv")

cat("\nDescriptive statistics tables saved successfully.\n")