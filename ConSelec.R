# ============================================================
# Conditional selection rates for BAN and SQ
# Files:
# - database.csv
# - choice_design_wide.csv
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(writexl)

# ------------------------------------------------------------
# 1. Set working directory
# ------------------------------------------------------------

setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

database_file <- "database.csv"
design_file   <- "choice_design_wide.csv"

# Check files
print(getwd())
print(list.files())

# ------------------------------------------------------------
# 2. Read Qualtrics database
# ------------------------------------------------------------

raw <- read_csv(database_file, show_col_types = FALSE)

database <- raw %>%
  slice(-(1:2))

# ------------------------------------------------------------
# 3. Convert respondent choices from wide to long format
# ------------------------------------------------------------

choice_long <- database %>%
  select(
    ResponseId,
    starts_with("S1_"),
    starts_with("S2_"),
    starts_with("S3_")
  ) %>%
  pivot_longer(
    cols = starts_with("S"),
    names_to = "sample_task",
    values_to = "chosen_option"
  ) %>%
  filter(!is.na(chosen_option), chosen_option != "") %>%
  mutate(
    sample = str_extract(sample_task, "^S[123]"),
    task = as.integer(str_extract(sample_task, "\\d+$")),
    chosen_option = as.integer(chosen_option)
  ) %>%
  select(ResponseId, sample, task, chosen_option)

print(choice_long %>% count(sample))

# ------------------------------------------------------------
# 4. Read choice design file correctly
# ------------------------------------------------------------

design_wide <- read_csv(
  design_file,
  show_col_types = FALSE,
  skip = 1
)

# Remove the empty first column
design_wide <- design_wide %>%
  select(-1)

# Check: these columns should appear:
# Sample, Task, Original_card, Alt1, Red1, Cost1, Alt2, Red2, Cost2, Alt3, Red3, Cost3
print(names(design_wide))
print(head(design_wide))

# ------------------------------------------------------------
# 5. Convert choice design to long format
# ------------------------------------------------------------

card_key <- design_wide %>%
  rename(
    sample = Sample,
    task = Task
  ) %>%
  mutate(
    sample = as.character(sample),
    task = as.integer(task)
  ) %>%
  pivot_longer(
    cols = c(Red1, Cost1, Red2, Cost2, Red3, Cost3),
    names_to = c(".value", "option"),
    names_pattern = "(Red|Cost)([123])"
  ) %>%
  mutate(
    option = as.integer(option),
    red = as.numeric(Red),
    cost = as.numeric(Cost)
  ) %>%
  select(sample, task, option, red, cost)

print(head(card_key, 12))
print(card_key %>% count(sample, task))

# ------------------------------------------------------------
# 6. Identify availability of BAN and SQ in each task
# ------------------------------------------------------------

availability <- card_key %>%
  mutate(
    is_ban = red == 100 & cost == 6,
    is_sq  = red == 0 & cost == 0
  ) %>%
  group_by(sample, task) %>%
  summarise(
    ban_available_in_task = any(is_ban),
    sq_available_in_task  = any(is_sq),
    
    ban_option = ifelse(
      any(is_ban),
      option[which(is_ban)[1]],
      NA_integer_
    ),
    
    sq_option = ifelse(
      any(is_sq),
      option[which(is_sq)[1]],
      NA_integer_
    ),
    
    .groups = "drop"
  )

print(availability)

# ------------------------------------------------------------
# 7. Merge choices with availability
# ------------------------------------------------------------

choice_analysis <- choice_long %>%
  left_join(
    availability,
    by = c("sample", "task")
  ) %>%
  mutate(
    ban_chosen = ban_available_in_task & chosen_option == ban_option,
    sq_chosen  = sq_available_in_task  & chosen_option == sq_option
  )

# Check unmatched choices
unmatched <- choice_analysis %>%
  filter(is.na(ban_available_in_task) | is.na(sq_available_in_task))

if (nrow(unmatched) > 0) {
  print(unmatched)
  stop("Some choices were not matched with the choice design.")
}

# ------------------------------------------------------------
# 8. Conditional selection rates
# ------------------------------------------------------------

conditional_rates <- choice_analysis %>%
  group_by(sample) %>%
  summarise(
    n_respondent_tasks = n(),
    
    ban_available = sum(ban_available_in_task, na.rm = TRUE),
    ban_chosen = sum(ban_chosen, na.rm = TRUE),
    ban_selection_rate_when_available =
      100 * ban_chosen / ban_available,
    
    sq_available = sum(sq_available_in_task, na.rm = TRUE),
    sq_chosen = sum(sq_chosen, na.rm = TRUE),
    sq_selection_rate_when_available =
      100 * sq_chosen / sq_available,
    
    .groups = "drop"
  ) %>%
  mutate(
    ban_selection_rate_when_available =
      round(ban_selection_rate_when_available, 2),
    sq_selection_rate_when_available =
      round(sq_selection_rate_when_available, 2)
  )

print(conditional_rates)

# ------------------------------------------------------------
# 9. Unconditional shares for comparison
# ------------------------------------------------------------

unconditional_shares <- choice_analysis %>%
  group_by(sample) %>%
  summarise(
    n_choices = n(),
    
    ban_chosen = sum(ban_chosen, na.rm = TRUE),
    ban_unconditional_share =
      100 * ban_chosen / n_choices,
    
    sq_chosen = sum(sq_chosen, na.rm = TRUE),
    sq_unconditional_share =
      100 * sq_chosen / n_choices,
    
    .groups = "drop"
  ) %>%
  mutate(
    ban_unconditional_share = round(ban_unconditional_share, 2),
    sq_unconditional_share = round(sq_unconditional_share, 2)
  )

print(unconditional_shares)

# ------------------------------------------------------------
# 10. Export results
# ------------------------------------------------------------

write_xlsx(
  list(
    card_key = card_key,
    availability = availability,
    choice_analysis = choice_analysis,
    conditional_rates = conditional_rates,
    unconditional_shares = unconditional_shares
  ),
  "conditional_selection_rates_BAN_SQ.xlsx"
)

cat("\nDone. Results exported to conditional_selection_rates_BAN_SQ.xlsx\n")
