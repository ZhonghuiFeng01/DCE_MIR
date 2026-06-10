# ################################################################# #
#### CREATE APOLLO DATABASE FROM QUALTRICS RESPONSES              ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Set working directory
setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

### Load packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ################################################################# #
#### 1. LOAD QUALTRICS RESPONSES                                  ####
# ################################################################# #

# database.csv = Qualtrics Export values file
raw <- read_csv(
  "database.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# Qualtrics usually exports:
# row 1 = question text
# row 2 = import ID
# actual respondent data start after these two rows.
raw <- raw[-c(1, 2), ]

# Create respondent ID
raw <- raw %>%
  mutate(ID = row_number())

# Convert Progress and Finished if they exist
if ("Progress" %in% names(raw)) {
  raw$Progress <- as.numeric(raw$Progress)
}

if ("Finished" %in% names(raw)) {
  raw$Finished <- as.numeric(raw$Finished)
}

# Keep completed responses only
if (all(c("Progress", "Finished") %in% names(raw))) {
  raw <- raw %>%
    filter(Finished == 1, Progress == 100)
}

cat("Number of completed respondents:", nrow(raw), "\n")

# ################################################################# #
#### 2. IDENTIFY CHOICE QUESTIONS                                 ####
# ################################################################# #

# Expected choice task columns:
# S1_1, S1_2, ..., S1_12
# S2_1, ..., S2_12
# S3_1, ..., S3_12

choice_cols <- names(raw)[str_detect(names(raw), "^S[123]_\\d+$")]

cat("\nChoice columns found:\n")
print(choice_cols)

if (length(choice_cols) == 0) {
  stop("No choice columns found. Check that Qualtrics columns are named S1_1, ..., S3_12.")
}

# ################################################################# #
#### 3. RESHAPE RESPONSES AND RECODE CHOICE VALUES                ####
# ################################################################# #

# Qualtrics may encode options as:
# 1,2,3 for some blocks
# 4,5,6 for copied blocks
#
# We recode:
# 1 or 4 -> Option 1
# 2 or 5 -> Option 2
# 3 or 6 -> Option 3

choice_task <- raw %>%
  select(ID, all_of(choice_cols)) %>%
  pivot_longer(
    cols = all_of(choice_cols),
    names_to = "question",
    values_to = "choice_raw"
  ) %>%
  filter(!is.na(choice_raw), choice_raw != "") %>%
  mutate(
    sample = str_extract(question, "^S[123]"),
    task = as.integer(str_extract(question, "\\d+$")),
    choice_raw = as.integer(choice_raw),
    choice = case_when(
      choice_raw %in% c(1, 4) ~ 1L,
      choice_raw %in% c(2, 5) ~ 2L,
      choice_raw %in% c(3, 6) ~ 3L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(choice)) %>%
  select(ID, sample, task, choice_raw, choice) %>%
  arrange(ID, sample, task)

# Checks
cat("\nRaw choice values:\n")
print(table(choice_task$choice_raw))

cat("\nRecoded choice values:\n")
print(table(choice_task$choice))

cat("\nRaw choice values by sample:\n")
print(table(choice_task$sample, choice_task$choice_raw))

cat("\nRecoded choice values by sample:\n")
print(table(choice_task$sample, choice_task$choice))

cat("\nNumber of answered choice tasks per respondent:\n")
print(table(table(choice_task$ID)))

# ################################################################# #
#### 4. LOAD CHOICE DESIGN                                        ####
# ################################################################# #

# choice_design.csv should have:
# sample, task, original_card, alt, red, cost

choice_design <- read_csv(
  "choice_design.csv",
  show_col_types = FALSE
)

choice_design <- choice_design %>%
  mutate(
    sample = as.character(sample),
    task = as.integer(task),
    original_card = as.integer(original_card),
    alt = as.integer(alt),
    red = as.numeric(red),
    cost = as.numeric(cost)
  )

cat("\nChoice design checks:\n")
cat("Rows in choice_design:", nrow(choice_design), "\n")
print(table(choice_design$sample))

# Check each sample-task has 3 alternatives
check_design <- choice_design %>%
  group_by(sample, task) %>%
  summarise(n_alt = n(), .groups = "drop")

if (any(check_design$n_alt != 3)) {
  warning("Some sample-task combinations in choice_design do not have exactly 3 alternatives.")
}

# ################################################################# #
#### 5. MERGE RESPONSES WITH CHOICE DESIGN                        ####
# ################################################################# #

# Alternative-level database:
# one row = respondent × task × alternative

database_long <- choice_task %>%
  left_join(choice_design, by = c("sample", "task")) %>%
  mutate(
    chosen = ifelse(alt == choice, 1L, 0L),
    T2 = ifelse(sample == "S2", 1L, 0L),
    T3 = ifelse(sample == "S3", 1L, 0L)
  ) %>%
  arrange(ID, sample, task, alt)

cat("\nPreview of database_long:\n")
print(head(database_long, 30))

cat("\nRows in database_long:", nrow(database_long), "\n")

# Check merged data
check_merge <- database_long %>%
  group_by(ID, sample, task) %>%
  summarise(
    n_alt = n(),
    n_chosen = sum(chosen),
    .groups = "drop"
  )

cat("\nCheck merged data:\n")
print(head(check_merge, 30))

if (any(check_merge$n_alt != 3)) {
  warning("Some respondent-task combinations do not have exactly 3 alternatives.")
}

if (any(check_merge$n_chosen != 1)) {
  warning("Some respondent-task combinations do not have exactly one chosen alternative.")
}

# ################################################################# #
#### 6. CREATE APOLLO DATABASE                                    ####
# ################################################################# #

# Apollo-style wide database:
# one row = respondent × task
# columns = red1, red2, red3, cost1, cost2, cost3

apollo_database <- database_long %>%
  select(ID, sample, task, choice_raw, choice, T2, T3, alt, red, cost) %>%
  pivot_wider(
    names_from = alt,
    values_from = c(red, cost),
    names_glue = "{.value}{alt}"
  ) %>%
  arrange(ID, sample, task)

cat("\nPreview of apollo_database:\n")
print(head(apollo_database, 20))

cat("\nRows in apollo_database:", nrow(apollo_database), "\n")

# Final check: choice must be only 1, 2, or 3
cat("\nFinal choice values in apollo_database:\n")
print(table(apollo_database$choice))

if (!all(apollo_database$choice %in% c(1, 2, 3))) {
  stop("Choice variable still contains values other than 1, 2, 3.")
}

# ################################################################# #
#### 7. SAVE OUTPUT FILES                                         ####
# ################################################################# #

write_csv(database_long, "database_long.csv")
write_csv(apollo_database, "apollo_database.csv")

cat("\nFiles created successfully:\n")
cat("- database_long.csv\n")
cat("- apollo_database.csv\n")