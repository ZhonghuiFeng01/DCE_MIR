# ################################################################# #
#### DESCRIPTIVE STATISTICS: RESPONDENT CHARACTERISTICS          ####
# ################################################################# #

rm(list = ls())

setwd("/Users/fengzhonghui/Desktop/Courses/MIR")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ================================================================ #
# 1. Load sample assignment
# ================================================================ #

apollo_database <- read_csv("apollo_database.csv", show_col_types = FALSE)

respondent_sample <- apollo_database %>%
  distinct(ID, sample)

# ================================================================ #
# 2. Load Qualtrics respondent-level data
# ================================================================ #

raw <- read_csv(
  "database.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# Remove Qualtrics metadata rows
raw <- raw[-c(1, 2), ]

raw <- raw %>%
  mutate(ID = row_number())

if ("Progress" %in% names(raw)) raw$Progress <- as.numeric(raw$Progress)
if ("Finished" %in% names(raw)) raw$Finished <- as.numeric(raw$Finished)

if (all(c("Progress", "Finished") %in% names(raw))) {
  raw <- raw %>%
    filter(Finished == 1, Progress == 100)
}

# ================================================================ #
# 3. Select variables
# ================================================================ #

socio_data <- raw %>%
  select(
    ID,
    household_size_raw = Q1,
    gender_raw = Q2,
    education_raw = Q3,
    municipality_raw = Q4,
    related_field_raw = Q5,
    food_expenditure = Q6_0,
    organic_raw = Q7,
    heard_pesticides_raw = Q8
  ) %>%
  left_join(respondent_sample, by = "ID")

# ================================================================ #
# 4. Recode variables according to Qualtrics internal values
# ================================================================ #

socio_data <- socio_data %>%
  mutate(
    household_size_raw = as.numeric(household_size_raw),
    gender_raw = as.numeric(gender_raw),
    education_raw = as.numeric(education_raw),
    municipality_raw = as.numeric(municipality_raw),
    related_field_raw = as.numeric(related_field_raw),
    food_expenditure = as.numeric(food_expenditure),
    organic_raw = as.numeric(organic_raw),
    heard_pesticides_raw = as.numeric(heard_pesticides_raw),
    
    # Q1 household size:
    # 4=1, 5=2, ..., 10=7, 11=8 or more
    household_size = case_when(
      household_size_raw == 4 ~ 1,
      household_size_raw == 5 ~ 2,
      household_size_raw == 6 ~ 3,
      household_size_raw == 7 ~ 4,
      household_size_raw == 8 ~ 5,
      household_size_raw == 9 ~ 6,
      household_size_raw == 10 ~ 7,
      household_size_raw == 11 ~ 8,
      TRUE ~ NA_real_
    ),
    
    # Q2 gender:
    gender = case_when(
      gender_raw == 2 ~ "Male",
      gender_raw == 5 ~ "Female",
      gender_raw == 6 ~ "Non-binary / third gender",
      gender_raw == 7 ~ "Prefer not to say",
      TRUE ~ NA_character_
    ),
    female = ifelse(gender == "Female", 1, 0),
    
    # Q3 education:
    education = case_when(
      education_raw == 3 ~ "No diploma or primary education",
      education_raw == 4 ~ "Secondary education",
      education_raw == 5 ~ "Bachelor's degree",
      education_raw == 6 ~ "Master's degree",
      education_raw == 7 ~ "Doctoral degree or higher",
      education_raw == 8 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    # Q4 municipality:
    municipality = case_when(
      municipality_raw == 1 ~ "Urban",
      municipality_raw == 4 ~ "Suburban",
      municipality_raw == 5 ~ "Small town",
      municipality_raw == 6 ~ "Rural",
      TRUE ~ NA_character_
    ),
    urban = ifelse(municipality == "Urban", 1, 0),
    suburban = ifelse(municipality == "Suburban", 1, 0),
    small_town = ifelse(municipality == "Small town", 1, 0),
    rural = ifelse(municipality == "Rural", 1, 0),
    
    # Q5 related field:
    related_field = case_when(
      related_field_raw == 1 ~ "Yes, work",
      related_field_raw == 4 ~ "Yes, study",
      related_field_raw == 5 ~ "Yes, both work and study",
      related_field_raw == 6 ~ "No, work in another field",
      related_field_raw == 7 ~ "No, study in another field",
      related_field_raw == 8 ~ "No, not currently working or studying",
      TRUE ~ NA_character_
    ),
    related_agri_food_env = ifelse(related_field_raw %in% c(1, 4, 5), 1, 0),
    
    # Q7 organic food:
    organic_frequency = case_when(
      organic_raw == 4 ~ "Almost everyday",
      organic_raw == 7 ~ "Once or several times per week",
      organic_raw == 8 ~ "One to three times per month",
      organic_raw == 9 ~ "Almost never",
      TRUE ~ NA_character_
    ),
    
    # Q8 pesticides awareness:
    heard_pesticides = case_when(
      heard_pesticides_raw == 1 ~ "Yes",
      heard_pesticides_raw == 4 ~ "No",
      TRUE ~ NA_character_
    ),
    heard_pesticides_dummy = ifelse(heard_pesticides == "Yes", 1, 0)
  )

# Check recoding
cat("\nRecoding checks:\n")
print(table(socio_data$household_size, useNA = "ifany"))
print(table(socio_data$gender, useNA = "ifany"))
print(table(socio_data$education, useNA = "ifany"))
print(table(socio_data$municipality, useNA = "ifany"))
print(table(socio_data$related_field, useNA = "ifany"))
print(table(socio_data$organic_frequency, useNA = "ifany"))
print(table(socio_data$heard_pesticides, useNA = "ifany"))

# ================================================================ #
# 5. Helper functions
# ================================================================ #

mean_by_sample <- function(data, var, label) {
  out <- data %>%
    group_by(sample) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = sample, values_from = value)
  
  pval <- tryCatch({
    summary(aov(as.formula(paste(var, "~ sample")), data = data))[[1]][["Pr(>F)"]][1]
  }, error = function(e) NA_real_)
  
  out %>%
    mutate(
      variable = label,
      p_value = pval
    ) %>%
    select(variable, S1, S2, S3, p_value)
}

categorical_pvalue <- function(data, var) {
  tab <- table(data[[var]], data$sample)
  tryCatch({
    suppressWarnings(chisq.test(tab)$p.value)
  }, error = function(e) NA_real_)
}

category_distribution <- function(data, var, label_prefix) {
  pval <- categorical_pvalue(data, var)
  
  dist <- data %>%
    filter(!is.na(.data[[var]])) %>%
    count(sample, .data[[var]]) %>%
    group_by(sample) %>%
    mutate(value = n / sum(n)) %>%
    ungroup() %>%
    rename(category = .data[[var]]) %>%
    select(sample, category, value) %>%
    pivot_wider(names_from = sample, values_from = value) %>%
    mutate(
      variable = paste0("  - ", category),
      p_value = NA_real_
    ) %>%
    select(variable, S1, S2, S3, p_value)
  
  header <- tibble(
    variable = label_prefix,
    S1 = NA_real_,
    S2 = NA_real_,
    S3 = NA_real_,
    p_value = pval
  )
  
  bind_rows(header, dist)
}

# ================================================================ #
# 6. Main descriptive statistics
# ================================================================ #

desc_main <- bind_rows(
  mean_by_sample(socio_data, "household_size", "Household size"),
  mean_by_sample(socio_data, "female", "Female"),
  mean_by_sample(socio_data, "urban", "Living in an urban municipality"),
  mean_by_sample(socio_data, "suburban", "Living in a suburban municipality"),
  mean_by_sample(socio_data, "small_town", "Living in a small town"),
  mean_by_sample(socio_data, "rural", "Living in a rural municipality"),
  mean_by_sample(socio_data, "related_agri_food_env", "Work/study related to agriculture, food or environment"),
  mean_by_sample(socio_data, "food_expenditure", "Food expenditure per week"),
  mean_by_sample(socio_data, "heard_pesticides_dummy", "Had heard about pesticide use in agriculture")
)

education_table <- category_distribution(
  socio_data,
  "education",
  "Education"
)

organic_table <- category_distribution(
  socio_data,
  "organic_frequency",
  "Frequency of organic food consumption"
)

gender_table <- category_distribution(
  socio_data,
  "gender",
  "Gender"
)

municipality_table <- category_distribution(
  socio_data,
  "municipality",
  "Municipality type"
)

related_field_table <- category_distribution(
  socio_data,
  "related_field",
  "Work/study field"
)

# ================================================================ #
# 7. Combine and format final table
# ================================================================ #

desc_table <- bind_rows(
  desc_main,
  gender_table,
  education_table,
  municipality_table,
  related_field_table,
  organic_table
) %>%
  mutate(
    S1 = round(S1, 3),
    S2 = round(S2, 3),
    S3 = round(S3, 3),
    p_value = round(p_value, 3)
  )

cat("\nDescriptive statistics by sample:\n")
print(desc_table, n = 100)

write_csv(desc_table, "descriptive_statistics_by_sample.csv")

cat("\nSaved: descriptive_statistics_by_sample.csv\n")