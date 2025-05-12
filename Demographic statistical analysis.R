#### DEMOGRAPHIC data
# Set random seed for consistent results
set.seed(123)


# 1. Create contingency tables

# Age data
age_data <- matrix(
  c(3, 8,  # 18 to 24
    12, 34, # 25 to 34
    8, 47,  # 35 to 44
    9, 29,  # 45 to 54
    5, 14,  # 55 to 64
    0, 5),   # 65 to 74
  nrow = 6, byrow = TRUE,
  dimnames = list(
    c("18 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74"),
    c("Group1", "Group2")  # Modify the names for your specific group categories
  )
)

# Ethnicity data
ethnicity_data <- matrix(
  c(0, 8,     # Any other Asian background
    0, 2,     # Any other Black background
    1, 2,     # Any other ethnic group
    11, 37,   # Any other white background
    1, 8,    # Asian or Asian British: Chinese
    1, 2,    # Asian or Asian British: Indian
    0, 3,    # Asian or Asian British: Pakistani
    1, 2,    # Black or black British: African
    2, 5,   # Black or black British: Caribbean
    0, 7,     # Latino
    1, 1,     # Mixed: White and Asian
    0, 2,     # Mixed: White and Black African
    1, 3,     # Mixed: White and Black Caribbean
    12, 49,   # White British
    2, 4,     # White Irish
    4, 2),    # Other
  nrow = 16, byrow = TRUE,
  dimnames = list(
    c("Any_other_Asian", "Any_other_Black", "Any_other_ethnic", "Any_other_white", 
      "Chinese", "Indian", "Pakistani", "African", "Caribbean", "Latino", 
      "White_Asian", "White_Black_African", "White_Black_Caribbean", "White_British", 
      "White_Irish", "Other"),
    c("Group1", "Group2")
  )
)


# Sexual orientation data
sexual_orientation_data <- matrix(
  c(1, 10, # Bisexual
    0, 1,  # Heterosexual
    35, 122, # Homosexual
    1, 2,     # Pansexual
    0, 2),    # No label
  nrow = 5, byrow = TRUE,
  dimnames = list(
    c("Bisexual", "Heterosexual", "Homosexual", "Pansexual", "No_label"),
    c("Group1", "Group2")
  )
)

# Household number data
household_data <- matrix(
  c(14, 36,# 0
    17, 69,   # 1
    1, 17,   # 2
    3, 7,    # 3
    2, 4,   # 4
    0, 4),     # ≥5
  nrow = 6, byrow = TRUE,
  dimnames = list(
    c("0", "1", "2", "3", "4", "≥5"),
    c("Group1", "Group2")
  )
)

# 2. Run Fisher's Exact Test with Monte Carlo

# Age data
age_test <- fisher.test(
  age_data,
  simulate.p.value = TRUE,
  B = 1e6  # 1,000,000 replicates
)

# Ethnicity data
ethnicity_test <- fisher.test(
  ethnicity_data,
  simulate.p.value = TRUE,
  B = 1e6
)

# Sexual Orientation data
sexual_orientation_test <- fisher.test(
  sexual_orientation_data,
  simulate.p.value = TRUE,
  B = 1e6
)

# Household number data
household_test <- fisher.test(
  household_data,
  simulate.p.value = TRUE,
  B = 1e6
)


# 3. Print results

cat("\n=== Age Group Test ===\n")
print(age_test)

cat("\n=== Ethnicity Test ===\n")
print(ethnicity_test)

cat("\n=== Sexual Orientation Test ===\n")
print(sexual_orientation_test)

cat("\n=== Household Number Test ===\n")
print(household_test)


  
