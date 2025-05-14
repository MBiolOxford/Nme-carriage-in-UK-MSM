# GRAPH OF MSMCARR LINEAGE FREQUENCIES IN MSMCARR AND UK CARRIERS

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load datasets (from Sheet1)
MSM_data <- read_excel("C:/Users/OneDrive - Nexus365/Year 4/MSM vs Non-MSM stat analysis/LIN Code/LIN Code MSMCARR.xlsx", sheet = "Sheet1")
UK_data <- read_excel("C:/Users/OneDrive - Nexus365/Year 4/MSM vs Non-MSM stat analysis/LIN Code/LIN Code All UK Carriers.xlsx", sheet = "Sheet1")

# Lineages of interest
lineages <- c(
  "29_1_1", "34_0_0", "43_0_0", "48_0_0", "0_16_0", "75_0_0",
  "0_0_0", "0_24_0", "10_1_0", "29_0_0", "47_2_0", "47_2_1",
  "69_0_0", "72_0_0", "72_1_1", "79_0_0"
)

# Prepare MSM data
MSM_plot <- MSM_data %>%
  mutate(Bin = trimws(Bin)) %>%
  filter(Bin %in% lineages) %>%
  mutate(Percent = as.numeric(as.character(Percentage)), Group = "MSMCARR") %>% # Explicit conversion
  select(Bin, Percent, Group)

# Prepare UK data
UK_plot <- UK_data %>%
  mutate(Bin = trimws(Bin)) %>%
  filter(Bin %in% lineages) %>%
  mutate(Percent = as.numeric(Percentage), Group = "UK") %>%
  select(Bin, Percent, Group)

# Combine both datasets
plot_data <- bind_rows(MSM_plot, UK_plot) %>%
  rename(Lineage = Bin)

# Ensure Lineage is a factor with consistent order
plot_data$Lineage <- factor(plot_data$Lineage, levels = lineages)

# Plot
ggplot(plot_data, aes(x = Lineage, y = Percent, fill = Group)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    x = "Lineages",
    y = "Percentage (%)",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),  # Push x-axis label down
    axis.title.y = element_text(margin = margin(r = 10), angle = 0, vjust = 0.53)   # Push y-axis label left
  ) +
  scale_fill_manual(values = c("MSMCARR" = "blue", "UK" = "orange"))

---------------------------------------------------------------------------------------------
# STATISTICAL ASSOCIATION OF MSMCARR LINEAGES IN MSMCARR AND UK CARRIERS

library(dplyr)
library(readxl)
library(tidyr)

# Load data
MSM_data <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/MSM vs Non-MSM stat analysis/LIN Code/LIN Code MSMCARR.xlsx", sheet = "Sheet1")
UK_data <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/MSM vs Non-MSM stat analysis/LIN Code/LIN Code All UK Carriers.xlsx", sheet = "Sheet1")

# Define lineages of interest
lineages <- c(
  "29_1_1", "34_0_0", "43_0_0", "48_0_0", "0_16_0", "75_0_0",
  "0_0_0", "0_24_0", "10_1_0", "29_0_0", "47_2_0", "47_2_1",
  "69_0_0", "72_0_0", "72_1_1", "79_0_0"
)

# Bins for Chi-squared
chi_bins <- c("29_1_1", "34_0_0")

# Clean and combine data
MSM <- MSM_data %>%
  mutate(Bin = trimws(Bin)) %>%
  filter(Bin %in% lineages) %>%
  select(Bin, Count) %>%
  mutate(Group = "MSM")

UK <- UK_data %>%
  mutate(Bin = trimws(Bin)) %>%
  filter(Bin %in% lineages) %>%
  select(Bin, Count) %>%
  mutate(Group = "UK")

all_data <- bind_rows(MSM, UK)

# Get total counts
total_MSM <- 35
total_UK <- 4617

# Loop through each bin
results <- lapply(lineages, function(bin) {
  counts <- all_data %>%
    filter(Bin == bin) %>%
    select(Group, Count) %>%
    pivot_wider(names_from = Group, values_from = Count, values_fill = 0)
  
  # Extract counts
  in_bin_MSM <- counts$MSM
  in_bin_UK <- counts$UK
  not_in_bin_MSM <- total_MSM - in_bin_MSM
  not_in_bin_UK <- total_UK - in_bin_UK
  
  # Create 2x2 contingency table
  contingency <- matrix(
    c(in_bin_MSM, in_bin_UK,
      not_in_bin_MSM, not_in_bin_UK),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(Status = c("Yes", "No"), Group = c("MSM", "UK"))
  )
  
  # Apply appropriate test and calculate odds ratio
  a <- in_bin_MSM
  b <- not_in_bin_MSM
  c <- in_bin_UK
  d <- not_in_bin_UK
  
  if (bin %in% chi_bins) {
    # Chi-squared test for specific bins
    test <- chisq.test(contingency)
    test_type <- "Chi-squared"
    
    # Odds ratio calculation
    odds_ratio <- ifelse(b * c == 0, NA, (a * d) / (b * c))
  } else {
    # Fisher's Exact test for other bins
    test <- fisher.test(contingency)
    test_type <- "Fisher's Exact"
    
    # Odds ratio calculation
    odds_ratio <- as.numeric(test$estimate)
  }
  
  # Calculate standard error of log(OR)
  if (!is.na(odds_ratio)) {
    se_log_OR <- sqrt(1/a + 1/b + 1/c + 1/d)
    
    # Calculate 95% Confidence Interval for log(OR)
    lower_CI <- exp(log(odds_ratio) - 1.96 * se_log_OR)
    upper_CI <- exp(log(odds_ratio) + 1.96 * se_log_OR)
  } else {
    lower_CI <- NA
    upper_CI <- NA
  }
  
  data.frame(
    Bin = bin,
    MSM_Count = in_bin_MSM,
    UK_Count = in_bin_UK,
    p_value = test$p.value,
    test = test_type,
    odds_ratio = odds_ratio,
    lower_CI = lower_CI,
    upper_CI = upper_CI
  )
})

# Combine results
results_df <- do.call(rbind, results) %>%
  arrange(p_value)

# View table
print(results_df)
