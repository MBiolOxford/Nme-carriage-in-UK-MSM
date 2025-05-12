library(dplyr)
library(tidyverse)
library(readxl)

# Read raw data
data <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/Clinical statistical analysis.xlsx", 
                   sheet = "ChiSqr")

# Check the class (e.g. numerical/categorical) of the variables  - correct
glimpse(data)

# variables' name
names(data)

# Multivariate logistic regression 
multi_model <- glm(Carrier ~ HIV + `Throat gono NAAT` + `Urethral gono NAAT`, 
                   data=data, family=binomial)
summary(multi_model)
# To convert log-odds to odds ratios
exp(coef(multi_model))
exp(confint(multi_model))

anova(multi_model, test = "Chisq")

