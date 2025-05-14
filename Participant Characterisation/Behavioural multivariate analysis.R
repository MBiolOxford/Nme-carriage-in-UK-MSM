library(dplyr)
library(tidyverse)
library(readxl)

# Read raw data
data <- read_excel("C:/Users/OneDrive - Nexus365/Year 4/Behavioural statistical analysis.xlsx", 
                              sheet = "ChiSqr")

# Check the class (e.g. numerical/categorical) of the variables  - correct
glimpse(data)

# variables' name
names(data)

# Check individual's variable

# Carrier (outcome variable)
table(data$Carrier, useNA = 'always')

# Waterpipe
table(data$Waterpipe, useNA = 'always')

# Males had sex with (3 months)
data$`Males had sex with (3 months)` <- factor(data$`Males had sex with (3 months)`, levels=c("0", "1-2", "3-5", ">6"))
table(data$`Males had sex with (3 months)`, useNA = 'always') 

# Condomless anal sex with males (3 months)
# Initially, '>6' group is ordered as the first group
# Re-order the categories (for table and plot) (and you can do the same for other variables)
data$`Condomless anal sex with males (3 months)` <- factor(data$`Condomless anal sex with males (3 months)`, levels=c("0", "1-2", "3-5", ">6"))
table(data$`Condomless anal sex with males (3 months)`, useNA = 'always') 
# 0  1-2  3-5   >6   <NA> 
# 33   64   47   30    0 

# Mutual masturbation (2 weeks)
table(data$`Mutual masturbation (2 weeks)`, useNA = 'always')

# Performing blowjob (2 weeks)
table(data$`Performing blowjob (2 weeks)`, useNA = 'always')

# Lick anus (2 weeks)
table(data$`Lick anus (2 weeks)`, useNA = 'always')

# PrEP
table(data$PrEP, useNA = 'always')

# Multivariate logistic regression 
multi_model <- glm(Carrier ~ Waterpipe + `Males had sex with (3 months)` + `Condomless anal sex with males (3 months)` + 
                               `Mutual masturbation (2 weeks)` + `Performing blowjob (2 weeks)` + 
                               `Lick anus (2 weeks)` + PrEP, 
                             data=data, family=binomial)
summary(multi_model)
# To convert log-odds to odds ratios
exp(coef(multi_model))
exp(confint(multi_model))

anova(multi_model, test = "Chisq")

