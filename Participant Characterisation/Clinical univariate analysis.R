library(dplyr)
library(tidyverse)
library(readxl)

# Read raw data
ChiSqrdata <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/Clinical statistical analysis.xlsx", 
                         sheet = "ChiSqr")

# Check the class (e.g. numerical/categorical) of the variables  - correct
glimpse(ChiSqrdata)

# variables' name
names(ChiSqrdata)

# Univariate logistic regression for HIV
uni_model_HIV <- glm(Carrier ~ HIV, data=ChiSqrdata, family=binomial)
summary(uni_model_HIV)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_HIV), confint(uni_model_HIV)))
# Overall p-value using chi-square test
anova(uni_model_HIV, test = "Chisq")

# Univariate logistic regression for Throat gono NAAT
uni_model_throatgono <- glm(Carrier ~ `Throat gono NAAT`, data=ChiSqrdata, family=binomial)
summary(uni_model_throatgono)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_throatgono), confint(uni_model_throatgono)))
# Overall p-value using chi-square test
anova(uni_model_throatgono, test = "Chisq")

# Univariate logistic regression for Rectal gono NAAT
uni_model_rectalgono <- glm(Carrier ~ `Rectal gono NAAT`, data=ChiSqrdata, family=binomial)
summary(uni_model_rectalgono)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_rectalgono), confint(uni_model_rectalgono)))
# Overall p-value using chi-square test
anova(uni_model_rectalgono, test = "Chisq")

# Univariate logistic regression for Urethral gono NAAT
uni_model_urethralgono <- glm(Carrier ~ `Urethral gono NAAT`, data=ChiSqrdata, family=binomial)
summary(uni_model_urethralgono)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_urethralgono), confint(uni_model_urethralgono)))
# Overall p-value using chi-square test
anova(uni_model_urethralgono, test = "Chisq")


--------------------------------------------------------
# Fishers exact test  
  
  # Read raw data
Fishersdata <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/Clinical statistical analysis.xlsx", 
                            sheet = "Fishers")


# Install if not already
install.packages("epitools")
library(epitools)

# Throat chla NAAT
throatchla <- table(Fishersdata$`Throat chla NAAT`, Fishersdata$Carrier)
oddsratio(throatchla, method = "fisher")
fisher.test(throatchla)

# Rectal chla NAAT
rectalchla <- table(Fishersdata$`Rectal chla NAAT`, Fishersdata$Carrier)
oddsratio(rectalchla, method = "fisher")
fisher.test(rectalchla)

# Urethral chla NAAT
urehtralchla <- table(Fishersdata$`Urethral chla NAAT`, Fishersdata$Carrier)
oddsratio(urehtralchla, method = "fisher")
fisher.test(urehtralchla)

# Cold/sore throat
coldsorethroat <- table(Fishersdata$`Cold/sore throat`, Fishersdata$Carrier)
oddsratio(coldsorethroat, method = "fisher")
fisher.test(coldsorethroat)

# Syphilis
Syphilis <- table(Fishersdata$`Syphilis`, Fishersdata$Carrier)
oddsratio(Syphilis, method = "fisher")
fisher.test(Syphilis)