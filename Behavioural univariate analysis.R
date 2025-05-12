library(dplyr)
library(tidyverse)
library(readxl)

# Read raw data
ChiSqrdata <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/Behavioural statistical analysis.xlsx", 
                   sheet = "ChiSqr")

# Check the class (e.g. numerical/categorical) of the variables  - correct
glimpse(ChiSqrdata)

# variables' name
names(ChiSqrdata)

# Univariate logistic regression for Waterpipe
uni_model_Waterpipe <- glm(Carrier ~ Waterpipe, data=ChiSqrdata, family=binomial)
summary(uni_model_Waterpipe)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_Waterpipe), confint(uni_model_Waterpipe)))
# Overall p-value using chi-square test
anova(uni_model_Waterpipe, test = "Chisq")

# Univariate logistic regression for Condomless anal sex with males (3 months)
ChiSqrdata$`Condomless anal sex with males (3 months)` <- factor(ChiSqrdata$`Condomless anal sex with males (3 months)`, levels=c("0", "1-2", "3-5", ">6"))
table(ChiSqrdata$`Condomless anal sex with males (3 months)`, useNA = 'always')
uni_model_condomless <- glm(Carrier ~ `Condomless anal sex with males (3 months)`, data=ChiSqrdata, family=binomial)
summary(uni_model_condomless)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_condomless), confint(uni_model_condomless)))
# Overall p-value using chi-square test
anova(uni_model_condomless, test = "Chisq")

# Univariate logistic regression for Mutual masturbation (2 weeks)
uni_model_masturbation <- glm(Carrier ~ `Mutual masturbation (2 weeks)`, data=ChiSqrdata, family=binomial)
summary(uni_model_masturbation)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_masturbation), confint(uni_model_masturbation)))
# Overall p-value using chi-square test
anova(uni_model_masturbation, test = "Chisq")

# Univariate logistic regression for Performing blowjob (2 weeks)
uni_model_blowjob <- glm(Carrier ~ `Performing blowjob (2 weeks)`, data=ChiSqrdata, family=binomial)
summary(uni_model_blowjob)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_blowjob), confint(uni_model_blowjob)))
# Overall p-value using chi-square test
anova(uni_model_blowjob, test = "Chisq")

# Univariate logistic regression for Lick anus (2 weeks)
uni_model_anus <- glm(Carrier ~ `Lick anus (2 weeks)`, data=ChiSqrdata, family=binomial)
summary(uni_model_anus)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_anus), confint(uni_model_anus)))
# Overall p-value using chi-square test
anova(uni_model_anus, test = "Chisq")

# Univariate logistic regression for PrEP
uni_model_PrEP <- glm(Carrier ~ `PrEP`, data=ChiSqrdata, family=binomial)
summary(uni_model_PrEP)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_PrEP), confint(uni_model_PrEP)))
# Overall p-value using chi-square test
anova(uni_model_PrEP, test = "Chisq")

# Univariate logistic regression for Smoker
ChiSqrdata$`Smoker` <- factor(ChiSqrdata$`Smoker`, levels=c("No", "1-10", "11-20"))
table(ChiSqrdata$`Smoker`, useNA = 'always')
uni_model_Smoker <- glm(Carrier ~ `Smoker`, data=ChiSqrdata, family=binomial)
summary(uni_model_Smoker)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_Smoker), confint(uni_model_Smoker)))
# Overall p-value using chi-square test
anova(uni_model_Smoker, test = "Chisq")

# Univariate logistic regression for E-cigarettes/vape
uni_model_cig <- glm(Carrier ~ `E-cigarettes/vape`, data=ChiSqrdata, family=binomial)
summary(uni_model_cig)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_cig), confint(uni_model_cig)))
# Overall p-value using chi-square test
anova(uni_model_cig, test = "Chisq")

# Univariate logistic regression for Regular Partner
ChiSqrdata$`Regular Partner` <- factor(ChiSqrdata$`Regular Partner`, levels=c("Yes", "No"))
table(ChiSqrdata$`Regular Partner`, useNA = 'always')
uni_model_reg <- glm(Carrier ~ `Regular Partner`, data=ChiSqrdata, family=binomial)
summary(uni_model_reg)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_reg), confint(uni_model_reg)))
# Overall p-value using chi-square test
anova(uni_model_reg, test = "Chisq")

# Univariate logistic regression for Males had sex with (3 months)
ChiSqrdata$`Males had sex with (3 months)` <- factor(ChiSqrdata$`Males had sex with (3 months)`, levels=c("0", "1-2", "3-5", ">6"))
table(ChiSqrdata$`Males had sex with (3 months)`, useNA = 'always')
uni_model_sex <- glm(Carrier ~ `Males had sex with (3 months)`, data=ChiSqrdata, family=binomial)
summary(uni_model_sex)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_sex), confint(uni_model_sex)))
# Overall p-value using chi-square test
anova(uni_model_sex, test = "Chisq")

# Univariate logistic regression for Number of people kissed (2 weeks)
ChiSqrdata$`Number of people kissed (2 weeks)` <- factor(ChiSqrdata$`Number of people kissed (2 weeks)`, levels=c("0", "1-2", "3-5", ">6"))
table(ChiSqrdata$`Number of people kissed (2 weeks)`, useNA = 'always')
uni_model_kiss <- glm(Carrier ~ `Number of people kissed (2 weeks)`, data=ChiSqrdata, family=binomial)
summary(uni_model_kiss)
# To convert log-odds to odds ratios
exp(cbind(OR = coef(uni_model_kiss), confint(uni_model_kiss)))
# Overall p-value using chi-square test
anova(uni_model_kiss, test = "Chisq")

--------------------------------------------------------
# Fishers exact test  

  
  # Read raw data
Fishersdata <- read_excel("C:/Users/memon/OneDrive - Nexus365/Year 4/Behavioural statistical analysis.xlsx", 
                           sheet = "Fishers")

# Install if not already
install.packages("epitools")
library(epitools)

# Fisting someone
fistingsomeone <- table(Fishersdata$`Fisting someone (last 2 weeks)`, Fishersdata$Carrier)
oddsratio(fistingsomeone, method = "fisher")
fisher.test(fistingsomeone)

# Getting fisted
gettingfisted <- table(Fishersdata$`Getting fisted (2 weeks)`, Fishersdata$Carrier)
oddsratio(gettingfisted, method = "fisher")
fisher.test(gettingfisted)

# Anal sex toys
sextoy <- table(Fishersdata$`Anal sex toys (last 2 weeks)`, Fishersdata$Carrier)
oddsratio(sextoy, method = "fisher")
fisher.test(sextoy)


