# Packages
library(ggplot2)
library(dplyr)
library(lmerTest)
library(lme4)
library(fitdistrplus)
library(tidyverse)
library(MuMIn)
library(glmmTMB)
library(DHARMa)
library (gam)
library (MASS)
library (car)

#Import data

CGB <- read.csv("data/Processed/CGB_col.csv")

CGB <- CGB[!is.na(CGB$"Climate"), ]
CGB$live <- CGB$Ndaphnia - CGB$X240m #creates a column with n of Daphnia alive in the end of trial


CGB$Site <- as.factor(CGB$Site) 
CGB$Climate <- as.factor(CGB$Climate) 
CGB$Sex <- as.factor(CGB$Sex) 

CGB_subset <- na.omit(CGB[, c("X240m", "live", "Climate", "Sex", "Site", "SS", "death.p", "Lum_Mean.hd", "Lum_StdDev.hd", "Sat_Mean.hd", "Sat_StdDev.hd", 
                              "Lum_Mean.fd", "Lum_StdDev.fd", "Sat_Mean.fd", "Sat_StdDev.fd", 
                              "Lum_Mean.hv", "Lum_StdDev.hv", "Sat_Mean.hv", "Sat_StdDev.hv", 
                              "Lum_Mean.fv", "Lum_StdDev.fv", "Sat_Mean.fv", "Sat_StdDev.fv")])


#Check for Colinearity

lm_model <- lm(death.p ~ Climate + Sex +
                 Lum_Mean.fd +  Sat_Mean.fd +  
                 Lum_Mean.fv +  Sat_Mean.fv, data = CGB_subset)
alias(lm_model) #removed variables that did not make too much sense, and site because of alias
vif(lm_model) #all values below 3

# Fit the model

#### Including site does not improve the model (to few variation per site)

m<- glm(cbind(X240m, live) ~ Climate + Sex + Lum_Mean.fd +  Sat_Mean.fd +  
            Lum_Mean.fv +  Sat_Mean.fv, 
                  data = CGB_subset, 
                  family = binomial)

summary(m)

m_simple<- glm(cbind(X240m, live) ~ Climate + Sex, 
               data = CGB_subset, 
               family = binomial)

summary(m_simple)

##### Check models
anova(m, m_simple)

#### Plot data

ggplot(CGB_subset, aes(x = Climate, y = death.p, fill=Sex)) +
  geom_boxplot(position = "dodge") +
  ylab("Death proportion")+
  theme_classic(base_size = 16)


#################

# Create a contingency table
contingency_table <- table(CGB_subset$Climate, CGB_subset$Sex)

# Print the contingency table
print(contingency_table)


####Use quasibinomial & Binomial controling for site, and without controling for site: Compare AIC and put this on paper. Get graph better. Discuss model has overdispersion. Check vif.