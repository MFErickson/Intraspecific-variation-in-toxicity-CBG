# Packages
library(tidyverse)
library(lmerTest)
library(lme4)
library(fitdistrplus)
library(MuMIn)
library(glmmTMB)
library(DHARMa)
library(gam)
library(MASS)
library(car)
library(inspecdf)
library(ggpubr)
library(ggdist)

#Import data

CGB <- read.csv("data/Processed/CGB_col.csv") %>% 
  select(-Collector,
         -Notes,
         -Date)

any(duplicated(CGB))  # There is any duplicated rows?
any(is.na(CGB))
na_check <- inspectdf::inspect_na(CGB) # Percentage of NA in each columns
na_check

#CGB <- CGB[!is.na(CGB$"Climate"), ]
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
car::vif(lm_model) #all values below 3

# Fit the model

#### Including site does not improve the model (to few variation per site)

m <- glm(cbind(X240m, live) ~ Climate + Sex + Lum_Mean.fd +  Sat_Mean.fd +  
            Lum_Mean.fv +  Sat_Mean.fv, 
                  data = CGB_subset, 
                  family = binomial)

summary(m)

#Final model
m_simple<- glm(cbind(X240m, live) ~ Climate + Sex, 
               data = CGB_subset, 
               family = binomial)

summary(m_simple)

##### Check models
anova(m, m_simple)

#### Plot data
p1 <- ggplot(CGB_subset, aes(x = Climate, y = death.p, fill = Sex)) +
  geom_boxplot(alpha = 0.6) +
  geom_point(aes(y = death.p, color = Sex), position = position_jitterdodge(), alpha = 0.7) +
  ylab("Death proportion")+
  scale_fill_manual(values=c("#003459","#007ea7"))+
  scale_color_manual(values=c("#003459","#007ea7"))+
  theme_classic(base_size = 16)
p1

p2 <- ggplot(CGB_subset, aes(x = Climate, y = death.p, fill = Sex)) +
  geom_violin(color = "white",alpha = 0.3, width = 1, position = position_dodge(0.75)) +
  geom_boxplot(aes(fill = Sex), width = 0.2, position = position_dodge(0.75), outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values=c("#003459","#007ea7"))+
  ylab("Death proportion") +
  theme_classic(base_size = 16)
p2

#################

# Create a contingency table
contingency_table <- table(CGB_subset$Climate, CGB_subset$Sex)

# Print the contingency table
print(contingency_table)


####Use quasibinomial & Binomial controling for site, and without controling for site: Compare AIC and put this on paper. Get graph better. Discuss model has overdispersion. Check vif.

#Save plots ----
ggsave(plot = p1, 
       filename = "output/figures/plot1.png",
       width = 6.5, 
       height = 4, 
       dpi = 300)

ggsave(plot = p2, 
       filename = "output/figures/plot2.png",
       width = 6.5, 
       height = 4, 
       dpi = 300)
