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
#### Import data
CGB <- read.csv("data/Processed/CGB.csv")

CGB <- CGB[!is.na(CGB$"Climate"), ]
#### Exploring response variable ####
plot(density(CGB$death.p))
hist(CGB$death.p)
f1 <- fitdist(CGB$death.p, "exp")
summary(f1)
plot(f1, demp = TRUE)
descdist(CGB$death.p, discrete=FALSE)

# Fit the model
CGB2 <- CGB %>% 
  mutate(death.p2 = death.p+0.001) 

CGB3 <- CGB2 %>% 
  mutate(death.p3 = death.p2/(max(death.p2) + 0.001))

model <- glmmTMB(death.p3 ~ Climate + (1/Site), data = CGB3, family = beta_family)
# Summary of the model
summary(model)

testDispersion(model)

simulationOutput <- simulateResiduals(fittedModel = model, plot = T)


################

ggplot(CGB3, aes(x= Climate, y= death.p3, color = Sex))+
  geom_boxplot()



ggplot(CGB3, aes(x= weight, y= death.p3, color = Sex))+
  geom_point()


