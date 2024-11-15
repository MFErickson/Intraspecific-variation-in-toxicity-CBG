# Packages
library(ggplot2)
library(dplyr)
library(lmerTest)

#### Import data
dt <- read.csv("data/Processed/CGB.csv")


### LLM

# Load necessary package
library(lme4)

# Fit the model
model <- glm(death.p ~ Climate + Sex, data = dt)

# Summary of the model
summary(model)

