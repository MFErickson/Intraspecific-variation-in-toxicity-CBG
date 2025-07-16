#'Whats the use of being blue?
#'This script fits a glm on the control data versus butterfly and plots it. 
#'Author: Marilia F Erickson


### Library
library(tidyverse)
library(car)
library(ggdist)

### 1. Import dataset ----
CGB <- read.csv("data/raw/CGB_col.csv") 
con <- read.csv("data/raw/controls.csv") 

### 2. Clean & filter data ----

CGB_trimmed <- CGB[, names(con)] #Kept columns in con
dat <- rbind(CGB_trimmed, con) #merge

dat$live <- dat$Ndaphnia - dat$X240m #creates a column with n of Daphnia alive in the end of trial

### 3. Fit the model ----
m <- glm(cbind(X240m, live) ~ Binomial, 
         data = dat, 
         family = binomial)
summary(m) 

### 4. Plot data ----

# Reorder Control levels
dat$Binomial <- factor(dat$Binomial, levels = c("Zizina otis", "Pieris rapae", "Methanol", "H2O"))

# Violin + boxplot with reordered x-axis
p6 <- ggplot(dat, aes(x = Binomial, y = death.p)) +
  geom_boxplot(aes(fill = Binomial), width = 0.4, position = position_dodge(0.75), outlier.shape = NA, alpha = 0.5) +
  geom_point(aes(color = Binomial), size = 2, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.3)) +
  scale_fill_manual(values = c("#003459", "#007ea7", "#9ecae1", "#deebf7")) +
  scale_color_manual(values = c("#003459", "#007ea7", "#9ecae1", "#deebf7")) +
  scale_x_discrete(labels = c(
    "Zizina otis" = expression(italic("Zizina otis")),
    "Pieris rapae" = expression(italic("Pieris rapae")),
    "Methanol" = "Methanol",
    "H2O" = "H2O"
  )) +
  ylab("Death proportion") +
  xlab("Treatment extracts") +
  theme_classic(base_size = 16)

p6

#Save plots ----
ggsave(plot = p6, 
       filename = "output/figures/plot6.png",
       width = 6.5, 
       height = 4, 
       dpi = 300)

