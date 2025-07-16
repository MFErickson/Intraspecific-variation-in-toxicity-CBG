#'Whats the use of being blue?
#'This script fits a glm on the data and plots it. 
#'Author: Marilia F Erickson
# Thu Feb 27 11:45:42 2025 ------------------------------

### Library
library(tidyverse)
library(car)
library(ggdist)

### 1. Import dataset ----

CGB <- read.csv("data/raw/CGB_col.csv") 

### 2. Check data set ----

any(duplicated(CGB))  # There is any duplicated rows?
any(is.na(CGB))

### 3. Clean & filter data ----

CGB$live <- CGB$Ndaphnia - CGB$X240m #creates a column with n of Daphnia alive in the end of trial

CGB$Site <- as.factor(CGB$Site) 
CGB$Climate <- as.factor(CGB$Climate) 
CGB$Sex <- as.factor(CGB$Sex) 


CGB_subset <- na.omit(CGB[, c("X240m", "live", "Climate", "Sex", "Site", "death.p", "Lum_Mean.hd", "Lum_StdDev.hd", "Sat_Mean.hd", "Sat_StdDev.hd",
                              "Lum_Mean.fd", "Lum_StdDev.fd", "Sat_Mean.fd", "Sat_StdDev.fd",
                              "Lum_Mean.hv", "Lum_StdDev.hv", "Sat_Mean.hv", "Sat_StdDev.hv",
                              "Lum_Mean.fv", "Lum_StdDev.fv", "Sat_Mean.fv", "Sat_StdDev.fv")]) #remove lines with NA for model variables


### 4. Check for Colinearity

lm_model <- lm(death.p ~ Climate + Sex +
                 Lum_Mean.fd +  Sat_Mean.fd +  
                 Lum_Mean.fv +  Sat_Mean.fv, data = CGB_subset)
alias(lm_model) #removed variables redundant variables and site because of alias
car::vif(lm_model) #all values below 3

### 4. Fit the model ----

m <- glm(cbind(X240m, live) ~ Climate + Sex + Lum_Mean.fd +  Sat_Mean.fd +  
            Lum_Mean.fv +  Sat_Mean.fv, 
                  data = CGB_subset, 
                  family = binomial)
summary(m) #Colour variables not significant and removed from final model

#Final model
m_simple<- glm(cbind(X240m, live) ~ Climate + Sex, 
               data = CGB_subset, 
               family = binomial)

summary(m_simple)

##### Check models
anova(m, m_simple) #compare models

### 5. Plot data ----


# Reorder Climate levels
CGB_subset$Climate <- factor(CGB_subset$Climate, levels = c("Tropical", "Subtropical", "Temperate"))

# Violin + boxplot with reordered x-axis
p1 <- ggplot(CGB_subset, aes(x = Climate, y = death.p, fill = Sex)) +
  geom_boxplot(aes(fill = Sex), width = 0.4, position = position_dodge(0.75), outlier.shape = NA, alpha = 0.5) +
  geom_point(aes(color = Sex), size = 2, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.3))+
  scale_fill_manual(values = c("#003459", "#007ea7")) +
  scale_color_manual(values = c("#003459", "#007ea7")) +
  ylab("Death proportion") +
  theme_classic(base_size = 16)
p1


#### Luminance

p2 <- ggplot(CGB_subset, aes(x = Lum_Mean.fd, y = death.p, color = Sex)) +
  geom_point(size = 2, alpha = 0.6) +  # Adds scatter points
  geom_smooth(method = "lm", se = TRUE, aes(fill = Sex), alpha = 0.3) +  # Adds regression line with confidence interval
  scale_color_manual(values = c("#003459", "#007ea7")) +  
  scale_fill_manual(values = c("#003459", "#007ea7")) +  
  labs(y ="Death proportion", x = "Mean Luminance of Dorsal Forewing") +
  theme_classic(base_size = 16)

p2


p3 <- ggplot(CGB_subset, aes(x = Lum_Mean.fv, y = death.p, color = Sex)) +
  geom_point(size = 2, alpha = 0.6) +  # Adds scatter points
  geom_smooth(method = "lm", se = TRUE, aes(fill = Sex), alpha = 0.3) +  # Adds regression line with confidence interval
  scale_color_manual(values = c("#003459", "#007ea7")) +  
  scale_fill_manual(values = c("#003459", "#007ea7")) +  
  labs(y ="Death proportion", x = "Mean Luminance of Ventral Forewing") +
  theme_classic(base_size = 16)
p3



### Saturation

p4 <- ggplot(CGB_subset, aes(x = Sat_Mean.fd, y = death.p, color = Sex)) +
  geom_point(size = 2, alpha = 0.6) +  # Adds scatter points
  geom_smooth(method = "lm", se = TRUE, aes(fill = Sex), alpha = 0.3) +  # Adds regression line with confidence interval
  scale_color_manual(values = c("#003459", "#007ea7")) +  
  scale_fill_manual(values = c("#003459", "#007ea7")) +  
  labs(y ="Death proportion", x = "Mean Saturation of Dorsal Forewing") +
  theme_classic(base_size = 16)

p4

p5 <- ggplot(CGB_subset, aes(x = Sat_Mean.fv, y = death.p, color = Sex)) +
  geom_point(size = 2, alpha = 0.6) +  # Adds scatter points
  geom_smooth(method = "lm", se = TRUE, aes(fill = Sex), alpha = 0.3) +  # Adds regression line with confidence interval
  scale_color_manual(values = c("#003459", "#007ea7")) +  
  scale_fill_manual(values = c("#003459", "#007ea7")) +  
  labs(y ="Death proportion", x = "Mean Saturation of Ventral Forewing") +
  theme_classic(base_size = 16)

p5


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

ggsave(plot = p3, 
       filename = "output/figures/plot3.png",
       width = 6.5, 
       height = 4, 
       dpi = 300)

ggsave(plot = p4, 
       filename = "output/figures/plot4.png",
       width = 6.5, 
       height = 4, 
       dpi = 300)

ggsave(plot = p5, 
       filename = "output/figures/plot5.png",
       width = 6.5, 
       height = 4, 
       dpi = 300)



