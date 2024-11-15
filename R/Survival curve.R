# Instale o pacote survival, caso ainda não tenha instalado
#install.packages("survival")
#install.packages("survminer")

# Carregue o pacote survival e o conjunto de dados lung
library(survival)
library(survminer)
library(readxl)

#import data ----

kmdata <- read.csv("data/processed/kmdataCGB.csv")

#####################################Site##############################################
#fit model ----
kmcurve <- survfit(Surv(time, death) ~ Site, data = kmdata)
summary(kmcurve, times=seq(0, 240, 60))

#Plot survivel curve b3 ----
png(file = "output/figures/survival_curve_site.png",
    res = 300,
    units = "in",
    width = 5,
    height = 4)

ggsurvplot(kmcurve,
                xlim = c(0, 240),  break.x.by = 60,
                linetype = 1,
                ylab = "Survival probability", xlab = "Time (min)",
                pval = TRUE,
                risk.table = FALSE,
                cumevents = FALSE,
                cumcensor = FALSE,
                risk.table.title = "",
                legend.labs = c("AO", "BBG", "BG", "CC", "JB", "JCU", "LSP", "MR", "OC", "PP", "WLR"),
                legend.title = "",  
                surv.scale = "percent",
                palette = c("#3182bd", "#31a354", "#fd8d3c", "#006d2c", "#08519c" , "#a63603", "#bae4b3", "#e6550d", "#74c476", "#6baed6", "#bdd7e7"),
                title="",  risk.table.height=.25)


dev.off()
#Compare results

surv_diff <- survdiff(Surv(time = kmdata$time, event = kmdata$death) ~ kmdata$Site)
print(surv_diff)  ###See if there is difference between curves

cox_model <- coxph(Surv(time, death) ~ Site, data = kmdata)
summary(cox_model)  ###See if there is difference between curves


pairwise_diff <- pairwise_survdiff(Surv(time, death) ~Site, data = kmdata)
print(pairwise_diff)  ###pairwise comparisson between curves




#####################################Climate##############################################
#fit model ----
kmcurve <- survfit(Surv(time, death) ~ Climate, data = kmdata)
summary(kmcurve, times=seq(0, 240, 60))

#Plot survivel curve b3 ----
png(file = "output/figures/survival_curve_Climate.png",
    res = 300,
    units = "in",
    width = 5,
    height = 4)

ggsurvplot(kmcurve,
           xlim = c(0, 240),  break.x.by = 60,
           linetype = 1,
           ylab = "Survival probability", xlab = "Time (min)",
           pval = TRUE,
           risk.table = FALSE,
           cumevents = FALSE,
           cumcensor = FALSE,
           risk.table.title = "",
           legend.labs = c("Subtropical", "Temperate", "Tropical"),
           legend.title = "",  
           surv.scale = "percent",
           palette = c( "#a1d99b","#3182bd" ,"#fd8d3c" ),
           title="",  risk.table.height=.25)


dev.off()
#Compare results

surv_diff <- survdiff(Surv(time = kmdata$time, event = kmdata$death) ~ kmdata$Climate)
print(surv_diff)  ###See if there is difference between curves

cox_model <- coxph(Surv(time, death) ~ Climate, data = kmdata)
summary(cox_model)  ###See if there is difference between curves


pairwise_diff <- pairwise_survdiff(Surv(time, death) ~Climate, data = kmdata)
print(pairwise_diff)  ###pairwise comparisson between curves


####################Sex


#fit model ----
kmcurve <- survfit(Surv(time, death) ~ Sex, data = kmdata)
summary(kmcurve, times=seq(0, 240, 60))

#Plot survivel curve b3 ----
png(file = "output/figures/survival_curve_Sex.png",
    res = 300,
    units = "in",
    width = 5,
    height = 4)

ggsurvplot(kmcurve,
           xlim = c(0, 240),  break.x.by = 60,
           linetype = 1,
           ylab = "Survival probability", xlab = "Time (min)",
           pval = TRUE,
           risk.table = FALSE,
           cumevents = FALSE,
           cumcensor = FALSE,
           risk.table.title = "",
           legend.labs = c("Female", "Male"),
           legend.title = "",  
           surv.scale = "percent",
           palette = c( "#addd8e", "#fee391" ),
           title="",  risk.table.height=.25)


dev.off()
#Compare results

surv_diff <- survdiff(Surv(time = kmdata$time, event = kmdata$death) ~ kmdata$Sex)
print(surv_diff)  ###See if there is difference between curves

cox_model <- coxph(Surv(time, death) ~ Sex, data = kmdata)
summary(cox_model)  ###See if there is difference between curves


pairwise_diff <- pairwise_survdiff(Surv(time, death) ~Sex, data = kmdata)
print(pairwise_diff)  ###pairwise comparisson between curves
