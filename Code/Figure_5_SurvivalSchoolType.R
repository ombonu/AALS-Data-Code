## ===========================
## Figure 5. Survival Curves by School Type
## ===========================

library(survival)
library(survminer)

####################
## Define Cohorts
####################

### year in which faculty turns 70 provided they are present at 50
faculty$year.70 <- ifelse(faculty$last.year >= (faculty$byear + 50), faculty$byear + 70, NA)

## define cohorts
faculty$cohort <- ifelse(faculty$year.70 < 1994, 'A', 'B')

####################
## Public/Private
####################
## analysis is at the faculty level
## a faculty member is 'private' if the last school in which they appeared before leaving the directory is a private school
## This is in variable name 'private'

####################
## Survival Analysis
####################

## For this version we use a 3 year window of observation
w <- 3
## look only at faculty turning 70 within w years of 1994
faculty.win <- faculty[faculty$year.70 >= (1994 - w) & faculty$year.70 <= (1994 + w),]
# exclude 1994 
faculty.win <- faculty.win[faculty.win$year.70 != 1994]

## Model age of departure for tenured faculty (no tenure track faculty) that are active at 50 y/o

## time to event ----
## for both cohorts time is captured by the age in which they depart
faculty.win$time <- faculty.win$last.year - faculty.win$byear

## event-------
## In this specification there is censoring only if your last year is 2017
faculty.win$event <- ifelse(faculty.win$last.year < 2017, 1, 0)

## keep only relevant columns-------
survival <- faculty.win[, c('facID', 'time', 'event', 'cohort', 'last.year', 'first.year.tenured', 'byear', 'year.70', 'private')]

## we care only about faculty members who were present past 50 and were tenured
survival <- survival[!is.na(survival$first.year.tenured) & !is.na(survival$year.70),]

## cohort A survival rates
cohortA <- survival[survival$cohort == 'A']
survival.A <- survfit(Surv(time, event) ~ private, data = cohortA)

#cohort B survival rates
cohortB <- survival[survival$cohort == 'B']
survival.B <- survfit(Surv(time, event) ~ private, data = cohortB)

## ===========================
## Create Figure
## ===========================
## cohort A-----
p.value <- signif(surv_pvalue(survival.A)$pval, 4)
p.value <- round(p.value, 4)
options(scipen = 999)
p.value <- as.character(p.value)

ggsurvA <- ggsurvplot(survival.A, conf.int = F, pval = paste0("p = ", p.value), pval.coord = c(80, 1), 
                      palette = c('darkgreen', 'purple'), size=.5, legend = 'none', title = '3 Year Cohort Subject to Cap', xlab = 'Age', 
                      axes.offset = FALSE, ylim = c(0, 1.0625), xlim = c(50, 95))


ggsurvA$plot <- ggsurvA$plot + 
  ggplot2::annotate("text", 
                    x = c(62, 75), y = c(0.40, 0.45), 
                    label = c("Public", 'Private'), size = 5, colour = c('darkgreen', 'purple')) + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14) , panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(50, 100, by = 10), expand = c(0,0)) #+ geom_vline(xintercept = 70, color = 'darkgrey')

ggsurvA$plot$layers <- c(geom_vline(xintercept = 70, color = 'darkgrey'), ggsurvA$plot$layers)

## cohort B -----
p.value <- signif(surv_pvalue(survival.B)$pval, 3)
p.value <- round(p.value, 4)
options(scipen = 999)
p.value <- as.character(p.value)

ggsurvB <- ggsurvplot(survival.B, conf.int = F, pval = paste0("p = ", p.value), pval.coord = c(80, 1),
                      palette = c('darkgreen', 'purple'), size=.5, legend = 'none', title = '3 Year Cohort Not Subject to Cap', xlab = 'Age', 
                      axes.offset = FALSE, ylim = c(0, 1.0625), xlim = c(50, 95))

ggsurvB$plot <-  ggsurvB$plot + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14) , panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(50, 90, by = 10), expand = c(0,0))

ggsurvB$plot$layers <- c(geom_vline(xintercept = 70, color = 'darkgrey'), ggsurvB$plot$layers)

## combine plots
plot <- arrange_ggsurvplots(list(ggsurvA, ggsurvB), print = FALSE,
                            ncol = 2, nrow = 1)

