## ===========================
## Figure 4. Survival Analysis.
## ===========================

## ===========================
## Packages
## ===========================
library(survival)
library(survminer)
library(gridExtra)

## ===========================
## Create left panel
## ===========================

## Year in which faculty turns 70 provided they are present at 50
faculty$year.70 <- ifelse(faculty$last.year >= (faculty$byear + 50), faculty$byear + 70, NA)

## Define cohorts: faculty turning 70 within w years of 1994
faculty$cohort <- ifelse(faculty$year.70 < 1994, 'A', 'B')
w <- 3
faculty.win <- faculty[faculty$year.70 >= (1994 - w) & faculty$year.70 <= (1994 + w),]

## Exclude 1994 
faculty.win <- faculty.win[faculty.win$year.70 != 1994]

## Time to event measured as age
faculty.win$timeV1 <- faculty.win$last.year - faculty.win$byear

## Event is leaving the school (for any reason) after the age of 50 for tenured faculty
faculty.win$eventV1 <- ifelse(faculty.win$last.year < 2017, 1, 0)

## Keep only relevant columns
survivalV1 <- faculty.win[, c('facID', 'timeV1', 'eventV1', 'cohort', 'last.year', 'first.year.tenured', 'byear', 'year.70')]

## Include only faculty members who were present past 50 and were tenured
survivalV1 <- survivalV1[!is.na(survivalV1$first.year.tenured) & !is.na(survivalV1$year.70),]

## KM plots
survival.V1 <- survfit(Surv(timeV1, eventV1) ~ cohort, data = survivalV1)

p.value <- round(signif(surv_pvalue(survival.V1)$pval, 3),4)
options(scipen = 999)
p.value <- as.character(p.value)

ggsurv1 <- ggsurvplot(survival.V1, conf.int = F, pval = paste0("p = ", p.value), pval.coord = c(80, 1),
                      palette = c('red', 'blue'), size=.5, legend = 'none', title = 'Survival Curve', xlab = 'Age', 
                      axes.offset = FALSE, ylim = c(0, 1.0625), xlim = c(50, 95))

ggsurv1$plot <- ggsurv1$plot + 
  ggplot2::annotate("text", x = c(85, 61), y = c(0.25, 0.3),
                    label = c("3 Year Cohort Not \n Subject to Cap", '3 Year Cohort \nSubject To Cap'), size = 4, colour = c('blue', 'red')) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15) , panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(50, 90, by = 10), expand = c(0,0))

ggsurv1$plot$layers <- c(geom_vline(xintercept = 70, color = 'darkgrey'), ggsurv1$plot$layers)

leftPanel <- ggsurv1

## ===========================
## Create right panel
## ===========================

## For this plot we use a window between 1985 - 2012
faculty.win <- faculty[faculty$year.70 >= (1994 - 9) & faculty$year.70 <= (1994 + 21),]

## Time to event measured as age
faculty.win$timeV1 <- faculty.win$last.year - faculty.win$byear

## Event is leaving the school (for any reason) after the age of 50 for tenured faculty
faculty.win$eventV1 <- ifelse(faculty.win$last.year < 2017, 1, 0)

## Keep only relevant columns 
survivalV1 <- faculty.win[, c('facID', 'timeV1', 'eventV1', 'cohort', 'last.year', 'first.year.tenured', 'byear', 'year.70')]

## Include only faculty members who were present past 50 and were tenured
survivalV1 <- survivalV1[!is.na(survivalV1$first.year.tenured) & !is.na(survivalV1$year.70),]

## For each sliding window of each placebo year, store the pvalue and the log rank test statistic in a table
years <- seq(1985,2010, by = 1)

p <- list()
LogRankTstat <- list()
comparison <- list()
g <- list()
n <- seq(1, length(years), by = 1)

for(i in n){
  y <- years[i]
  sub <- subset(survivalV1, year.70 >= y & year.70 <= y+6 & year.70 != y+3)
  
  sub$cohort <- ifelse(sub$year.70 <= y + 2, "A", "B")
  table(sub$year.70[sub$cohort == "A"])
  
  #Censoring in cohort 
  censoring <- subset(sub, cohort == "B" & eventV1 ==0)
  if(dim(censoring)[1] > 0){
    minCensoredAge <- min(sub$timeV1[sub$cohort == "B" & sub$eventV1 ==0])
    sub$eventV1[sub$timeV1 >= minCensoredAge] <- 0
  }
  
  survival.V1 <- survfit(Surv(timeV1, eventV1) ~ cohort, data =sub)

  log.rank <- survdiff(Surv(timeV1, eventV1) ~ cohort, data =sub)
  t.stat <- log.rank$chisq
  p.value <-signif(surv_pvalue(survival.V1, data = sub)$pval, 4)

  label <- paste0(y, "-", y+2, " v. ", y+4, "-", y+6)
  p[[i]] <- p.value
  comparison[[i]] <- label
  LogRankTstat[[i]] <- t.stat
  
  p.value <- round(as.numeric(p.value), 4)
  # graph this
  ggsurv1 <- ggsurvplot(survival.V1, conf.int = F, pval = paste0("p = ", p.value), pval.coord = c(80, 1),
                        palette = c(rgb(1,0,0,.4), rgb(0,0,1,.4)), 
                        size=.5, 
                        legend = 'none',
                        main = "",
                        title = paste0(label), xlab = 'Age', 
                        axes.offset = FALSE, 
                        ylim = c(0, 1.0625), xlim = c(50, 95), 
                        data = survivalV1)
  ggsurv1$plot <- ggsurv1$plot + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15) , panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    scale_x_continuous(breaks = seq(50, 90, by = 10), expand = c(0,0))

  ggsurv1$plot$layers <- c(geom_vline(xintercept = 70, color = 'darkgrey'), ggsurv1$plot$layers)
  g[[i]] <- ggsurv1$plot
  
}
do.call(grid.arrange, c(g, ncol = 4))

## Plot distribution 
df <- as.data.frame(cbind(comparison,p, LogRankTstat))
df$p <- round(as.numeric(df$p), 4)
df$LogRankTstat <- round(as.numeric(df$LogRankTstat), 4)

# One sided test
observed <- df$LogRankTstat[df$comparison == "1991-1993 v. 1995-1997"]
nullDist <- df$LogRankTstat
toReturn <- list(placeboDist=nullDist,observed=observed,p.value=mean(nullDist >= observed))
oneSide <- round(toReturn$p.value, 4)

## Plot
hist <- ggplot(df, aes(x=LogRankTstat)) + geom_histogram(binwidth = .5, color="white", fill = "darkgrey") + ylim(0,7) + 
  labs(x = "Test statistic for survival curve difference (log rank)", y= "Frequency") + ggtitle("Permutation Test") + 
  annotate("text", x = 9.3, y=2.75, label =  paste0("Observed\nuncapping"), color = 'black') +
  annotate("text", x = .6, y=6.7, label =  paste0("Placebo\nuncapping"), color = 'black')+
  theme(plot.title = element_text(size=15, hjust=0.5, face='bold'))+ theme_classic()


## ===========================
## Combine panels
## ===========================
grid.arrange(leftPanel$plot, hist,ncol=2)

