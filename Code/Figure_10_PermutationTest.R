## ===========================
## Figure 10. Permutation Test
## ===========================


# Calculate the number of faculty over the age of 70 in the previous year
schoolYear[, over70.lag:= c(NA, over70[-.N]), by = schoolID]

# Pull variables needed
dta <- schoolYear[,c("schoolID","nFaculty",  "year", "over70", "nFemale", "nMinority",
                     "nMinorityFemale", "nJunior")]
dta <- subset(dta, year > 1994)

# School IDs
ids <- unique(dta$schoolID)

# Function to calculate t-stat for each permutation
get.teststat <- function(dta, var){
  #Reshuffle
  shuffle <- sample(ids, length(ids), replace = F)
  #Create empty dataframe for shuffled dataset
  new.dta <- data.frame(year =integer(), schoolID = character(), nFaculty = integer(),
                        over70 = numeric(), nFemale = numeric(), nMinority = numeric(),
                        nMinorityFemale = numeric(), nJunior = numeric())
  
  #Reshuffle treatment (prop. over 70)
  for(i in 1:length(ids)){
    this.id <- ids[i]
    # Get the school ID's outcome vector and FEs
    this.outcome <- dta[schoolID == this.id, -c("over70")]
    # Get the resampled treatment
    this.age <- dta[schoolID == shuffle[i], c("over70", "year")]
    # Merge reshuffled over70 prop. with school data
    new <- merge(this.age, this.outcome, by= "year")
    new.dta <- rbind(new.dta, new)
  }
  #Use lagged independent variable to bypass mechanistic issues
  new.dta[, over70.lag:= c(NA, over70[-.N]), by = schoolID]
  #Run regression
  reg.formula <- paste0(var, '~ over70.lag + as.factor(schoolID) + as.factor(year)')
  reg <-  glm(reg.formula, data = new.dta,
              family = quasipoisson, offset = log(nFaculty))
  
  return(coefficients(reg)[2])
}

## Function to run permutation test an calculate p-value
test.adj <- function(dta, var, side='less'){
  # Placebo distribution
  nullDist <- replicate(1000,get.teststat(dta, var),simplify='array')
  # Get observed estimate
  reg.formula <- paste0(var, '~ over70.lag + as.factor(schoolID) + as.factor(year)')
  observed.reg <-  glm(reg.formula, data = schoolYear,
                       family = quasipoisson, offset = log(nFaculty))
  observed <- coefficients(observed.reg)[2]
  
  # Calculate one-sided p-value
  if (side=='less'){
    toReturn <- list(simulatedDist=nullDist,observed=observed, p.value=mean(nullDist <= observed))
  } else {
    toReturn <- list(simulatedDist=nullDist,observed=observed, p.value=mean(nullDist >= observed))
  }
  toReturn
}

## Female results
set.seed(262)
femaleResults <- test.adj(dta, "nFemale")

## Minority
set.seed(973)
minorityResults <- test.adj(dta, "nMinority")

## Minority female
set.seed(422)
minorityFemaleResults <- test.adj(dta, "nMinorityFemale")

## Junior
set.seed(140)
juniorResults <- test.adj(dta, "nJunior")


## ===========================
## Create Figure
## ===========================

## Data to plot
dta.lists <- c('juniorResults', 'femaleResults', 'minorityResults', 'minorityFemaleResults')
## Graph titles
titles <- c('Junior faculty','Female faculty', 'Minority faculty', 'Minority female faculty')


par(mfrow = c(2,2), mar=c(3,3,2,1),mgp=c(1.5,0.5,0), tcl=-0.3, cex.main = .9)

for(i in 1:length(dta.lists)){
  hist(get(paste0(dta.lists[1]))$simulatedDist,xlab='Coefficient on Prop. Over 70',
       main=titles[i],
       col="dark grey", border=rgb(192,192,192, max=255), cex.axis=1,cex.lab = 1,ylim=c(0,3),
       breaks=seq(-1.8,1.8,by=.1), freq = FALSE,  ylab = c("Density"))
  abline(v=get(paste0(dta.lists[1]))$observed,col=rgb(0,0,0,.8))
  text(-1.18,.58,paste0("p=", get(paste0(dta.lists[1]))$p.value),cex=.8, col = rgb(0,0,0,.6))
  text(-1.23,.79,paste0("Observed"),cex=.8, col = rgb(0,0,0,.6))
  
}