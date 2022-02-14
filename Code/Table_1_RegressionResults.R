#################################################
#Table 1: Regression Results
#################################################


##############
# Functions
##############
# Calculate robust ses
robust.se <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  ## dfc formula difference from linear models?
  dfc <- (M/(M - 1))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(rcse.se)
}

# Add stars for significance
createRegressionTable <- function(beta, se, p, n){
  #Regression table
  my.table <- rbind(
    paste0(formatC(beta,format = "f", digits = 2),
           if(p<0.01){
             "***"
           }else if(p<0.05){
             "**"}else if(p<0.1){
               "*"}else{""}),
    paste0("(",formatC(se,format = "f", digits = 2),")"),
    paste(n)
  )
  return(my.table)
  
}

#################################################
# Quasi-Poisson Model
#################################################


##### Data #####
# Calculate the number of faculty over the age of 70 in the previous year
# Main sample
schoolYear[, over70.lag:= c(NA, over70[-.N]), by = schoolID]
# Includes post-1994 enterances
schoolYear.post1994[, over70.lag:= c(NA, over70[-.N]), by = schoolID]
# HBCU, PI, and HI
schoolYear.hbcu[, over70.lag:= c(NA, over70[-.N]), by = schoolID]
schoolYear.hbcu <- schoolYear.hbcu[!is.na(schoolYear.hbcu$over70),]
# Alternative aging measure
schoolYear <- merge(schoolYear, schoolYear[, c('nFaculty', 'schoolID')][schoolYear$year == 1993], all=T, by = 'schoolID', suffixes = c('', '93'))


##### Regressions #####
# Variables of interest
variables <- c('nJunior','nFemale','nMinority','nMinorityFemale')

model.list <- list()
for(var in variables){
  # Regression formula
  reg.formula <- paste0(var, '~ over70.lag + as.factor(schoolID) + as.factor(year)')
  
  ## ---- (1) Main List ----#
  main <- glm(reg.formula, data = schoolYear,
             family = quasipoisson, offset = log(nFaculty))
  # get coefficients and robust standard errors
  main.robust <- robust.se(main, schoolYear$schoolID[!is.na(schoolYear$over70.lag)])
  
  # create table rows
  main.table <- createRegressionTable(main.robust["over70.lag", "Estimate"],
                                     main.robust["over70.lag","Std. Error"],
                                     main.robust["over70.lag","Pr(>|z|)"], 
                                     nobs(main))
  
  
  ## ---- (2) Post-1994 Entrances ----#
  post1994 <- glm(reg.formula,data = schoolYear.post1994, family = quasipoisson, offset = log(nFaculty))
  # get coefficients and robust standard errors
  post1994.robust <- robust.se(post1994, schoolYear.post1994$schoolID[!is.na(schoolYear.post1994$over70.lag)])

  # create table rows
  post1994.table <- createRegressionTable(post1994.robust["over70.lag", "Estimate"],
                                          post1994.robust["over70.lag","Std. Error"],
                                          post1994.robust["over70.lag","Pr(>|z|)"], 
                                      nobs(post1994))
  
  ## ---- (3) HBCUs, HI, and PR ----# 
  hbcu <- glm(reg.formula, data = schoolYear.hbcu, family = quasipoisson, offset = log(nFaculty))
  
  # get coefficients and robust standard errors
  hbcu.robust <- robust.se(hbcu, schoolYear.hbcu$schoolID[!is.na(schoolYear.hbcu$over70.lag)])
  
  # create table rows
  hbcu.table <- createRegressionTable(hbcu.robust["over70.lag", "Estimate"],
                                      hbcu.robust["over70.lag","Std. Error"],
                                      hbcu.robust["over70.lag","Pr(>|z|)"], 
                                          nobs(hbcu))
  
  ## ---- (4) Alternative Aging Measure ----# 
  alt.formula <- paste0(var, '~ over70 + as.factor(schoolID) + as.factor(year)')
  alt <- glm(alt.formula, data = schoolYear[schoolYear$year >= 1993,], family = quasipoisson, offset = log(nFaculty93))
  
  # get coefficients and robust standard errors
  alt.robust <- robust.se(alt, schoolYear$schoolID[schoolYear$year >= 1993 & !is.na(schoolYear$nFaculty93)])
  
  # create table rows
  alt.table <- createRegressionTable(alt.robust["over70", "Estimate"],
                                     alt.robust["over70","Std. Error"],
                                     alt.robust["over70","Pr(>|z|)"], 
                                      nobs(alt))
  
  # save regression results
  model.list[[var]] <- rbind(main.table, post1994.table, hbcu.table, alt.table)
}

##############
# Build Table
##############

#Model table
model.table <- data.frame(cbind(model.list$nJunior, 
                                model.list$nFemale, 
                                model.list$nMinority, 
                                model.list$nMinorityFemale))
#Add row and column labels
model.table <- cbind(c("(1) Main Sample", "", "", 
                       "(2) Post-1994 Entrances", "", "",
                       "(3) HBCUs, HI, PR", "", "",
                       "(4) Alternative Aging Measure", "", ""), 
                     model.table)
names(model.table) <- c("","(A) Junior", "(B) Female", "(C) Minority", "(D) Minority Female")


#Print table
print(model.table)
