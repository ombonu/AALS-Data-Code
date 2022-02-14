## ===========================
## Figure 11. Policy Alternatives
## ===========================

# Calculate the number of faculty over the age of 70 in the previous year
schoolYear[, over70.lag:= c(NA, over70[-.N]), by = schoolID]

# Variables of interest
variables <- c('nFemale','nMinority','nMinorityFemale')

# Function to generate a randomized sample of schools (block bootstrap)
blocksample <- function(n,df) {
  unique_schools <- unique(n)
  #choose from unique schools randomly with replacement
  sample_schools <- sample(unique_schools, size=length(unique_schools), replace=T ) 
  #fetch all years for each randomly picked school and rbind
  new_df <- do.call(rbind, lapply(sample_schools, function(x)  df[df$schoolID==x,] ))  
}

# number of draws
draws <- 10000

# years of interest
years <- seq(1972, 2017, by = 1)

## ===========================
## Policy 1: Mandatory Retirement (no uncapping)
## ===========================

# save predicted and additional counts for each sample draw
pred <- additional <- data.frame()
pred <- additional <- years

# loop over variables (female, minority, and minorityFemale)
for (var in variables){
  # generate 10,000 draws (with replacement) of schools
  for(n in 1:draws){
    print(n)
    # construct sample
    sample <- blocksample(schoolYear$schoolID, schoolYear)
    # run model on sample
    formula <- paste0(var, "~ over70.lag + as.factor(schoolID) + as.factor(year)")
    mod <-  glm(formula, data = sample, family = quasipoisson, offset = log(nFaculty))  
    # construct prediction data frame from the sample, setting over70 to 0
    pred.frame <- sample
    pred.frame$over70.lag <- 0
    # for each year, generate predicted variable count and diff between pred and actual in the sample
    # save predicted count and additional count implied
    pred.year <- additional.year <- c()
    for(i in years){
      pred.count <- sum(predict(mod, pred.frame[year == i], type = "response"))
      actual <- sum(pred.frame[[var]][pred.frame$year == i])
      additional.count <- pred.count  - actual
      pred.year <- c(pred.year, pred.count)
      additional.year <- c(additional.year, additional.count)
    }
    pred <- cbind(pred, pred.year)
    additional <- cbind(additional, additional.year)
  }
  
  # Calculate percentile counts
  ci.lr <- ci.upr <- mid.line <- c()
  for(i in years){
    y <- subset(additional, years == i)
    vec <- y[, 2:draws+1]
    ci.lr <- c(ci.lr, quantile(vec, c(.025)))
    ci.upr <- c(ci.upr, quantile(vec, c(.975)))
    mid.line <- c(mid.line, quantile(vec, c(.5)))
  }
  
  # create data frame with 75th, 50th, and 25th quantiles
  assign(paste0(var,"Policy1"), cbind(years, ci.lr, ci.upr, mid.line))
}

## ===========================
## Policy 2: Life Expectancy Index
## ===========================
# In this simulation, MR age set at 71 from 1994 until 2002, 72 from 2003-2008, 73 2009-2017

# year faculty turned 70
facultySchool$turn70year<- facultySchool$byear+70
facultySchool$turn71year<- facultySchool$byear+71
facultySchool$turn72year<- facultySchool$byear+72
facultySchool$turn73year<- facultySchool$byear+73

# faculty who would be mandated to retired under this LE index policy
turned70 <- subset(facultySchool, turn70year < 1994)
turned71 <- subset(facultySchool, turn71year >= 1994 & turn71year < 2002)
turned72 <- subset(facultySchool, turn72year >= 2002 & turn72year < 2008)
turned73 <- subset(facultySchool, turn73year >= 2008 & turn73year <= 2017)

# drop the years of service after faculty turn;
# 70 before 1994
facultySchool$drop70 <- ifelse(facultySchool$facID %in% turned70$facID & facultySchool$year >=  facultySchool$turn70year, 1, 0)
# 71 between 1994 and 2002
facultySchool$drop71 <- ifelse(facultySchool$facID %in% turned71$facID & facultySchool$year >=  facultySchool$turn71year, 1, 0)
# 72 between 2002 and 2008
facultySchool$drop72 <- ifelse(facultySchool$facID %in% turned72$facID & facultySchool$year >=  facultySchool$turn72year, 1, 0)
# 73 between 2008 and 2017
facultySchool$drop73 <- ifelse(facultySchool$facID %in% turned73$facID & facultySchool$year >=  facultySchool$turn73year, 1, 0)

facultySchoolLE <- subset(facultySchool, drop70 == 0 & drop71 == 0 & drop72 == 0 & drop73 == 0)


# recalculate over70 in each year after removing service years of faculty who would have retired under this policy
schoolYearLE <- facultySchoolLE[, .(schoolName = max(schoolName),
                                    nFacultyAdj = length(facID[tenure.tenureTrack==1]),
                                    nOver70 = sum(age[tenure.tenureTrack==1]>=70, na.rm = T),
                                    over70 = mean(age[tenure.tenureTrack==1]>=70, na.rm = T),
                                    nFemale = sum(female[tenure.tenureTrack==1], na.rm = T),
                                    nMinority = sum(minority[tenure.tenureTrack==1], na.rm = T),
                                    nMinorityFemale = sum(minorityFemale[tenure.tenureTrack==1], na.rm = T)), 
                                by=.(year, schoolID)]

schoolYearLE <- schoolYearLE[order(year),]
# Calculate the number of faculty over the age of 70 in the previous year (for sample with LE retirement plan)
schoolYearLE[, over70.lag:= c(NA, over70[-.N]), by = schoolID]

pred <- additional <- data.frame()
pred <- additional <- years

for (var in variables){
  for(n in 1:draws){
    print(n)
    # construct sample, start at each school's second year
    sample <- blocksample(schoolYear$schoolID, schoolYear)
    sample <- subset(sample, !is.na(over70.lag))
    
    # run model on sample
    formula <- paste0(var, "~ over70.lag + as.factor(schoolID) + as.factor(year)")
    mod <-  glm(formula, data = sample, family = quasipoisson, offset = log(nFaculty))  
    
    # merge in pred frame schoolYearLE
    pred.frame <- merge(sample[,c("schoolID", "year", "nFaculty", "nFemale", "nMinority", "nMinorityFemale")],
                        schoolYearLE[,c("schoolID", "year", "over70.lag")], 
                        by=c("schoolID", "year"), all.x=T)  
    pred.frame <- subset(pred.frame, !is.na(over70.lag))
    
    # for each year, generate the predictions and store
    pred.year <- additional.year <- c()
    for(i in years){
      pred.count <- sum(predict(mod, pred.frame[year == i], type = "response"))
      actual <- sum(pred.frame[[var]][pred.frame$year == i])
      additional.count <- pred.count  - actual
      pred.year <- c(pred.year, pred.count)
      additional.year <- c(additional.year, additional.count)
    }
    pred <- cbind(pred, pred.year)
    additional <- cbind(additional, additional.year)
  }
  
  # calculate percentiles
  ci.lr <- ci.upr <- mid.line <- c()
  for(i in years){
    y <- subset(additional, years == i)
    vec <- y[, 2:draws+1]
    ci.lr <- c(ci.lr, quantile(vec, c(.025)))
    ci.upr <- c(ci.upr, quantile(vec, c(.975)))
    mid.line <- c(mid.line, quantile(vec, c(.5)))
  }
  # create data frame with 75th, 50th, and 25th quantiles
  assign(paste0(var,"Policy2"), cbind(years, ci.lr, ci.upr, mid.line))
}


## ===========================
## Policy 3: Delayed Uncapping
## ===========================
# Missing faculty counterfactual: 15 year exemption (i.e. uncapping in 2001)

# year faculty turned 70
facultySchool$turn70year<- facultySchool$byear+70

# identify faculty affected by 15 year exemption - turned 70 before 2002
turned70 <- subset(facultySchool, turn70year < 2002)

# drop the years after they turned 70
facultySchool$drop <- ifelse(facultySchool$facID %in% turned70$facID & facultySchool$year >=  facultySchool$turn70year, 1, 0)
facultySchool15yr <- subset(facultySchool, drop == 0)

# recalculate over70 in each year after - this is the pred frame
schoolYear15exp <- facultySchool15yr[, .(schoolName = max(schoolName),
                                         nFacultyAdj = length(facID[tenure.tenureTrack==1]),
                                         nOver70 = sum(age[tenure.tenureTrack==1]>=70, na.rm = T),
                                         over70 = mean(age[tenure.tenureTrack==1]>=70, na.rm = T),
                                         nFemale = sum(female[tenure.tenureTrack==1], na.rm = T),
                                         nMinority = sum(minority[tenure.tenureTrack==1], na.rm = T),
                                         nMinorityFemale = sum(minorityFemale[tenure.tenureTrack==1], na.rm = T)), 
                                     by=.(year, schoolID)]


schoolYear15exp <- schoolYear15exp[order(year),]
# Calculate the number of faculty over the age of 70 in the previous year
schoolYear15exp[, over70.lag:= c(NA, over70[-.N]), by = schoolID]

pred <- additional <- data.frame()
pred <- additional <- years

for (var in variables){
  for(n in 1:draws){
    print(n)
    # construct sample, start at each school's second year
    sample <- blocksample(schoolYear$schoolID, schoolYear)
    sample <- subset(sample, !is.na(over70.lag))
    
    # run model on sample
    formula <- paste0(var, "~ over70.lag + as.factor(schoolID) + as.factor(year)")
    mod <-  glm(formula, data = sample, family = quasipoisson, offset = log(nFaculty))  
    
    # merge in pred frame
    pred.frame <- merge(sample[,c("schoolID", "year", "nFaculty", "nFemale", "nMinority", "nMinorityFemale")],
                        schoolYear15exp[,c("schoolID", "year", "over70.lag")], 
                        by=c("schoolID", "year"), all.x=T)  
    pred.frame <- subset(pred.frame, !is.na(over70.lag))
    
    # for each year, generate the predictions and store
    pred.year <- additional.year <- c()
    for(i in years){
      pred.count <- sum(predict(mod, pred.frame[year == i], type = "response"))
      actual <- sum(pred.frame[[var]][pred.frame$year == i])
      additional.count <- pred.count  - actual
      pred.year <- c(pred.year, pred.count)
      additional.year <- c(additional.year, additional.count)
    }
    pred <- cbind(pred, pred.year)
    additional <- cbind(additional, additional.year)
    
  }
  # Calculate percentiles
  ci.lr <- ci.upr <- mid.line <- c()
  for(i in years){
    y <- subset(additional, years == i)
    vec <- y[, 2:draws+1]
    ci.lr <- c(ci.lr, quantile(vec, c(.025)))
    ci.upr <- c(ci.upr, quantile(vec, c(.975)))
    mid.line <- c(mid.line, quantile(vec, c(.5)))
  }
  # create data frame with 75th, 50th, and 25th quantiles
  assign(paste0(var,"Policy3"), cbind(years, ci.lr, ci.upr, mid.line))
  
}


## ===========================
## Create Figure
## ===========================

par(mar=c(3,3,3,1), mgp=c(1.5, 0.5, 0), tcl=-0.3, oma=c(1, 0, 4, 0), mfrow = c(3,3))

## Policy 1
with(as.data.frame(nFemalePolicy1), plot(years, mid.line,
                                    xlab = "Year", 
                                    ylab = "Predicted - actual",
                                    type = "l", ylim=c(0, 250)))
with(as.data.frame(nFemalePolicy1), polygon(c(years, rev(years)), 
                                       c(ci.lr, 
                                         rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))

with(as.data.frame(nMinorityPolicy1), 
     plot(years, mid.line,
          xlab = "Year", 
          ylab = "Predicted - actual",
          type = "l", ylim=c(0, 150)))
with(as.data.frame(nMinorityPolicy1),
     polygon(c(years, rev(years)), 
             c(ci.lr, 
               rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))

with(as.data.frame(nMinorityFemalePolicy1), 
     plot(years, mid.line, #main = "Missing minority female faculty",
          xlab = "Year", 
          ylab = "Predicted - actual",
          type = "l", ylim=c(0, 150)))
with(as.data.frame(nMinorityFemalePolicy1),
     polygon(c(years, rev(years)), 
             c(ci.lr, 
               rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))


# policy 2
with(as.data.frame(nFemalePolicy2), plot(years, mid.line,
                                       xlab = "Year", 
                                       ylab = "Predicted - actual",
                                       type = "l", ylim=c(0, 250)))
with(as.data.frame(nFemalePolicy2), polygon(c(years, rev(years)), 
                                          c(ci.lr, 
                                            rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))


with(as.data.frame(nMinorityPolicy2), 
     plot(years, mid.line,
          xlab = "Year", 
          ylab = "Predicted - Actual",
          type = "l", ylim=c(0, 150)))
with(as.data.frame(nMinorityPolicy2),
     polygon(c(years, rev(years)), 
             c(ci.lr, 
               rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))

with(as.data.frame(nMinorityFemalePolicy2), 
     plot(years, mid.line, #main = "Missing minority female faculty",
          xlab = "Year", 
          ylab = "Predicted - Actual",
          type = "l", ylim=c(0, 150)))
with(as.data.frame(nMinorityFemalePolicy2),
     polygon(c(years, rev(years)), 
             c(ci.lr, 
               rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))

# policy 3
with(as.data.frame(nFemalePolicy3), plot(years, mid.line, #main = "Missing female faculty",
                                       xlab = "Year", 
                                       ylab = "Predicted - actual",
                                       type = "l", ylim=c(0, 250)))
with(as.data.frame(nFemalePolicy3), polygon(c(years, rev(years)), 
                                          c(ci.lr, 
                                            rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))


with(as.data.frame(nMinorityPolicy3), 
     plot(years, mid.line, 
          xlab = "Year", 
          ylab = "Predicted - Actual",
          type = "l", ylim=c(0, 150)))
with(as.data.frame(nMinorityPolicy3),
     polygon(c(years, rev(years)), 
             c(ci.lr, 
               rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))

with(as.data.frame(nMinorityFemalePolicy3), 
     plot(years, mid.line,
          xlab = "Year", 
          ylab = "Predicted - Actual",
          type = "l", ylim=c(0, 150)))
with(as.data.frame(nMinorityFemalePolicy3),
     polygon(c(years, rev(years)), 
             c(ci.lr, 
               rev(ci.upr)),col=rgb(0,0,0,.1), border = NA))
abline(v = 1993,col = rgb(0, 0, 1, 0.4))



mtext("Policy Alternatives", line= 2, font=2, outer = TRUE, cex = 1.1)
mtext("(1) Mandatory Retirement (no uncapping)", line = -.5, side = 3, outer = TRUE, at = c(.235), las=1, font=2)
mtext("Additional female faculty", line = -2.5, side = 3, outer = TRUE, at = c(.18), las=1, font=1,cex = .85)
mtext("Additional minority faculty", line = -2.5, side = 3, outer = TRUE, at = c(.52), las=1, font=1,cex = .85)
mtext("Additional minority female faculty", line = -2.5, side = 3, outer = TRUE, at = c(.85), las=1, font=1, cex = .85)

mtext("(2) Life Expectancy Index", line = -18.35, side = 3, outer = TRUE, at = c(.15), las=1, font=2)
mtext("Additional female faculty", line = -20, side = 3, outer = TRUE, at = c(.18), las=1, font=1,cex = .85)
mtext("Additional minority faculty", line = -20, side = 3, outer = TRUE, at = c(.52), las=1, font=1,cex = .85)
mtext("Additional minority female faculty", line = -20, side = 3, outer = TRUE, at = c(.85), las=1, font=1, cex = .85)


mtext("(3) Delayed Uncapping", line = -35.5, side = 3, outer = TRUE, at = c(.14), las=1, font=2)
mtext("Additional female faculty", line = -37, side = 3, outer = TRUE, at = c(.18), las=1, font=1,cex = .85)
mtext("Additional minority faculty", line = -37, side = 3, outer = TRUE, at = c(.52), las=1, font=1,cex = .85)
mtext("Additional minority female faculty", line = -37, side = 3, outer = TRUE, at = c(.85), las=1, font=1, cex = .85)

