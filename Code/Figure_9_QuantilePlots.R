## ===========================
## Figure 9. Diversification by Quantile and Rank
## ===========================


######################
## Calculate Quantiles
######################

## Proportion of junior faculty
schoolYear$propJunior <- schoolYear$nJunior/schoolYear$nFaculty

## Average prop.over 70 over all years
school.averages <- aggregate(over70 ~ schoolID, data=schoolYear, FUN=mean)

## Seperate schools into above/below median
school.averages$bin <- findInterval(school.averages$over70, vec = quantile(school.averages$over70, probs=seq(0,1,by=1/2)))

## Get aggregated data for the two groups of schools
# Top half
school.sub <- school.averages$schoolID[school.averages$bin==1]
dta.sub <- schoolYear[schoolYear$schoolID %in% school.sub,]
dta.agg.first <- aggregate(cbind(over70, female, minority, minorityFemale, propJunior) ~ year, data=dta.sub, FUN=mean)

# Bottom half
school.sub <- school.averages$schoolID[school.averages$bin==2]
dta.sub <- schoolYear[schoolYear$schoolID %in% school.sub,]
dta.agg.second <- aggregate(cbind(over70, female, minority, minorityFemale, propJunior) ~ year, data=dta.sub, FUN=mean)

###############
## School Rank
###############

## T14 v. others schools
## Rank meta data
schoolMetaData <- read.csv("SchoolMetaData/Output/schoolMetadataTable.csv")
schoolMetaData.AALS <- subset(schoolMetaData, AALS.Member == 1)
schoolMetaData2010 <- subset(schoolMetaData.AALS, reportingYear == 2010)

## T-14 schools
t14 <- subset(schoolMetaData2010, rank <= 14)
schoolYear$rank <-  ifelse(schoolYear$schoolName %in%  t14$Name, "T14", "Other")

## Subset into two groups
schoolYear.t14 <- subset(schoolYear, rank == "T14")
schoolYear.others <- subset(schoolYear, rank == "Other")

## Averages
## T14 (top 10)
dta.agg.T14 <- aggregate(cbind(over70, female, minority, minorityFemale, propJunior) ~ year, data=schoolYear.t14, FUN=mean)

## Other Ranks
dta.agg.other <- aggregate(cbind(over70, female, minority, minorityFemale, propJunior) ~ year, data=schoolYear.others, FUN=mean)


#################
## Create Figure
#################

my.vars <- c("female", "minority", "minorityFemale")
my.labs <- c("Female Faculty", "Minority Faculty", "Minority Female")
row1 <- c("A. ", "B. ", "C. ")
row2 <- c("D. ", "E. ", "F. ")

####==== Plot Graph
par(mfrow = c(2,3))
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0),tcl=-0.3, cex.lab = 1.3)

## First row of plots
## Loop through variables and labels
for(j in 1:3){
  ## Get range for this variable
  max <- max(dta.agg.first[[my.vars[j]]], dta.agg.second[[my.vars[j]]])
  min <- min(dta.agg.first[[my.vars[j]]], dta.agg.second[[my.vars[j]]])
  
  ## Top half
  with(dta.agg.first, plot(year, eval(parse(text = my.vars[j])), type="l", ylim = range(max, min),
                           main=paste0(row1[j], my.labs[j], " by Age Quantile"), 
                           col = rgb(1,0,0), xlim = c(1971, 2018.5),
                           xlab = "Year", ylab = paste0("Proportion ", my.labs[j])
  ))
  
  abline(v=1994, col = rgb(0, 0, 1, 0.4))
  
  ## Bottom half
  with(dta.agg.second, lines(year, eval(parse(text = my.vars[j])), col = rgb(0,0,1)))
  
  if(j == 1){
    text(2008, .27, "Top half", col = rgb(0,0,1), cex = 1)
    text(2006.5, 0.39, "Bottom half", col = rgb(1,0,0), cex = 1)  # shift this up -- new dimensions threw this off
    
    text(x=1990.6, y=0.38, "1994", col = rgb(0, 0, 1, 0.4)) # shift this over -- new dimensions threw this off
    text(x=1987.1, y=0.36, "Uncapping", col = rgb(0, 0, 1, 0.4))
    
  }
}

## Second row of plots
for(j in 1:3){
  ## Get range for this variable
  max <- max(dta.agg.T14[[my.vars[j]]], dta.agg.other[[my.vars[j]]]) #nyu[[my.vars[j]]], davis[[my.vars[j]]],
  min <- min(dta.agg.T14[[my.vars[j]]], dta.agg.other[[my.vars[j]]]) #nyu[[my.vars[j]]], davis[[my.vars[j]]],

  with(dta.agg.T14, plot(year, eval(parse(text = my.vars[j])), type="l", ylim = range(max, min),
                         col = 'purple', xlim = c(1971, 2018.5),
                         main=paste0(row2[j], my.labs[j], " by Rank"),
                         xlab = "Year", ylab = paste0("Proportion ", my.labs[j])
  ))
  
  ## Others
  with(dta.agg.other, lines(year, eval(parse(text = my.vars[j])), col = 'dark green'))
  
  abline(v=1994, col = rgb(0, 0, 1, 0.4))
  
  if(j == 1){
    text(2007.1, .22, "Top 10", col = 'purple', cex = 1)
    text(2005.5, .36, "Others", col = 'dark green', cex = 1)  # shift this up -- new dimensions threw this off
    
  }
}
