## ===========================
## Figure 2. Proportion of faculty over the age of 70
## ===========================

## Create a data frame with a year for each school
## All schools should have NAs for years they don't appear
schools <- unique(schoolYear$schoolID)
n.schools <- length(schools)
tmp <- data.frame('year'=rep(c(1971:2017), n.schools), schoolID = rep(schools, each=47))

## Merge in schoolYear table
schoolYear <- merge(tmp, schoolYear, by=c('year', 'schoolID'), all.x=T)

## Number of years to plot
years <- c(1971:2017)
n.years <- length(years)

## ===========================
## Create Figure
## ===========================
par(mfrow=c(1,1), mar=c(3,3,2,1),mgp=c(1.5,0.5,0), tcl=-0.3)

variable <- 'over70'
plot(years, years,  type="n", xlab="Year", ylab="Proportion Over 70",
     main = 'Faculty Over 70', ylim = c(0.0, max(schoolYear$over70, na.rm = T)))

## Points for each school
for(i in 1:n.schools){
  n.var <- schoolYear[['nFaculty']][schoolYear$schoolID==schools[i]]
  points(jitter(years,factor=2), schoolYear[[variable]][schoolYear$schoolID==schools[i]],
         col=rgb(0,0,0,0.07), pch=16, cex=0.3*sqrt(n.var/pi))
}

## Yearly average as a line
lines(year$year, year$over70, col = rgb(1,0,0), lwd=2)

## Add line at 1994
abline(v=1994, col = rgb(0,0,1,0.4))

## Add label 
text(x = 1991.5, y=0.4, "1994", col = rgb(0,0,1,0.4))
text(x = 1989.5, y=0.38, "Uncapping", col = rgb(0,0,1,0.4))


