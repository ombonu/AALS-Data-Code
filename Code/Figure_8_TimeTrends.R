## ======================================
## Figure 8. Race and Gender Time Trends
## ======================================

##
## Plot time trend of gender and minority for all schools. 
## 

####=====

## All schools should have NAs for years they don't appear
# Create a df with a year for each school
schools <- unique(schoolYear$schoolID)
n.schools <- length(schools)
tmp <- data.frame('year'=rep(c(1971:2017), n.schools), schoolID = rep(schools, each=47))

# Merge with schoolYear.dta
schoolYear <- merge(tmp, schoolYear, by=c('year', 'schoolID'), all.x=T)

# number of years
years <- c(1971:2017)
n.years <- length(years)

## ===========================
## Create Figure
## ===========================
par(mfrow=c(1,2))
par(mar=c(3,3,2,1),mgp=c(1.5,0.5,0), tcl=-0.3, cex.main = 1.3, cex.lab = 1.2)

## Gender
variable <- 'female'
plot(years, years,  type="n", xlab="Year", ylab="Proportion Female",
     main = 'Female Faculty', ylim=c(0.0, max(schoolYear$female, na.rm = T)))
for(i in 1:n.schools){
  n.var <- schoolYear[['nFaculty']][schoolYear$schoolID==schools[i]]
  points(jitter(years,factor=2), schoolYear[[variable]][schoolYear$schoolID==schools[i]],
         col=rgb(0,0,0, 0.1), pch=16, cex=0.3*sqrt(n.var/pi))
}

lines(year$year, year$female, col = rgb(1,0,0), lwd=2)

abline(v=1994, col = rgb(0,0,1,0.4))
text(x=1991.5, y=0.64, "1994", col = rgb(0,0,1,0.4))
text(x=1989, y=0.61, "Uncapping", col = rgb(0,0,1,0.4))

## Minority
variable <- 'minority'
plot(years, years,  type="n", xlab="Year", ylab="Proportion Minority",
     main = 'Minority Faculty', ylim=c(0.0, max(schoolYear$minority, na.rm = T)))
for(i in 1:n.schools){
  var.name <- paste0('n', variable)
  n.var <- schoolYear[['nFaculty']][schoolYear$schoolID==schools[i]]
  points(jitter(years,factor=2), schoolYear[[variable]][schoolYear$schoolID==schools[i]],
         col=rgb(0,0,0,0.1), pch=16, cex=0.3*sqrt(n.var/pi))
}

## yearly average as a line
lines(year$year, year$minority, col = rgb(1,0,0), lwd=2)

abline(v=1994, col = rgb(0,0,1,0.4))
