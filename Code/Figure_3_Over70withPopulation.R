## ===========================
## Figure 3. Proportion of faculty over the age of 70 and population average. 
## ===========================

## ===========================
## Create Figure
## ===========================
## Number of years
years <- c(1971:2017)
n.years <- length(years)

par(mfrow=c(1,1), mar=c(3,3,2,1),mgp=c(1.5,0.5,0), tcl=-0.3)
variable <- 'over70'
plot(years, years,  type="n", xlab="Year", ylab="Proportion Over 70",
     main = 'Over 70', ylim = c(0.0, .2))

## Yearly average as a line
## Faculty
lines(year$year, year$over70, col = rgb(1,0,0), lwd=2)
## Overall population
lines(ageCensus$year[!is.na(ageCensus$propOver70)], ageCensus$propOver70[!is.na(ageCensus$propOver70)],col = rgb(0,0,0, .7), lwd=2)

## Add line at 1994 (uncapping)
abline(v=1994, col = rgb(0,0,1,0.4))

## Add labels
text(x = 1991.25, y=0.2, "1994", col = rgb(0,0,1,0.4),cex = .8)
text(x = 1988.5, y=0.185, "Uncapping", col = rgb(0,0,1,0.4),cex = .8)
text(x = 1972, y = 0.095, "U.S.", col = rgb(0,0,0, .7),cex = .8)
text(x = 1974.75, y = 0.08, "population", col = rgb(0,0,0, .7),cex = .8)
text(x = 1974, y = 0.027, "Faculty", col = rgb(1,0,0), cex = .8)

