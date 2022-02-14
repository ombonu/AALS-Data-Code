## =========================================
## Figure 6. Prop. Over 70 by School Type
## =========================================

# plot prop. over 70 by public and private schools

###############
## School Type
###############

## subset into two groups
schoolYear.public <- subset(schoolYear, private == 0)
schoolYear.private <- subset(schoolYear, private == 1)

# Averages
dta.agg.public <- aggregate(cbind(over70) ~ year, data=schoolYear.public, FUN=mean)
dta.agg.private <- aggregate(cbind(over70) ~ year, data=schoolYear.private, FUN=mean)

## ===========================
## Create Figure
## ===========================

par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0), tcl=-0.3, cex.lab = 1.3)

plot(dta.agg.private$year, dta.agg.private$over70, type = 'l', ylab = 'Prop. Faculty Over 70', 
     xlab = 'Year', main = 'Faculty Over 70 By School Type', col = 'purple')
lines(dta.agg.public$year, dta.agg.public$over70, col = 'darkgreen')

abline(v=1994, col = rgb(0, 0, 1, 0.4))

text(x=1991.5, y=0.145, "1994", col = rgb(0, 0, 1, 0.4))
text(x=1989, y=0.135, "Uncapping", col = rgb(0, 0, 1, 0.4))
text(x=2010, y=0.11, "Private", col = 'purple')
text(x=2015, y=0.06, "Public", col = 'darkgreen')
