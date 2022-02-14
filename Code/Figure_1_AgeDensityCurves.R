## ===========================
# Figure 1. Kernel density plot of faculty age across all schools for each year.
## ===========================
library(plotrix)

## ===========================
## Functions
## ===========================
## Function to plot age densities across schools
plotAgeDensity <- function(data, my.range = c(25, 85), 
                           my.ylim = c(0, 0.06),
                           my.col = rgb(seq(0,1,length=n.years), 0, seq(1,0,length=n.years),0.6),
                           years = c(1971:2017)){
  ## Number of relevant years to plot
  n.years <- length(years)
  
  ## Fix up color vector to correspond to years
  my.col <- my.col[years%in%years]
  
  ## Years-Color dataframe 
  my.col.legend <- as.data.frame(my.col)
  my.col.legend$years <- years
  my.col.legend$val <- seq(1:nrow(my.col.legend))
  
  ## Age vector for first observed year (depends on year and faculty school composition)
  ages.year1 <- years[1] - data$byear[(data$year==years[1])]
  
  ## Range (to make sure densities have a sharp stop)
  age.range.year1 <- range(ages.year1,na.rm=T)
  
  ## First plot: before uncapping
  ## Add densities for first year
  plot(density(ages.year1, na.rm=T, from=age.range.year1[1],to=age.range.year1[2]),
       xlab = "Faculty Age", 
       xlim = my.range, col = my.col[1], ylim = my.ylim, main = "Before Uncapping")
  abline(v=70,col="darkgrey")
  
  ## Add raster legend
  ## Plot earliest year on top of the legend
  ## Add legend without border
  gradient.rect(85,.04,86,.06,
                col = rev(my.col)[25:47], gradient="y", 
                border = NA)
  ## Add labels manually
  text(81.5, .05925, "1971")
  text(81.5, .04075, "1993")
  
  ## Add densities for each of the other years
  if(n.years>1){
    for(j in 2:23){
      ## Calculating age vector
      ages.year.j <- years[j] -  data$byear[(data$year==years[j])]
      ## Calculating range
      age.range.year.j <- range(ages.year.j, na.rm=T)
      ## Adding density
      lines(density(ages.year.j, na.rm=T, from=age.range.year.j[1],to=age.range.year.j[2]),
            col=my.col[j]) 
    }
  }
  
  ## Second plot: post uncapping
  ## Age vector for first observed year (depends on year and faculty school composition)
  ages.year1post <- years[24] - data$byear[(data$year==years[24])]
  
  ## Range (to make sure densities have a sharp stop)
  age.range.year1post <- range(ages.year1post,na.rm=T)
  
  plot(density(ages.year1post, na.rm=T, from=age.range.year1post[1],to=age.range.year1post[2]),
       xlab = "Faculty Age", 
       xlim = my.range, col = my.col[24], ylim = my.ylim, main = "After Uncapping")
  abline(v=70,col="darkgrey")
  
  ## Add raster legend
  ## Plot earliest year on top of the legend
  ## Add legend without border
  gradient.rect(85,.04,86,.06,
                col = rev(my.col)[1:24], gradient="y", 
                border = NA)
  ## Add labels manually
  text(81.5, .05925, "1994")
  text(81.5, .04075, "2017")
  
  ## Adding densities for each of the other years
  if(n.years>23){
    for(j in 24:47){
      ## Calculating age vector
      ages.year.j <- years[j] -
        data$byear[(data$year==years[j])]
      ## Calculating range
      age.range.year.j <- range(ages.year.j, na.rm=T)
      ## Adding density
      lines(density(ages.year.j, na.rm=T, from=age.range.year.j[1],to=age.range.year.j[2]),
            col=my.col[j]) 
    }
  }
}

## ===========================
# Create Figure
## ===========================
## Set start and end years
start.year <- 1971
end.year <- 2017

## Vector of all years
years <- start.year:end.year
## Number of years
n.years <- length(years)
my.col <- rgb(seq(0, 1, length=n.years), 0, seq(1,0,length=n.years),0.6)

par(mfrow = c(1, 2), mar=c(3,3,2,1),mgp=c(1.5,0.5,0), tcl=-0.3, cex.main = 1.3, cex.lab = 1.2)
plotAgeDensity(facultySchool, years = years,
               my.col = my.col)

