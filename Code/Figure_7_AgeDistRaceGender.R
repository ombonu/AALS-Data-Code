## =============================================
## Figure 7. Age Distribution by Race and Gender
## =============================================

# Plot age distribution in 1971, 1993, and 2017
my.years <- c(1971, 1993, 2017)


## ===========================
## Create Figure
## ===========================
par(mgp=c(1.5, 0.5, 0), tcl=-0.3, oma=c(1, 0, 4, 0), mfrow = c(3,2))

for(i in 1:length(my.years)){
  
  ## By Race
  par(mar=c(3,3,2,0))
  hist(facultySchool$age[facultySchool$tenure.tenureTrack == 1 &
                           facultySchool$minority==0 & facultySchool$year%in%my.years[i]],
       xlim=c(13,80), main = " "
       , xlab="Age", freq=T, col=rgb(0,0,1,0.4), border="white", ylim=c(0,1200))
  hist(facultySchool$age[facultySchool$tenure.tenureTrack == 1 &
                           facultySchool$minority==1 & facultySchool$year%in%my.years[i]],
       add=T, col=rgb(1,0,0,0.4), freq=T, border="white")
  abline(v=70, col= 'darkgrey')
  
  if(i==2){
    text(21.5, 110, "Minority", col=rgb(1,0,0, 0.4),cex = 1, font=2)
    text(27, 425, "Majority", col = rgb(0,0,1, 0.4), cex = 1, font=2)
    
  }
  
  ## By gender
  par(mar=c(3,0,2,3))
  hist(facultySchool$age[facultySchool$tenure.tenureTrack == 1 &
                           facultySchool$female==0 & facultySchool$year%in%my.years[i]],
       xlim=c(13,80), main= " " #my.years[i]
       , xlab="Age", freq=T, col=rgb(0, 0.5, 0, 0.4), border="white", ylim=c(0,1200), yaxt='n', ylab = '')
  hist(facultySchool$age[facultySchool$tenure.tenureTrack == 1 &
                           facultySchool$female==1 & facultySchool$year%in%my.years[i]],
       xlim=c(20,80), add=T, col=rgb(.8,.7,.5, .8), freq=T, border="white")
  abline(v=70, col='darkgrey')
  
  if(i==2){
    text(21.5, 100, "Female", col = rgb(.8,.7, .5), cex = 1, font=2)
    text(29.5, 350, "Male", col = rgb(0, 0.5, 0, 0.6), cex = 1, font=2)
  }
}



mtext("Age Distribution", line= 2, font=2, outer = TRUE, cex = 1.1)
mtext("By Race", line = 0, side = 3, outer = TRUE, at = c(.25), las=1, font=2)
mtext("By Gender", line = 0, side = 3, outer = TRUE, at = c(.75), las=1, font=2)
mtext("1971", line = -1, side = 3, outer = TRUE, at = c(.50), las=1, font=2)
mtext("1993", line = -19, side = 3, outer = TRUE, at = c(.50), las=1, font=2)
mtext("2017", line = -36, side = 3, outer = TRUE, at = c(.50), las=1, font=2)

