# ====== start ===== #

# basic example -----------------------------------------------------------
#### generate some data and see can we locate them
set.seed(2020)
plot(x=1:10, y=rnorm(10))
(loc = locator(10)) # click on the 10 points
points(loc$x, loc$y, col="red") # plot them over
# looks good?


# example for (multiple) curves ------------------------------------------------------
# fda package for data------------------------------------------
# install.packages(fda) # install fda package if you havent
library(fda)
data("gait") # just the data just do ?gait if unsure
demo.data <- gait[,,2] # just take knee angle
time = seq(from=0.025, to=0.975, by=0.05) # set up time for the plot
dim(demo.data) # 20x39
head(demo.data) # each col is a different gait cycle 
# each row is an increment of time 
# basically each column is a curve


# set up to use locator function ------------------------------------------
(nsubjects <- ncol(demo.data)) # (39)
# set up matrix to store clicks
# 39 rows one for each subject
# 2 columns - we'll pick one landmark and store x & y
landmarks <- matrix(0, nrow=nsubjects, ncol=2) 
colnames(landmarks) = c("x", "y")
par(mfrow=c(1,1), ask=T) # means you click return to show next plot (ie move to next subject)


# apply locator - lets click on the peak ----------------------------------
# just demo for the first 5 subjects... change 5 to nsubjects if you were to loop through all
for(icase in 1:5){
  plot(time, demo.data[,icase], "l", main=paste("case", icase),
       xlab="time", ylab="angle")
  my_loc = locator(n=1) # 1 click and will store x & y 
  landmarks[icase, 1]=my_loc$x[1]
  landmarks[icase, 2]=my_loc$y[1]
}

### look at where they were stored
landmarks[1:5,] # looks good!
# turn asking off
par(mfrow=c(1,1), ask=T)

