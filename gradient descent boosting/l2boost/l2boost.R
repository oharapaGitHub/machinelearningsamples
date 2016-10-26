# implementation of the l2boost gradient descent algorithm
# this will test to see if can successfully classify 6 from 9
# it will use the minst dataset

# install the l2 boost package if not already installed, if the pacakge 
# is already installed the library is loaded
setupL2Boost <- function() {
  if(!"l2boost" %in% rownames(installed.packages())) {
    # the install wil load the packages, MASS, parallel if not already installed
    # a checksum was done for R, data, inst.  Not sure what that means
    install.packages("l2boost")
  }
  library(l2boost)
}

init <- function() {
  setupL2Boost()
}

train <- function(x,y) {
  # execute
  l2.object <- l2boost(x, y, M=1000, nu=.01)
  
  # Plot the boosting rho, and regression beta coefficients as a function of
  # boosting steps m
  #
  # Note: The selected coordinate trajectories are colored in red after selection, and
  # blue before. Unselected coordinates are colored grey.
  #
  par(mfrow=c(2,2))
  plot(l2.object)
  plot(l2.object, type="coef")
  
  #increased shrinkage and number of iterations.
  l2.shrink <- l2boost(x,y,M=5000, nu=1.e-3)
  plot(l2.shrink)
  plot(l2.shrink, type="coef")
}