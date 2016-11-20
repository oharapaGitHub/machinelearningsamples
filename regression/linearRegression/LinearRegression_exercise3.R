###
# this script works through exercise three, which studies the relationship between the cost
# function and the learning rate alpha
# it also attempts to manipulate the cost function to make it generic and allow the application
# of other cost function.
# namely huber, also want to look at taylors.
###

# clear the workspace
rm(list = ls())

# source of the lession
# - 
ex2.readData <- function() {
  wd <- getwd()
  modeldata <- read.table(paste(wd,"/data/ex2_data/ex2x.dat",sep=""), col.names=c("x"), header = FALSE)
  y <- read.table(paste(wd,"/data/ex2_data/ex2y.dat",sep=""), col.names=c("y"), header = FALSE)
  modeldata$y <- y$y
  return(modeldata)
}

ex2.plot <- function(data) {
  plot(x=data[[1]],y=data[[2]], xlab = "Height in Meters", ylab = "Age in years")
} 

ex2.addInterceptToX <- function(data) {
  # function to add a column of ones to the x column in the dataset
  return(cbind(rep(1),data))
}

ex2.generateThetaAlt <- function(data) {
  # generated a theta based on the number columns a m*n, where m
  # equals the number of columns
  theta <- numeric(ncol(data))
  matrix(data=theta,nrow=length(theta),ncol = 1)
}

ex2.generateTheta <- function(data) {
  theta0 <- numeric(nrow(data[[1]]))
  theta1 <- numeric(nrow(data[[1]]))
  
  return(matrix(c(theta0,theta1), nrow=nrow(data[[1]]), ncol=2, byrow = FALSE))
}


ex2.all <- function(alpha = 0.07,n.rounds=1500) {
  par(pch=22, col="red") # plotting symbol and color
  par(mar=c(1,1,1,1))  # all plots on one page 
  ex2Data <- ex2.readData()
  ex2.plot(ex2Data)
  ex2Data[[1]] <- ex2.addInterceptToX(ex2Data[[1]])
  thetaData <- ex2.generateThetaAlt(ex2Data)
  
  initialThetas <- matrix(thetaData)
  for(index in 1:n.rounds) {
    
    thetaData <- ex2.linearRegressWithGradientDescentSecondAttempt(ex2Data$x, ex2Data$y, thetaData, alpha)
    if(index == 1) {
      initialThetas <-thetaData
    }
  }
  
  ## need to plot my thetas... 
  
  plot(x=ex2Data$x[,2],y=ex2Data$y, xlab = "Height in Meters", ylab = "Age in years")
  lines(x=ex2Data$x[,2],y=ex2Data$x %*% thetaData)
  return (c(initialThetas,thetaData))
}

ex2.linearRegressWithGradientDescentSecondAttempt <- function(xdata, target, inThetas, alpha) {
  tempThetas = matrix(data = inThetas, nrow = nrow(inThetas), ncol = 1)
  for(thetaIndex in 1:nrow(inThetas)) {
    linRegressionResult <- ex2.linearRegress(data = xdata, thetas = inThetas)
    linRegressionLessTarget <- linRegressionResult - target
    
    vectorToSum <-  linRegressionLessTarget * xdata[,thetaIndex]
    tempThetas[thetaIndex] <- (inThetas[thetaIndex] - (alpha *(sum(vectorToSum)/nrow(xdata))))
  }
  inThetas <- tempThetas
}
ex2.linearRegress <- function(data, thetas) {
  # don't believe it
  # think the correct answer is to use sweep # todo try with sweep
  data %*% thetas
}
