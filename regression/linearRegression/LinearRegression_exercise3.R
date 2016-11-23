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
ex3.readData <- function() {
  wd <- getwd()
  modeldata <- read.table(paste(wd,"/data/ex3_data/ex3x.dat",sep=""), header = FALSE)
  y <- read.table(paste(wd,"/data/ex3_data/ex3y.dat",sep=""), col.names=c("y"), header = FALSE)
  return(list(x=modeldata, y=y$y))
}

ex3.scaleXdata <- function(data) {

  scaledataCallback <- function(xcolumn) {
    return ((xcolumn - mean(xcolumn))/sd(xcolumn))
  }
  scaledData <- apply(data,2,scaledataCallback)
  
  return (scaledData)
} 

ex3.addInterceptToX <- function(data) {
  # function to add a column of ones to the x column in the dataset
  return(cbind(rep(1),data))
}

ex3.generateThetaAlt <- function(data) {
  # generated a theta based on the number columns a m*n, where m
  # equals the number of columns
  theta <- numeric(ncol(data))
  matrix(data=theta,nrow=length(theta),ncol = 1)
}



ex3.all <- function(alpha = 0.07,n.rounds=50) {
  par(pch=22, col="red") # plotting symbol and color
  par(mar=c(1,1,1,1))  # all plots on one page 
  ex3Data <- ex3.readData()
  ex3Data$x <- ex3.scaleXdata(ex3Data$x)
  ex3Data$x <- ex3.addInterceptToX(ex3Data$x)
  thetaData <- ex3.generateThetaAlt(ex3Data$x)

  
  initialThetas <- matrix(thetaData)
  costData <- c()
  for(index in 1:n.rounds) {
    
    output <- ex3.linearRegressWithCostCalculation(ex3Data$x, ex3Data$y, thetaData, alpha)
    if(index == 1) {
      initialThetas <-output$theta
    }
    thetaData <- output$theta
    costData <- append(costData,output$cost)
  }

  
  ## need to plot my cost 
  costMatrix <- matrix(costData)
  costMatrix <- cbind(costMatrix, 1:n.rounds)
  plot(x=costMatrix[,2],y=costMatrix[,1], type='l')
  
  #plot(x=ex2Data$x[,2],y=ex2Data$y, xlab = "Height in Meters", ylab = "Age in years")
  #lines(x=ex2Data$x[,2],y=ex2Data$x %*% thetaData)
  return (list(lastiter =output,thetas=output$theta,costdata=costData))
}

#
#I need
# to calculate the cost, and see it decreasing.
ex3.linearRegressWithCostCalculation <- function(xdata, target, inThetas, alpha) {
  tempThetas <- matrix(data = inThetas, nrow = nrow(inThetas), ncol = 1)  
  # calcaulte the cost for the original thetas


  
  linRegressionResult <- ex3.linearRegress(data = xdata, thetas = inThetas)
  linRegressionLessTarget <- linRegressionResult - target
  vectorToSum <-  linRegressionLessTarget ^ 2  
  summedVector <- sum(vectorToSum)
  OneOverTwoM <- 2*nrow(xdata)
  cost <- summedVector/OneOverTwoM

  for(thetaIndex in 1:nrow(inThetas)) {
    linRegressionResult <- ex3.linearRegress(data = xdata, thetas = inThetas)
    linRegressionLessTarget <- linRegressionResult - target
    
    vectorToSum <-  linRegressionLessTarget * xdata[,thetaIndex]
    tempThetas[thetaIndex] <- (inThetas[thetaIndex] - (alpha *(sum(vectorToSum)/nrow(xdata))))
  }
  inThetas <- tempThetas
  return(list(cost=cost, thetas=inThetas))
}

#
# The extrea step i need to do here, is in addition to calculating the thetas, I need
# to calculate the cost, and see it decreasing.
ex3.linearRegressWithGradientDescentSecondAttempt <- function(xdata, target, inThetas, alpha) {
  tempThetas <- matrix(data = inThetas, nrow = nrow(inThetas), ncol = 1)
  for(thetaIndex in 1:nrow(inThetas)) {
    linRegressionResult <- ex3.linearRegress(data = xdata, thetas = inThetas)
    linRegressionLessTarget <- linRegressionResult - target
    
    vectorToSum <-  linRegressionLessTarget * xdata[,thetaIndex]
    tempThetas[thetaIndex] <- (inThetas[thetaIndex] - (alpha *(sum(vectorToSum)/nrow(xdata))))
  }
  inThetas <- tempThetas
}
ex3.linearRegress <- function(data, thetas) {
  # don't believe it
  # think the correct answer is to use sweep # todo try with sweep
  data %*% thetas
}
