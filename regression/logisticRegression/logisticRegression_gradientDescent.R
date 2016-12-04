###
# this script works through exercise one and four, which work on classification problems using
# gradient descent.
###

# clear the workspace
rm(list = ls())

# source of the lession
# - 
ex.readData <- function() {
  wd <- getwd()
  modeldataMatrix <- matrix(unlist(read.table(paste(wd,"/data/ex4_data/ex4x.dat",sep=""), header = FALSE)), ncol = 2, byrow = FALSE)
  y <- read.table(paste(wd,"/data/ex4_data/ex4y.dat",sep=""), col.names=c("y"), header = FALSE)
  return(list(x=modeldataMatrix, y=y$y))
}

ex.scaleXdata <- function(data) {
  
  scaledataCallback <- function(xcolumn) {
    return ((xcolumn - mean(xcolumn))/sd(xcolumn))
  }
  scaledData <- apply(data,2,scaledataCallback)
  
  return (scaledData)
} 

ex.addInterceptToX <- function(data) {
  # function to add a column of ones to the x column in the dataset
  return(cbind(rep(1),data))
}

ex.generateThetaAlt <- function(data) {
  # generated a theta based on the number columns a m*n, where m
  # equals the number of columns
  theta <- numeric(ncol(data))
  matrix(data=theta,nrow=length(theta),ncol = 1)
}



ex.all <- function(alpha = 0.0002,n.rounds=100) {
  par(pch=22, col="red") # plotting symbol and color
  par(mar=c(1,1,1,1))  # all plots on one page 
  exData <- ex.readData()
  thetaData <- ex.generateThetaAlt(exData$x)
  
  
  initialThetas <- matrix(thetaData)
  costData <- c()
  for(index in 1:n.rounds) {
    
    output <- ex.linearRegressWithCostCalculation(exData$x, exData$y, initialThetas, alpha)
    if(index == 1) {
      initialThetas <-output$theta
    }
    initialThetas <- output$theta
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
ex.linearRegressWithCostCalculation <- function(xdata, target, inThetas, alpha) {
 
  # calcaulte the cost for the original thetas
  # remember the expression is 
  #' y * log(htheta) + [1-y]log(1-hteta)
  logRegressResult <- ex.logisticRegressHyphothesisFunction(z=ex.linearRegress, data = xdata, thetas = inThetas)
  typeLg <- typeof(logRegressResult)
  #print(paste('log result,', logRegressResult))
  #print(paste('Type,', typeLg))
  logRegressResultLess1 <- 1 - logRegressResult
  
  postivie <- (target *log(logRegressResult))
  #print(paste('Pos', postivie))
  negative <- ((1-target) * log(logRegressResultLess1))
 # print(paste('Neg ', negative))
  resultToSum <- postivie + negative
  summedTotal <- sum(resultToSum)
  OneOverM <- nrow(xdata)  
  cost <- summedTotal/-OneOverM
  

  optimisedThetas <- ex.logisticRegressOptimisationFunction(xdata, inThetas, target, alpha)
  return(list(cost=cost, thetas=optimisedThetas))
}




ex.logisticRegressOptimisationFunction <- function(data, inThetas,target,alpha) {
  
  # how to fit the parameters for theta using the cost function
  # it is supervised learning
  # so how do we fit the parameters theat, one way is to try
  # use squared error? non, there is a non-convex function.
  # and manay local optimum, so have log cost function
  # and we will use gradient descent to do it
  tempThetas <- matrix(data = inThetas, nrow = nrow(inThetas), ncol = 1)
  for(thetaIndex in 1:nrow(inThetas)) {
    linRegressionResult <- ex.logisticRegressHyphothesisFunction(z=ex.linearRegress, data = data, thetas = inThetas)
    linRegressionLessTarget <- linRegressionResult - target
    
    vectorToSum <-  linRegressionLessTarget * data[,thetaIndex]
    tempThetas[thetaIndex] <- (inThetas[thetaIndex] - (alpha *(sum(vectorToSum)/nrow(data))))
  }
  inThetas <- tempThetas
}

ex.logisticRegressHyphothesisFunction <- function(z, data, thetas) {
  
  # this is 1/1+e^-z
  result <- 1/(1+exp(-(z(data,thetas))))

}
ex.linearRegress <- function(data, thetas) {
  # don't believe it
  # think the correct answer is to use sweep # todo try with sweep
  data %*% thetas
}

ex.newtonsMethod <- function() {
  
}