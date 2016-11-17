
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
ex2.generateTheta <- function(data) {
  theta0 <- numeric(nrow(data[[1]]))
  theta1 <- numeric(nrow(data[[1]]))
  
  return(matrix(c(theta0,theta1), nrow=nrow(data[[1]]), ncol=2, byrow = FALSE))
}

ex2.getGradientDescentData <- function() {
  
  # read the data
  ex2Data <- ex2.readData()
  ex2.plot(ex2Data)
  ex2Data[[1]] <- ex2.addInterceptToX(ex2Data[[1]])
  thetaData <- ex2.generateTheta(ex2Data)
  return(ex2Data)
}
ex2.all <- function() {
  ex2Data <- ex2.readData()
  ex2.plot(ex2Data)
  ex2Data[[1]] <- ex2.addInterceptToX(ex2Data[[1]])
  ex2Data$thetas <- ex2.generateTheta(ex2Data)
  return(ex2Data)
}

ex2.linearRegress <- function(data) {
  # don't believe it
  # think the correct answer is to use sweep # todo try with sweep
  resullt <- sum(data$x %*% c(0,0) - data$y)
  ##return ((data$x %*% data$thetas)) - data$y
  return(resullt)
}

ex2.linearRegressWithGradientDescent <- function(data, target, thetas) {
  
  m <- length(data$X0)
  
  ## this needs to be row access
  linearRegression <- function(input) {
    ## this will do it all but... I need to take y away, and muliply by the xrelated to theta
    ## this the
    return(sum(input$data * input$theta))
  }
  
  ###
  # in this attemptI will look to create a dataframe where each column is a list
  # with the required data.
  # I will then try 'apply' an expression to do what I need'
  ###
  secondAttemptAtGradientDescent <- function (data,thetas,target, thetaindex) {
    
    #
    # general outline of the formula to apply is
    # algorithm, minus the target, times the x for the theta in particualr
    # data frame will be
    # all x values, the y value, then another row of the x[i] the theta is in relation to
    # 
    # step 1, first get list of all x values
    # step 2, get lsit of y
    # step 3, get list of x related to theta
    # step 4 combine
    
    indexNames <- names(data$V1)
    thetaNames <- names(thetas)
    dataout <- c()
    thetaout <- c()
    #rows - for each item of data
    for(i in 1:length(data$V1$x0)) {
      
      targety <- target[i]
      # columns - apply the learning algorithm
      for(j in 1:length(data$V1)) {
        
        dataRef <- indexNames[j]
        thetaRef <- thetaNames[j]
        dataout <- append(dataout,unlist(data$V1[dataRef])[i])
        thetaout <- append(thetaout,unlist(thetas[thetaRef])[i])    
      }
      # at this point I am ready to call linear regression, and finish the algorithm
      # then just reset
      summedLinear <- linearRegression(list("data"=dataout,"theta"=thetaout))
      
      ## do the minus 
      linMinusTarget <- summedLinear - targety
      
      ## now it is multiply by x theta index is related to
      
      
      ## reset 
      dataout <- c()
      thetaout <- c()
      
    }
    return (list("data"=dataout,"theta"=thetaout))
  }
  
  
  #####
  # the start of my expression, all needs to be defined. 
  # the I need to get the deriviative of it.
  # the below formulas are the partial derviate 
  # thetaZeroExp <- expression((1/m) * (sum(linearRegression(data, thetas)-target)))
  
  
  resolveGradientDescentExpression <- function (data,thetas,target, thetaindex) {
    indexNames <- names(data$V1)
    thetaNames <- names(thetas)
    dataout <- c()
    thetaout <- c()
    #rows - for each item of data
    for(i in 1:length(data$V1$x0)) {
      
      targety <- target[i]
      # columns - apply the learning algorithm
      for(j in 1:length(data$V1)) {
        
        dataRef <- indexNames[j]
        thetaRef <- thetaNames[j]
        dataout <- append(dataout,unlist(data$V1[dataRef])[i])
        thetaout <- append(thetaout,unlist(thetas[thetaRef])[i])    
      }
      # at this point I am ready to call linear regression, and finish the algorithm
      # then just reset
      summedLinear <- linearRegression(list("data"=dataout,"theta"=thetaout))
      
      ## do the minus 
      linMinusTarget <- summedLinear - targety
      
      ## now it is multiply by x theta index is related to


      ## reset 
      dataout <- c()
      thetaout <- c()

    }
    return (list("data"=dataout,"theta"=thetaout))
  }
  
  
  unlist(target[2]$V1)
 # thetaJExp <- expression((1/m) * ( sum((linearRegression(data, thetas)-target)*data[thetaindex])) )
  
  #thetaZero <- expression((1/length(x)) * (sum(costf(x)-y)))
  #thetaOne <- expression((1/length(x)) * (sum((costf(x)-y)*x)))
  # lets work this out, I want the value to stop changing.  So lets say I start at 

  alpha <- 0.2
  # do for 15 rounds
  doit <- function() {
    print(thetaZero)
    print(thetaOne) 
    for (i in c(1:50)) {
      tempValeZero <- eval(thetaZeroExp)
      tempValeOne <- eval(thetaOneExp)
      preThetaZero <- thetaZero - (alpha * tempValeZero)
      preThetaOne <- thetaOne - (alpha * tempValeOne)
      thetaZero <- preThetaZero
      thetaOne <- preThetaOne
      print(thetaZero)  
      print(thetaOne)  
    }
    return (c(thetaZero, thetaOne))
  }
}

# costfunction 
# going to use a fixed learning rate
## my expression 1 over m times sum
htheta <- expression(1 + 0:19)

n<-3
#x <- seq(-4, 4, len = 101)

#demo(plotmath)

x <- c(2.1,3.8,6.5)
y <- c(2,4,6)
costf <- function(item) {
  return(item + 1)
}
thetaZero <-0
thetaOne <- 0



doit()

eval(thetaZero)
eval(thetaOne)

exp3 <- expression(sum(x[i], i==1, n))

eval(exp2)

sum(x[i], i==1,3)

eval(exp2)

?sum

x <- 4
y <- x* 10

z <- quote(y <- x* 10)
z
library(pryr)
ast(y <- x * 10)
