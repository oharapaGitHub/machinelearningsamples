# read the mnist dataset
readImageData <- function(location) {
  to.read <- file(location, "rb")
  
  # read the magic number
  readBin(to.read, integer(), n=1, size=4,endian = "big")
  # this is a single guy
  numberOfImages <- readBin(to.read, integer(), n=1, size=4,endian = "big")
  numberOfRows <- readBin(to.read, integer(), n=1, size=4,endian = "big")
  numberOfColumn <- readBin(to.read, integer(), n=1, size=4,endian = "big")
  
  rowSize <- numberOfImages*numberOfRows*numberOfColumn
  imageItems <- readBin(to.read, integer(), size = 1, n=rowSize, signed=F, endian = "big")
  imageMatrix <- matrix(imageItems, ncol=numberOfRows*numberOfColumn, byrow=TRUE)
  close(to.read)
  return(imageMatrix)
}

readLabelData <- function(location) {
  # needed to know the types of the byte data in the file, can be obtained form
  # http://yann.lecun.com/exdb/mnist/
  to.read <- file(location, "rb")
  
  # read the magic number
  readBin(to.read, integer(), n=1, size=4,endian = "big")
  # this is a single guy
  numberOfItems <- readBin(to.read, integer(), n=1, size=4,endian = "big")
  rowSize <- numberOfItems
  imageItems <- readBin(to.read, integer(), size = 1, n=rowSize, signed=F, endian = "big")
  imageMatrix <- matrix(imageItems, ncol=1)
  close(to.read)
  return(imageMatrix)
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}