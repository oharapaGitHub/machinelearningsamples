# read the mnist dataset

load_image_file <- function(filename) {
  ret = list()
  # rb = read binary (to the best of my abilities)
  f = file(filename,'rb')
  # this is saying read one column?  n = 1, the number of items to read ?
  # the size states the number of bytes to read, and this is important,
  # an integer is 4 bytes, and unsigned byte is 1
  readBin(f,'integer',n=1,size=4,endian='big')
  ret$n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  ret
}

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