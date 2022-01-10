##In this function we define the getters and setters for the class and how to store data in cache

makeCacheMatrix <- function(x = matrix(x)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function() m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##In this function we retrieve the inverse of the matrix but taking into accunt if has been calculated before or not
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    print("Getting cached data...")
    return(m)
  }else{
  print("Setting new cached data...")
  data <- x$get()
  
  x$setinv()
  x$getinv()
  }
}

##Testing the methods

a1 <- c(3, 2, 5)
a2 <- c(2, 3, 2)
a3 <- c(5, 2, 2)

A <- rbind(a1, a2, a3)
mtrx <- makeCacheMatrix(A)
cacheSolve(mtrx)
