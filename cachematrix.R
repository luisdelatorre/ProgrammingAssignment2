# This is de first part of Assigment 2 (R Programming Coursea)
## This function creates a verctor > It is given by Coursera team

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Define a sq matrix (2 x 2)
x <- matrix(rnorm(4), nrow = 2) 

# Inverse of matrix 
## This function allows to inverse any matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
## Setter the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }

##  Call the new matrix function
get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
## Return the matrix with new functions
list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Print and solve de matriz given as example
print(x)
solve(x)

# This is the second part of assigment
## Function given by Cousera team
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Compute the inverse of the matrix. If the inverse is already 
## calculated before, it returns the cached inverse
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## To solve the cache
## If the inversed matriz is returned 
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }

## If the inverse is not calculated
  data <- x$get()
  
## Invertion
s <- solve(data, ...)
x$setsolve(s)
  s
}
