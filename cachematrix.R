
makeCacheMatrix <- function(x = matrix()){
  
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  getInv <- function() return(inverse)
  
  setInv <- function(q) inverse <<- q
  
  list(set = set, get = get, setInv = setInv,
       getInv = getInv)
  
}

cacheSolve <- function(x){
  
  inv <- x$getInv()
  
  if(!is.null(inv)){
    message("getting cashed data")
    return(inv)
  }
  
  else{
    
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
    
  }
  
}