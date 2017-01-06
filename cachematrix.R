## this solution contains two high level functions
## the first fucntion is used to set and retieve the orginal and inverse matrixes
## the second function is used to check for cached IM values and generate fresh IM values if cache is null


## this function contain contains a list of four functions that;
## set the orignal matirx (setOM)
## Return the original matrix (getOM)
## stores the inverse matrix passed to it (setIM)
## returns the inverse matrix (getIM)
makeCacheMatrix <- function(x = matrix()) 
{
      IM <- NULL
      setOM <- function(y = matrix())
      {
            x <<- y
            IM <<- NULL
      }
      getOM <- function() 
      {
            x
      }
      setIM <- function(inverse_matrix) 
      {
            IM <<- inverse_matrix
      }
      getIM <- function()
      {
            IM
      }
      list(setOM = setOM, getOM = getOM, setIM = setIM, getIM = getIM)
}

## this function takes an input of makeCacheMatrix, checks to see if the inverse matrix (IM) is null
## if null, to uses solve to invert it, if not null, it returns the current IM as the cached value
cacheSolve <- function(x, ...) 
{
      IM <- x$getIM()
      if(!is.null(IM))
      {
            message("Matrix already cached, returning cached inverse")
            return(IM)
      }
      OM <- x$getOM()
      IM <- solve(OM)
      x$setIM(IM)
      IM
}


