# This is a set of functions that allow the creation and use of a cached matrix object
# where 'cache' refers to the ability to store the matrix inverse after it has been solved


# Args:
#   m - an invertible square matrix
# 
# Returns an instance of a CacheMatrix object with the following
# instance methods:
#   set_matrix - sets the matrix
#   get_matrix - gets the matrix
#   set_inverse - caches the inverse of the matrix
#   get_inverse - gets the cached inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
  inverse = NULL
  
  set_matrix  = function(new_m) { m <<- new_m; inverse <<- NULL }
  get_matrix  = function() { m }
  set_inverse = function(inverse) { inverse <<- inverse }
  get_inverse = function() { inverse }
  
  list(set_matrix  = set_matrix,
       get_matrix  = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# Args:
#   cache_matrix - an instance of a cacheMatrix object
#
# Returns the inverse of the matrix contained by cache_matrix

cacheSolve = function(cache_matrix) {
  inverse = cache_matrix$get_inverse()
  
  if(!is.null(inverse)) {
    return(inverse)
  }
  
  m = cache_matrix$get_matrix()
  inverse = solve(m)
  cache_matrix$set_inverse(inverse)
  
  inverse
}
