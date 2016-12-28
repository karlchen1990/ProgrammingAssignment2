## functions to compute a matrix's inverse with cache functionality
## 

## function to create a makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
  x_prime<-NULL
  set_matrix<-function(y){
    x<<-y
    x_prime<<-NULL
  }
  get_matrix<-function() x
  set_inverse<-function(inv) x_prime<<-inv
  get_inverse<-function() x_prime
  list(x=x,x_prime=x_prime,get_matrix=get_matrix,set_inverse=set_inverse,
       get_inverse=get_inverse)
}


## calculate matrix inverse using func solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv<-x$get_inverse()
  if(!is.null(x_inv))
  {
    message("getting cached data")
    return(x_inv)
  }
  x_inv<-x$get_matrix()
  x_inv<-solve(x_inv)
  x$set_inverse(x_inv)
  x_inv
}
