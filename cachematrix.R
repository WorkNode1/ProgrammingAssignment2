## Create and maintain a matrix object that can cache its inverse
## Caching matrix object can be manipulated by four methods:

## 1) set    - Accepts matrix, returns inverse-caching matrix object
## 2) get    - Retrieves already-existing matrix object
## 3) setinv - Accepts already-calculated inverse and caches it inside object
## 4) getinv - If inverse already cached, returns inverse, else returns NULL

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(localinv) inv <<- localinv
  getinv <- function() inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
##   If inverse matrix already cached, return cached inverse matrix.
##   If inverse matrix not already cached, calculate matrix inverse
##   cache this inverse and return inverse matrix.

cacheSolve <- function(x, ...) {

  inverse <- x$getinv()            # Check if cached inverse exists
  
  if(!is.null(inverse)) {          # If cached inverse exists...
    message("getting cached data")
    return(inverse)                # ...return cached copy. Don't recalculate
  }
  
  data <- x$get()                  # Otherwise, retrieve original matrix
  inverse <- solve(data)           # Calculate inverse matrix
  x$setinv(inverse)                # cache calculated inverse in matrix object
  return(inverse)                  # Return calculated (and newly-cached) inverse
}

## Test run
# testmatrix <- rbind(c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29),
#                     c(31, 37, 41, 43, 47, 53, 59, 61, 67, 71),
#                     c(73, 79, 83, 89, 97, 101, 103, 107, 109, 113),
#                     c(127, 131, 137, 139, 149, 151, 157, 163, 167, 173),
#                     c(179, 181, 191, 193, 197, 199, 211, 223, 227, 229),
#                     c(233, 239, 241, 251, 257, 263, 269, 271, 277, 281),
#                     c(283, 293, 307, 311, 313, 317, 331, 337, 347, 349),
#                     c(353, 359, 367, 373, 379, 383, 389, 397, 401, 409),
#                     c(419, 421, 431, 433, 439, 443, 449, 457, 461, 463),
#                     c(467, 479, 487, 491, 499, 503, 509, 521, 523, 541))
# testmatrix
## [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,]    2    3    5    7   11   13   17   19   23    29
## [2,]   31   37   41   43   47   53   59   61   67    71
## [3,]   73   79   83   89   97  101  103  107  109   113
## [4,]  127  131  137  139  149  151  157  163  167   173
## [5,]  179  181  191  193  197  199  211  223  227   229
## [6,]  233  239  241  251  257  263  269  271  277   281
## [7,]  283  293  307  311  313  317  331  337  347   349
## [8,]  353  359  367  373  379  383  389  397  401   409
## [9,]  419  421  431  433  439  443  449  457  461   463
## [10,]  467  479  487  491  499  503  509  521  523   541
# mycachingmatrix <- makeCacheMatrix(testmatrix)     # Populate matrix object with matrix
# timeNoCache <- system.time(newinv <- cacheSolve(mycachingmatrix)) # Calculate matrix inverse and time it
# timeNoCache
## user  system elapsed 
## 0       0       0 
# newinv
## [,1]         [,2]         [,3]       [,4]        [,5]
## [1,]  0.851780337 -0.494295886  0.505704114 -0.5622538  0.06311822
## [2,] -2.256540460  1.392640283 -1.607359717  1.5828157 -0.10695745
## [3,]  1.114940464 -0.652334658  0.847665342 -0.7847430  0.01171258
## [4,] -0.000803291 -0.006987634 -0.006987634 -0.1359620  0.02939247
## [5,] -0.876025838  0.338255570 -0.661744430  0.8925584 -0.11489722
## [6,] -0.973219060  0.874782858 -0.625217142  0.5320641 -0.11449184
## [7,]  4.646183994 -3.124343584  3.375656416 -3.2776375  0.31882046
## [8,]  0.894105707 -0.651997667  0.848002333 -0.8106892  0.22146966
## [9,] -4.239446411  2.804277234 -3.195722766  3.1197232 -0.33594943
## [10,]  0.838594640 -0.481665881  0.518334119 -0.5556610  0.02695016
# timeWithCache <- system.time(newinv2 <- cacheSolve(mycachingmatrix)) # Retrieve already-calculated matrix inverse and time it
# timeWithCache
## user  system elapsed 
## 0       0       0 
# newinv2
## [,1]         [,2]         [,3]       [,4]        [,5]
## [1,]  0.851780337 -0.494295886  0.505704114 -0.5622538  0.06311822
## [2,] -2.256540460  1.392640283 -1.607359717  1.5828157 -0.10695745
## [3,]  1.114940464 -0.652334658  0.847665342 -0.7847430  0.01171258
## [4,] -0.000803291 -0.006987634 -0.006987634 -0.1359620  0.02939247
## [5,] -0.876025838  0.338255570 -0.661744430  0.8925584 -0.11489722
## [6,] -0.973219060  0.874782858 -0.625217142  0.5320641 -0.11449184
## [7,]  4.646183994 -3.124343584  3.375656416 -3.2776375  0.31882046
## [8,]  0.894105707 -0.651997667  0.848002333 -0.8106892  0.22146966
## [9,] -4.239446411  2.804277234 -3.195722766  3.1197232 -0.33594943
## [10,]  0.838594640 -0.481665881  0.518334119 -0.5556610  0.02695016
