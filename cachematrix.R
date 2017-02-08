## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The two following functions implement a mechanism to cache the inverse of a
## matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix2get = matrix()) {
        inverse2get <- NULL;
        set <- function(matrix2set) {
                matrix2get <<- matrix2set;
                inverse2get <<- NULL;
        }
        get <- function() {
                matrix2get;
        };
        setinverse <- function(inverse2set) {
                inverse2get <<- inverse2set;
        };
        getinverse <- function() {
                inverse2get;
        };
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse);
};

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(cachematrix, ...) {
        ## Return a matrix that is the inverse of 'cachematrix'
        inv <- cachematrix$getinverse();
        if(!is.null(inv)) {
                message("getting cached data");
                return(inv);
        };
        matrix2solve <- cachematrix$get();
        inv <- solve(matrix2solve, ...);
        cachematrix$setinverse(inv);
        inv;
};
