## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The two following functions implement a mechanism to cache the inverse of a
## matrix.

## 'makeCacheMatrix' creates a special "matrix" object that can cache its
## inverse.

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

## 'cacheSolve' computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the
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

testAndBenchmark <- function(matdim = 1500) {
        mat <- matrix(0, matdim, matdim);

        ## Creating a lower-triangular matrix 'mat' starting from a 0 matrix of
        ## dimension 'matdim'.

        mat <- matrix(0, matdim, matdim);
        mat[row(mat) + col(mat) >= matdim + 1] <- (row(mat) + col(mat) -11)[row(mat) + col(mat) >= matdim + 1] * -1;
        testcachemat <- makeCacheMatrix(mat);

        ## Benchmarking 'cacheSolve' against 'solve'. If everything works as
        ## expected the second execution of 'cacheSolve' should take far shorter
        ## time than the one of 'solve'. The higher 'matdim', the more evident
        ## the difference between the two should be (try with a 'matdim' around
        ## 1000).

        print("first execution of 'solve'");
        print(system.time(solve(testcachemat$get())));

        print("first execution of 'cacheSolve'");
        print(system.time(cacheSolve(testcachemat)));

        print("second execution of 'solve'");
        print(system.time(solve(testcachemat$get())));

        print("second execution of 'cacheSolve'");
        print(system.time(cacheSolve(testcachemat)));

        ## Testing the accuracy of 'cacheSolve'. If everything works as expected
        ## there should be no difference between the solution calculated by
        ## 'solve' ad the one calculated by ''cacheSolve'; in that case 'sum'
        ## should return 0.

        print("difference in the solutions calculated by 'solve' and 'cacheSolve'");
        print(sum(solve(testcachemat$get()) - cacheSolve(testcachemat)));
};
