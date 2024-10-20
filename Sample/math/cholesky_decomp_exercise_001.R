## Some math stuff, not necessarily great, just to implement from scratch some algorithm here...
## Also, not so useful in concept... But fair enough, it's just an example.

A <- matrix(c(6, 15, 55, 15, 55, 225, 55, 225, 979), nrow=3)

my_chol <- function(mat_A) {
    ## So by now I know, I should check for symmetry, definite positive properties...
    ## Skipping for here.
    mat_L <- matrix(rep(0, length(mat_A)), nrow=nrow(mat_A))
    for(k in 1:nrow(mat_A)) {
        t_sum <- 0
        if(k > 1) {
            for(i in 1:(k-1)) {
                t_sum <- 0
                if(i > 1) {
                    for(j in 1:(i-1)) {
                        t_sum <- t_sum + mat_L[i,j]*mat_L[k, j]
                    }
                }
                mat_L[k, i] <- (mat_A[k, i] - t_sum) / mat_L[i, i]
            }
            t_sum <- 0
            for(j in 1:(k-1)) {
                t_sum <- t_sum + mat_L[k, j]^2
            }
        }
        mat_L[k , k] <- sqrt(mat_A[k , k] - t_sum)
    }
    mat_L
}

all.equal(my_chol(A)%*%t(my_chol(A)), A)
