library(boot)
library(matlib) # For graphics of inequalities

a<-c(150,800) # Objective function's Coefs. 

#A1<-rbind(c(400, 2000),c(0.7, -0.3)) # m1*n coefs (lhs) <= constraints
A1<-matrix(c(400, 2000, 0.7, -0.3),nrow = 2, ncol=2, byrow=TRUE) # m1*n coefs (lhs) <= constraints
b1 <- c(25000,0) # Vector m1 rhs constraints.

A2 <- NULL # m2*n coefs (lhs) >= constraints
b2 <- NULL# Vector m2 rhs constraints.

adv_alloc_simplex_res <- simplex(a=a,A1=A1,b1=b1, A2=A2, b2=b2, maxi="TRUE") # Maximize Objective.
adv_alloc_simplex_res # Explained results

plotEqn(A1, b1, labels = TRUE, solution = TRUE) # Showing output graphically
