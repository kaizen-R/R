library(magrittr)

# Playing the FizzBuzz kata, in R 4.0 (R 4.1 would give us more options)
microbenchmark::microbenchmark(
  # basic, I guess very common version:
  for(each in 1:100) {
    if(each %% 3 == 0 & each %% 5 == 0) print("FIZZBUZZ")
    else if(each %% 3 == 0) print("fizz")
    else if(each %% 5 == 0) print("buzz")
    else print(each)
  },
  # faster option: Base, vectorized:
  sapply(1:100, function(x) {
    if(x %% 3 + x %% 5 == 0) return("FIZZBUZZ") # Sum of the modulus == 0
    if(x %% 3 == 0) return("fizz")
    ifelse(x %% 5 == 0, "buzz", x)
  }),
  # Now note: LCP(3, 5) == 15... Math helps!
  # Maybe this is a bit more original? I found it cool
  unlist(lapply(1:100, function(x) {
    t <- c("FIZZBUZ", "fizz", "buzz")[x %% c(15, 3, 5) == 0][1]
    if(!is.na(t)) return(t)
    x
  })),
  # fastest so far, without going directly to Rcpp...
  1:100 %>% {ifelse(. %% 15 == 0, "FIZZBUZZ", 
                    (ifelse(. %% 3 == 0, "fizz",
                            ifelse(. %% 5 == 0, "buzz", .))))
  },
  # Finally, let's still think a bit out of the box: Use 2 dimensions and indexes?
  {
    t1 <- matrix(1:100, ncol=5)
    t1[which(row(t1) %% 5 == 0)] <- "buzz"
    t1 <- as.vector(t1)[1:100]
    t1 <- matrix(t1, nrow=3)
    t1[which(row(t1) %% 3 == 0)] <- "fizz"
    t1 <- as.vector(t1)[1:100]
    t1[which(1:100 %% 15 == 0)] <- "FIZZBUZZ"
    t1
  }
)
