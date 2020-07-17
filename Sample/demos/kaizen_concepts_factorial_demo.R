##
# File: kaizen_concepts_factorial_demo.R
# Author: Nico.
# Last edited: 2020/07/17
#
# Objectives: Teaching "Kaizen-R" through one function
##

# Now a couple of months ago I had to explain how to program a "factorial" function to explain a 
# while loop.
# It was actually to help someone with a Python course, but that gave me the idea for this entry 
# here...
# ----
# If you use the following as definition:
# n! = n * (n-1)!
# where:
# n is an integer
# ! means "factorial"
# and 0! = 1
#
# How would you go about programming it?
# ----

# This script intends to see different versions/approaches of that same function.

# ----
# As the baseline, we'll use the native (base) "factorial" function.
# Let's see what we should expect, before we begin:
factorial(3)
factorial(5)
factorial(100) # BIG number this one... Relevant later on.
factorial()
factorial(3.5) # Beyond our definition, btw. This works using an integral function, see:
               # https://en.wikipedia.org/wiki/Gamma_function
factorial(-1)
factorial("Hello world!")
# Now if (as I did two months ago) we didn't know that this function existed,
# Or if we wanted to teach R programming through the example of calculating "n!"
# What could happen?

# ----

# Let's act as if we were new to R programming. Or new to programming, even.
# We might see results like what we'll call our V1 here:
# No comments, not really "R-like", little-to-no testing, spacing is all wrong, use of ";" is bad...
# ----
# Bad but functional
own_factorial_v1 <- function (x) {
  i=1
  while (x > 0) {
    i = i * x; x=x- 1;
    
  }
  return(i)
}
own_factorial_v1(3)
own_factorial_v1(5)

# Side note: Cool thing about R Studio, you can fix some uglyness above using the Menu option:
# Code -> Reformat Code.
# ----



# Make it v2: a bit better:
#  Code is commented
#  A bit more "R-Like" using "<-" instead of "="
#  variable names can be understood
#  We even add a default value to control empty calls
#  And we think a bit more through the tests to validate afterwards
#  But still plenty of room to improve

# function: own_factorial_v2 ----
# usage:
#  receives: numeric
#  returns: numeric, as calculated factorial of received number
own_factorial_v2 <- function(x = 0) {
  # variable to be finally returned
  result <- 1
  
  while (x > 0) {
    result <- result * x
    x <- x - 1
  }
  # In R, the last statement returns the value for the function.
  # This can be made explicit (I personally prefer it, in fact)
  result
}

# Tests
own_factorial_v2(0)
own_factorial_v2(-1)
own_factorial_v2(0)
own_factorial_v2(1)
own_factorial_v2(3)
own_factorial_v2(5)


# v3: The "algorithmic" version, using recursivity
#  Code is commented
#  A bit more "R-Like" using "<-" instead of "="
#  variable names can be understood
#  We even add a default value to control empty calls
#  We consider more fringe cases for the tests
#  But still plenty of room to improve


# function: own_factorial_v3 ----
# usage:
#  receives: numeric
#  returns: numeric, as calculated factorial of received number
own_factorial_v3 <- function(x = 0) {
  # value to be finally returned when we are down to 1
  if (x == 0)
    return(1)
  # Here I use no {} as it is short and one-liner
  # Also, no need for else.
  # Once again, R-like: No explicit return...
  x * own_factorial_v3(x - 1)
}

# Btw, the resulting factorial calculation using recursivity is quite elegant:
#
# own_factorial_v3 <- function(x = 0) {
#   if(x == 0) return(1)
#   x * own_factorial_v3(x-1)
# }

own_factorial_v3()
own_factorial_v3("Hello world!")
own_factorial_v3(-1)
own_factorial_v3(0)
own_factorial_v3(1)
own_factorial_v3(3)
own_factorial_v3(5)

# BTW, this bit here is cool:
own_factorial_v3(100)
own_factorial_v3(1000)
# We could have gone with integers (32 bits), but R by default goes for numeric.
# This is relevant! But not the goal of this exercise here...


# v4: Use the vectorization here
#  Oh, and one more test to check for...

# function: own_factorial_v4 ----
# usage:
#  receives: numeric
#  returns: numeric, as calculated factorial of received number
own_factorial_v4 <- function(x = 0) {
  # really "R-like", and 1-liner
  ifelse(x > 0, prod(1:x), 1)
}

own_factorial_v4()
own_factorial_v4("Hello world!")
own_factorial_v4(3)
own_factorial_v4(5)
own_factorial_v4(3.5)

# v5: v4, but with entry controls

# function: own_factorial_v5 ----
# usage:
#  receives: numeric
#  returns: numeric, as calculated factorial of received number
# ----
own_factorial_v5 <- function(x = 0) {
  # Check for invalid inputs:
  if (is.numeric(x)) {
    ifelse(x > 0, prod(1:x), 1)
  } else {
    print("Invalid input")
    return(0)
  }
} # End function own_factorial_v5 ----

# Normal Checks ----
own_factorial_v5(0)
own_factorial_v5(1)
own_factorial_v5(3)
own_factorial_v5(5)
own_factorial_v5(100)
# Special conditions ----
own_factorial_v5()
own_factorial_v5(3.5)
own_factorial_v5("Hello world!")
own_factorial_v5(10000)
# End tests v5 ----

## Enough for the versions.
# We could demo try-catch instead of a simple "if" statement, we could see limits of the R numeric 
# variables... But that's beyond this exercise. Let's just say there are more situations we could 
# test.
#
# Now what IS within our goals with "Kaizen-R" is to IMPROVE, not ONLY on style, but ALSO in terms 
# of efficiency.
#
# Let's do some comparative testing: we use microbenchmark as explained in "Efficient R"
# See: https://csgillespie.github.io/efficientR/introduction.html#prerequisites

# Also: Yes: Good practice would require us to load that at the beginning of the script.
# This is here for demo purposes; I'd have installed in another script, and sourced at the beginning
# Anyhow...
#install.packages("microbenchmark")
library(microbenchmark)

microbenchmark(
  factorial(100),
  own_factorial_v1(100),
  own_factorial_v2(100),
  own_factorial_v3(100),
  own_factorial_v4(100),
  own_factorial_v5(100)
)

# Now we can definitely say that native gamma(x+1) is much better than our code in terms of 
# execution times.
# Also, V4 and V5 are ~5 times faster than v1 and v2, and ~14 times faster than V3.

# Lesson here is:
# V1 and V2 are not taking advantage of native R way of things.
# V3, while very elegant, is not efficient in R.
# V4 and V5 are using a vectorized operation and although not quite as readable for the newcomer to 
# R, they are actually quite nice.

# But there is still one relevant detail: our code is "interpreted".
# Could we add a compiled version maybe, and see how that goes?

# R can work closely with C++, through the Rcpp package.
# A quick and dirty version of what could be done is offered here:

# Once again, that would be loaded at the beginning
#install.packages("Rcpp")
library(Rcpp)


cppFunction('
int own_factorial_vCPP(int x) {
  int i;
  int fact = 1;

  if(x==0) fact=1;
  else{
    for (i = 1; i <= x; i++){
      fact = fact * i;
    }
  }
  return(fact);
}
')

own_factorial_vCPP(5)
own_factorial_vCPP(10)

# Relevant note: This next bit won't work:
own_factorial_vCPP(100)
# INT_MAX, +2147483647, Defines the maximum value for an int.
#> factorial(100)
#[1] 9.332622e+157
own_factorial_v1(100)

microbenchmark(
  factorial(10),
  own_factorial_v1(10),
  own_factorial_v2(10),
  own_factorial_vCPP(10),
  own_factorial_v3(10),
  own_factorial_v4(10),
  own_factorial_v5(10)
)


# Finally, I made a point above to test different scenarios.
# That's just good practice.
# Now there is a package for that too, as I found out reading around.
# Organizing test cases, so that you can re-launch them in the future "quickly"
# seems like a good idea.
# Actually, in "XP" (for "eXtreme Programming"), one of the ideas is to:
# "Write the test before you write the function".

# You'll need this package:
# install.packages("testthat")
library(testthat)
test_that("testing different factorial evaluation functions", {
  # case empty variable
  expect_error(own_factorial_v1())
  expect_equal(own_factorial_v2(), 1)
  expect_equal(own_factorial_v3(), 1)
  expect_equal(own_factorial_v4(), 1)
  expect_equal(own_factorial_v5(), 1)
  expect_error(own_factorial_vCPP())
  
  # case negative value
  expect_equal(own_factorial_v1(-1), 1)
  expect_equal(own_factorial_v2(-1), 1)
  expect_error(own_factorial_v3(-1))
  expect_error(own_factorial_v4(-1))
  expect_error(own_factorial_v5(-1))
  expect_error(own_factorial_vCPP(-1))
  
  # case strings
  expect_error(own_factorial_v1("Hello"))
  expect_error(own_factorial_v2("Hello"))
  expect_error(own_factorial_v3("Hello"))
  expect_error(own_factorial_v4("Hello"))
  expect_error(own_factorial_v5("Hello"), 0)
  expect_error(own_factorial_vCPP("Hello"))
  
  # case non integers: Our approach might work better
  expect_equal(own_factorial_v1(3.5), 6)
  expect_equal(own_factorial_v1(3.5), 6)
  expect_equal(own_factorial_v1(3.5), 6)
  expect_equal(own_factorial_v1(3.5), 6)
  expect_equal(own_factorial_v1(3.5), 6)
  expect_error(own_factorial_vCPP(3.5))
  
  # case 0
  expect_equal(own_factorial_v1(0), 1)
  expect_equal(own_factorial_v2(0), 1)
  expect_equal(own_factorial_v3(0), 1)
  expect_equal(own_factorial_v4(0), 1)
  expect_equal(own_factorial_v5(0), 1)
  expect_equal(own_factorial_vCPP(0), 1)
  
  # case 5
  expect_equal(own_factorial_v1(5), factorial(5))
  expect_equal(own_factorial_v2(5), factorial(5))
  expect_equal(own_factorial_v3(5), factorial(5))
  expect_equal(own_factorial_v4(5), factorial(5))
  expect_equal(own_factorial_v5(5), factorial(5))
  expect_equal(own_factorial_vCPP(5), factorial(5))
  
  # case 100
  expect_equal(own_factorial_v1(100), factorial(100))
  expect_equal(own_factorial_v2(100), factorial(100))
  expect_equal(own_factorial_v3(100), factorial(100))
  expect_equal(own_factorial_v4(100), factorial(100))
  expect_equal(own_factorial_v5(100), factorial(100))
  expect_error(own_factorial_vCPP(100))
  
})

# And this is it!
# Of course there must be plenty more to test, demo, explain...
# But this one use-case seemed a good candidate to explain through example a few
# things about R programming ideas...
