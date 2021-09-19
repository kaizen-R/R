# Basic stuff: Comparing speeds to different way to re-cateorize factors
library(microbenchmark)
library(plyr)
library(dplyr)
library(testthat)

test1 <- data.frame(num = c(1:99),
                    cat = factor(rep(c("A", "B", "C"), 33)))

f1 <- function(df) { # base R basic way
  df$new_cat <- ifelse(test1$cat == "C", "cat2", "cat1")
  df$new_cat <- as.factor(df$new_cat)
  df[, c(1,3)]
}
f2 <- function(df) { # dplyr way
  df <- df |> mutate(new_cat = ifelse(cat == "C", "cat2", "cat1"))
  df$new_cat <- as.factor(df$new_cat)
  df |> select(num, new_cat)
}
f3 <- function(df) { # plyr way
  df$new_cat <- mapvalues(df$cat, from = c("A", "B", "C"), to = c("cat1", "cat1", "cat2"))
  df[, c(1,3)]
}
f4 <- function(df) { # base R working on factor levels
  levels(df$cat)[df$cat %in% c("A", "B")] <- "cat1"
  levels(df$cat)[levels(df$cat) == "C"] <- "cat2"
  names(df)[2] <- "new_cat"
  df
}

test_that("Validate equal functionality", {
  expect_equal(f1(test1), f2(test1))
  expect_equal(f1(test1), f3(test1))
  expect_equal(f1(test1), f4(test1))
})

microbenchmark(
  f1(test1), f2(test1), f3(test1),
  f4(test1),
  times = 100L)


