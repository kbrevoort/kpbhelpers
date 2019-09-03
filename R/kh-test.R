#' Krzanowski and Hand Test
#'
#' Implements the test outlined in Krzanowski and Hand (2011). The test is used
#' for credit scoring models to test the significance of the difference between
#' the KS statistics of two alternative credit scores using the same evaluation
#' sample.
#' @param score1 First credit score
#' @param score2 Second credit score
#' @param performance A zero-one variable designating whether the performace was
#' "good" or "bad"
#' @param draws Number of Monte-Carlo draws to use (default = 10,000)
#' @param na.rm Should NA values be removed?  (default = FALSE).
#' @import data.table
#' @importFrom purrr map_dbl
#' @export
kh_test <- function(score1, score2, performance, draws = 10000L, na.rm = FALSE) {
  # Check dimensions of input variables
  if (length(score1) != length(score2) | length(score1) != length(performance)) {
    stop("Lengths of scores and performance must be equal.")
  }

  dt <- data.frame(s1 = score1,
                   s2 = score2,
                   y = performance)
  dt <- na.omit(dt)

  if (nrow(dt) < length(score1))
    stop('Missing values found in data supplied to kh_test.')

  score_cor <- cor(dt$s1, dt$s2, use = 'pairwise.complete.obs')

  ks_s1 <- ks.test(dt$s1[dt$y == 1], dt$s1[dt$y == 0])[[1]]
  ks_s2 <- ks.test(dt$s2[dt$y == 1], dt$s2[dt$y == 0])[[1]]

  # Calculate mean scores by performance
  sbar_11 <- mean(dt$s1[dt$y == 1])
  sbar_10 <- mean(dt$s1[dt$y == 0])
  sbar_21 <- mean(dt$s2[dt$y == 1])
  sbar_20 <- mean(dt$s2[dt$y == 0])

  ssig_11 <- sd(dt$s1[dt$y == 1])
  ssig_10 <- sd(dt$s1[dt$y == 0])
  ssig_21 <- sd(dt$s2[dt$y == 1])
  ssig_20 <- sd(dt$s2[dt$y == 0])

  a <- ((sbar_11 - sbar_10) / ssig_11) + ((sbar_21 - sbar_20) / ssig_21) / 2
  b <- ((ssig_10 / ssig_11) + (ssig_20 / ssig_21)) / 2

  results <- vapply(X = c(1:draws),
                    FUN = simulate,
                    FUN.VALUE = 3.2,
                    n = sum(dt$y == 1),
                    m = sum(dt$y == 0),
                    correlation = score_cor,
                    mu = a / b,
                    sigma = 1 / b)

  ret_val <- list()
  class(ret_val) <- 'kh_result'
  ret_val$ks1 <- ks_s1
  ret_val$ks2 <- ks_s2
  ret_val$pval <- mean(results > abs(ks_s1 - ks_s2))

  ret_val
}

#' Simulate a Draw for the Krzanowski and Hand Test
#'
#' Do a single draw for the Krzanoski and Hand test.
#' @param i Integer value supplied by purrr.  Does not do anything
#' @param n Number of "goods"
#' @param m Number of "bads"
#' @param correlation Correlation between scores
#' @param mu Mean of scores (normalized)
#' @param sd Mean of sigma
#' @return A scalar value showing the difference in KS statstics
simulate <- function(i, n, m, correlation, mu, sigma) {
  a <- rmvnorm(n, correlation)
  b <- rmvnorm(m, correlation, mu, sigma)

  abs(ks.test(a[, 1], b[, 1])[[1]] - ks.test(a[, 2], b[, 2])[[1]])
}


