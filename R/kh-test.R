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
  if (length(score1) != length(score2) | length(score1) != length(performace)) {
    stop("Lengths of scores and performance must be equal.")
  }

  dt <- data.table(s1 = score1,
                   s2 = score2,
                   y = performance) %>%
    .[!is.na(s1) & !is.na(s2) & !is.na(y)] %>%
    melt(id.vars = 'y', variable.name = 's', value.name = 'score')

  if (nrows(dt) < length(s1))
    stop('Missing values found in data supplied to kh_test.')

  score_cor <- cor(dt$s1, dt$s2, use = 'pairwise.complete.obs')

  ks_dt <- dt[, .(ks = run_ks(score, y)), by = 's']

  temp <- dt[, .(xbar = mean(score),
                 sdev = sd(score)), by = c('s', 'y')]

  a1 <- (temp[s == 's1' & y == 1, xbar] - temp[s == 's1' & y == 0, xbar]) / temp[s == 's1' & y == 1, sd]
  a2 <- (temp[s == 's2' & y == 1, xbar] - temp[s == 's2' & y == 0, xbar]) / temp[s == 's2' & y == 1, sd]
  b1 <- temp[s == 's1' & y == 0, sd] / temp[s == 's1' & y == 1, sd]
  b2 <- temp[s == 's2' & y == 0, sd] / temp[s == 's2' & y == 1, sd]
  a <- (a1 + a2) / 2
  b <- (b1 + b2) / 2

  results <- purrr::map_dbl(c(1:draws),
                            simulate,
                            n = dt[y == 1, .N],
                            m = dt[y == 0, .N],
                            correlation = score_cor,
                            mu = a / b,
                            sigma = 1 / b)

  ret_val <- list()
  class(ret_val) <- 'kh_result'
  ret_val$ks1 <- ks_dt[s == 's1', ks]
  ret_val$ks2 <- ks_dt[s == 's2', ks]
  ret_val$pval <- mean(results > abs(ret_val$ks1 - ret_val$ks2))

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

  abs(run_ks(a[, 1], b[, 1]) - run_ks(a[, 2], b[, 2]))
}

run_ks <- function(s, y) {
  ks.test(s[y == 1], s[y == 0])[[1]]
}
