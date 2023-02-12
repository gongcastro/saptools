#' Transform months to years and months
#'
#' @param x Age in months
#' @param .sep Separator between years and months, ';' by default
#' @export
months_to_years <- function(x, .sep = ";") {
  glue(floor(x %/% 12),
    floor(x %% 12),
    .sep = .sep
  )
}


#' Cut age variable into age bins
#'
#' @param x Numeric vector with ages in months
#' @export
cut_age <- function(x) {
  y <- cut(x, breaks = seq(9, 35, 2), labels = seq(10, 34, 2))
  y <- as.integer(as.character(y))
  return(y)
}

#' Adjusted estimated proportion of successes from Gelman, Hill & Vehtari (2020)
#'
#' @param y Number of successes
#' @param n Number of trials
#' @export
prop_adj <- function(y, n) {
  (y + 2) / (n + 4)
}

#' Standard error of the adjusted proportion of successes from Gelman, Hill & Vehtari (2020)
#'
#' @param y Number of successes
#' @param n Number of trials
#' @export
prop_adj_se <- function(y, n) {
  prop <- prop_adj(y, n)
  sqrt(prop * (1 - prop) / (n + 4))
}

#' Confidence interval of the adjusted proportion of successes from Gelman, Hill & Vehtari (2020)
#'
#' @param y Number of successes
#' @param n Number of trials
#' @param .width Width of the confidence interval (0.95 by default)
#' @export
prop_adj_ci <- function(y, n, .width = 0.95, limit) {
  prop <- (y + 2) / (n + 4)
  se <- sqrt(prop * (1 - prop) / (n + 4))
  ci <-
    prop + qnorm(c((1 - .width) / 2, (1 - (1 - .width) / 2))) * se
  ci[1] <- ifelse(ci[1] < 0, 0, ci[1]) # truncate at 0
  ci[2] <- ifelse(ci[2] > 1, 1, ci[2]) # truncate at 1

  if (limit == ".lower") {
    return(ci[1])
  }
  if (limit == ".upper") {
    return(ci[2])
  }
}
