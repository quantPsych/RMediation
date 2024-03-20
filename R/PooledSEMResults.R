#' Pooled SEM Analysis Results Class
#'
#' An S4 class to represent pooled results from SEM analysis across multiple imputations or datasets.
#' It contains pooled estimates, standard errors, test statistics, p-values, and confidence intervals
#' for each parameter estimated across multiple imputations.
#'
#' @slot tidy_table data.frame A data frame containing the pooled results of the SEM analyses. The column names adhere to tidy conventions and include the following columns:
#' - `term`: The name of the parameter being estimated.
#' - `estimate`: The pooled estimate of the parameter.
#' - `std.error`: The pooled standard error of the estimate.
#' - `p.value`: The pooled p-value for the test statistic.
#' - `conf.low`: The lower bound of the confidence interval for the estimate.
#' - `conf.high`: The upper bound of the confidence interval for the estimate.
#' @slot cov_total matrix The pooled total covariance matrix of the parameter estimates.
#' @slot cov_between matrix The pooled between-imputation covariance matrix of the parameter estimates.
#' @slot cov_within matrix The pooled within-imputation covariance matrix of the parameter estimates.
#' @slot method character The method used for SEM analysis ('lavaan' or 'OpenMx').
#' @slot conf.int logical Whether to calculate confidence intervals for the pooled estimates. default is `FALSE`.
#' @slot conf.level numeric The confidence level used in the interval calculation. default is `0.95`.
#'
#' @import methods
#' @importFrom methods setClass setValidity setMethod validObject
#' @exportClass PooledSEMResults
#' @name PooledSEMResults
#' @rdname PooledSEMResults-class
#' @docType class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @aliases PooledSEMResults PooledSEMResults-class

PooledSEMResults <- setClass(
  "PooledSEMResults",
  slots = c(
    tidy_table = "data.frame",
    cov_total = "matrix",
    cov_between = "matrix",
    cov_within = "matrix",
    method = "character",
    conf.int = "logical",
    conf.level = "numeric"
  )
)

setValidity("PooledSEMResults", function(object) {
  messages <- character(0)

  # Check if tidy_table is a non-empty data frame with required columns
  if (conf.int(object) && !is.logical(conf.int(object))) {
    messages <- c(messages, "conf.int must be a logical value.")
  }

  requiredColumns <- c("term", "estimate", "std.error", "p.value")

  requiredColumns <-  ifelse(object@conf.int,
                            c(requiredColumns, "conf.low", "conf.high"),
                            requiredColumns)

  if (!is.data.frame(object@tidy_table) ||
      nrow(object@tidy_table) == 0) {
    messages <-
      c(messages, "tidy_table must be a non-empty data frame.")
  } else if (!all(requiredColumns %in% colnames(object@tidy_table))) {
    missingCols <- setdiff(requiredColumns, colnames(object@tidy_table))
    messages <-
      c(messages,
        paste(
          "Missing required columns in tidy_table:",
          paste(missingCols, collapse = ", ")
        ))
  }

  # Check if cov_total, cov_between, and cov_within are positive definite symmetric matrices
  if (!is_pd(object@cov_total)) {
    messages <-
      c(messages,
        "cov_total must be a symmetric positive definite matrix.")
  }

  if (!is_pd(object@cov_between)) {
    messages <-
      c(messages,
        "cov_between must be a symmetric positive definite matrix.")
  }

  if (!is_pd(object@cov_within)) {
    messages <-
      c(messages,
        "cov_within must be a symmetric positive definite matrix.")
  }

  if (length(messages) > 0) {
    stop(paste(messages, collapse = "\n"))
  } else {
    TRUE
  }
})


### =============================================
### Methods for the PooledSEMResults Class
### ============================================
