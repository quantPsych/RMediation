#' SemImputedData Class
#'
#' An S4 class to hold multiply imputed datasets for structural equation modeling (SEM) analysis.
#' It facilitates working with imputed data from the `mice` package and supports SEM analysis
#' using either the `lavaan` or `OpenMx` packages.
#'
#' @slot data An object of class `mids` from the `mice` package, representing multiply imputed datasets.
#' @slot model A `lavaan` or `OpenMx` model syntax to be used for SEM analysis. For `lavaan` models, the syntax should be a character string as described in [lavaan::model.syntax]. For `OpenMx` models, the syntax should be an [mxModel] object with or without [mxData()] specified; that is, `mxModel` syntax can be without data specified. In addition, both `lavaan` and `OpenMx` models can be a fitted model object in the respective package.
#' @slot method A character string indicating the SEM package to be used for analysis. It is a derived slot from the `model` slot, and it is set automatically based on the class of the `model` slot. The possible values are "lavaan" or "OpenMx".
#' @slot conf.int A logical value indicating whether confidence intervals are
#'   included in the SEM results. Defaults to `FALSE`.
#' @slot conf.level A numeric value specifying the confidence level for
#'   confidence intervals, which must be between 0 and 1. Defaults to 0.95.
#' @slot original_data A derived (from mids object) slot to store the original data used to create the imputed datasets.
#' @slot n_imputations A derived (from mids object) slot to store the number of imputations used to create the imputed datasets.
#' @slot fit_model A slot to store the fitted SEM model object.
#' @exportClass SemImputedData
#' @name SemImputedData
#' @rdname SemImputedData
#' @aliases SemImputedData SemImputedData-class
#' @docType class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}

SemImputedData <- setClass("SemImputedData",
  slots = c(
    data = "ANY", # Ensuring 'data' is specifically a 'mids' object
    model = "ANY", # 'lavaan' or 'OpenMx' model syntax
    method = "character", # 'lavaan' or 'OpenMx',
    conf.int = "logical", # Whether to include confidence intervals in the SEM results
    conf.level = "numeric", # The confidence level for confidence intervals
    original_data = "data.frame", # The original data used to create the imputed datasets
    n_imputations = "numeric", # The number of imputations used to create the imputed datasets
    fit_model = "ANY" # The fitted SEM model object
  ),
  prototype = list(
    data = NULL,
    model = NULL,
    method = "lavaan",
    conf.int = FALSE,
    conf.level = 0.95,
    original_data = NULL,
    n_imputations = NULL,
    fit_model = NULL
  )
)

setValidity("SemImputedData", function(object) {
  messages <- character()

  if (!inherits(object@data, "mids")) {
    messages <- c(messages, "'data' must be a 'mids' object from the 'mice' package.")
  }

  # if (!is_lav_syntax(object@model, quiet = TRUE) && !inherits(object@model, "lavaan") && !inherits(object@model, "MxModel")) {
  #   messages <- c(messages, "'model' must be a character string, a 'lavaan' object, or an 'MxModel' object.")
  # }

  # if (!object@method %in% c("lavaan", "OpenMx")) {
  #   messages <- c(messages, "'method' must be either 'lavaan' or 'OpenMx'.")
  # }
  # if (!object@method %in% c("lavaan", "OpenMx")) {
  #   messages <- c(messages, "'method' must be either 'lavaan' or 'OpenMx'.")
  # }

  if (!is.logical(object@conf.int) || length(object@conf.int) != 1) {
    messages <- c(messages, "'conf.int' must have single logical value.")
  }

  if (!is.numeric(object@conf.level) || length(object@conf.level) != 1 || object@conf.level < 0 || object@conf.level > 1) {
    messages <- c(messages, "'conf.level' must be a single numeric value between 0 and 1.")
  }
  # if (!inherits(object@fit_model, "lavaan") && !inherits(object@fit_model, "MxModel")) {
  #   messages <- c(messages, "'fit_model' must be a 'lavaan' or 'MxModel' object.")
  # }

  if (length(messages) == 0) TRUE else messages
})

### =========================================================
### SemResults Methods
### =========================================================

### ----------------------------------------------------------------------------
### run_sem method
### ----------------------------------------------------------------------------
#' Run a SEM model
#'
#' A generic function to run and analyze multiply imputed data sets.
#'
#' @param object A `SemImputedData` object
#' @param ... Additional arguments passed to either [lavaan::sem] or [OpenMx::MxModel].
#' @return A `SemResults` object
#' @usage run_sem(object, ...)
#' @export
#' @rdname run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @importFrom methods setGeneric

setGeneric(
  "run_sem",
  function(object, ...) {
    standardGeneric("run_sem")
  }
)

#' Run SEM Analysis on Imputed Data
#'
#' This method facilitates running SEM analysis using either lavaan or OpenMx
#' on multiply imputed datasets contained within a [SemImputedData-class] object.
#' @export
#' @rdname run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @examples
#' \dontrun{
#' # Load Holzinger and Swineford (1939) dataset
#' data("HolzingerSwineford1939", package = "lavaan")
#' # Introduce missing data
#' df_complete <- na.omit(HolzingerSwineford1939[paste0("x", 1:9)])
#' amp <- mice::ampute(df_complete, prop = 0.1, mech = "MAR")
#' data_with_missing <- amp$amp
#' # Perform multiple imputation
#' imputed_data <- mice::mice(data_with_missing, m = 3, maxit = 3, seed = 12345, printFlag = FALSE)
#' sem_data <- set_sem(data = imputed_data, method = "lavaan")
#' model <- "
#'  visual  =~ x1 + x2 + x3
#'  textual =~ x4 + x5 + x6
#'  speed   =~ x7 + x8 + x9
#'  "
#' ## Note that the model is specified as a string
#' res <- run_sem(sem_data, model)
#' }
setMethod("run_sem", "SemImputedData", function(object, ...) {
  if (!inherits(object@data, "mids")) {
    stop("'object@data' must be a 'mids' object from the 'mice' package.")
  }

  # Dynamically select the appropriate function based on the SEM method.
  sem_fn <- switch(tolower(object@method),
                   "lavaan" = lav_mice,
                   "openmx" = mx_mice,
                   stop("Unsupported method specified: ", object@method)
  )

  # Run the SEM model on the imputed datasets
  sem_results <- sem_fn(object@data, object@model, ...)

  # Extract the results from the imputed datasets
  vcov_sem <- switch(tolower(object@method),
                     "lavaan" = vcov_lav,
                     "openmx" = vcov,
                     stop("Unsupported method specified: ", object@method)
  )

  coef_sem <- switch(tolower(object@method),
                     "lavaan" = lavaan::coef,
                     "openmx" = coef,
                     stop("Unsupported method specified: ", object@method)
  )
  # Extract the tidy results from the estimated SEM models
  estimate_df <- purrr::map_dfr(sem_results, tidy, .id = ".imp") # long tidy table of estimates across imputed datasets
  # Extract the coefficients from the estimated SEM models
  coef_df <- purrr::map_dfr(sem_results, coef_sem, .id = ".imp") # long table of coefficients across imputed datasets
  # Extract the sampling covariance (within covariance) matrices from the estimated SEM models
  cov_df <- purrr::map(sem_results, vcov_sem) # list coefficients estimates sampling covariances across imputed datasets

  # Create a new SemResults object
  SemResults(results = sem_results, estimate_df = estimate_df, coef_df = coef_df, cov_df = cov_df, method = object@method, conf.int = object@conf.int, conf.level = object@conf.level)
})


### ----------------------------------------------------------------------------
### Helper internal functions for run_sem method
### ----------------------------------------------------------------------------
lav_mice <- function(data, model, ...) {
  # Extract complete imputed datasets
  sem_results <-
    mice::complete(data, action = "all") |> purrr::map(lavaan::sem, model = model, ...)
  return(sem_results)
}

mx_mice <- function(data, model, ...) {
  # Ensure 'mxModel' is an OpenMx model object
  if (!inherits(model, "MxModel")) {
    stop("'model' must be an 'MxModel' object from the 'OpenMx' package.")
  }
  verified <- OpenMx::imxVerifyModel(model)
  if (!verified) {
    stop("The mxModel object failed verification.")
  }
  # Extract complete imputed datasets
  data_complete <- mice::complete(data, action = "all")
  # Fit the model to each imputed dataset
  sem_results <- data_complete |> purrr::map(\(df) {
    mxDataObj <- OpenMx::mxData(df, type = "raw")
    updatedModel <- OpenMx::mxModel(model, mxDataObj)
    OpenMx::mxRun(updatedModel)
  })
  return(sem_results)
}
