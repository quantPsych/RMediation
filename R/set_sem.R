#' Create SemImputedData Object
#'
#' Constructs a new `SemImputedData` object for structural equation modeling (SEM) analysis
#' on multiply imputed datasets. This function ensures that the provided data is a [mice::mids]
#' object from the `mice` package and that the specified SEM analysis method is supported.
#'
#' @param data A `mids` object from the `mice` package containing multiply imputed datasets.
#' @param model A `lavaan` or `OpenMx` model syntax to be used for SEM analysis. For `lavaan` models, the syntax should be a character string as described in [lavaan::model.syntax]. For `OpenMx` models, the syntax should be an [mxModel] object with or without [mxData()] specified; that is, `mxModel` syntax can be without data specified. In addition, both `lavaan` and `OpenMx` models can be a fitted model object in the respective package.
#' @param conf.int A logical value indicating whether confidence intervals are
#'   included in the SEM results. Defaults to `FALSE`.
#' @param conf.level A numeric value specifying the confidence level for
#'  confidence intervals, which must be between 0 and 1. Defaults to 0.95.
#'  If `conf.int` is `FALSE`, this argument is ignored.
#'
#' @return An object of class SemImputedData. See [SemImputedData] for the details of the slots.
#'
#' @details All the arguments `data`, `method`, `conf.int`, and `conf.level` are used to specify the SEM analysis. `set_sem` is a constructor function for `SemImputedData` class. These methods are used as constructors for the `SemImputedData` class.
#' @usage set_sem(data, method = "lavaan", conf.int = FALSE, conf.level 0.95)
#' @seealso  [SemImputedData] [mice::mids] [lavaan] [OpenMx]
#' @aliases set_sem set_sem-methods
#' @examples
#' \dontrun{
#' data("HolzingerSwineford1939", package = "lavaan")
#' df_complete <- na.omit(HolzingerSwineford1939)
#' amp <- mice::ampute(df_complete, prop = 0.2, mech = "MAR")
#' imputed_data <- mice::mice(amp$amp, m = 3, maxit = 3, seed = 12345, printFlag = FALSE)
#' sem_data <- set_sem(data = imputed_data, method = "lavaan")
#' str(sem_data)
#' }
#' @rdname set_sem
#' @export

setGeneric("set_sem", function(data, model, conf.int = FALSE, conf.level = 0.95) standardGeneric("set_sem"),
           signature = "data"
)

#' @rdname set_sem
#' @export

setMethod("set_sem", "mids", function(data, model, conf.int = FALSE, conf.level = 0.95) {
  if (missing(data)) {
    stop("Argument 'data' is missing.", call. = FALSE)
  }

  if (missing(model)) {
    stop("Argument 'model' is missing.", call. = FALSE)
  }

  if (!inherits(data, "mids")) {
    stop("'data' must be a 'mids' object from the 'mice' package.",
         call. = FALSE
    )
  }

  if (!all(model_type(model) %in% c("lavaan_syntax", "lavaan", "MxModel", "OpenMx"))) {
    stop("The model must be a character string, a lavaan model object, or an OpenMx model object.")
  }

  if (!is.numeric(conf.level) ||
      length(conf.level) != 1 || conf.level < 0 || conf.level > 1) {
    stop("'conf.level' must be a single numeric value between 0 and 1.",
         call. = FALSE
    )
  }

  if (!is.logical(conf.int) || length(conf.int) != 1) {
    stop("'conf.int' must be a single logical value (TRUE or FALSE).",
         call. = FALSE
    )
  }

  n_imputations <- data$m # number of imputations
  original_data <- mice::complete(data, action = 0L) # original data
  fit_model0 <- fit_model(model, original_data)
  method <- model_type(fit_model0)
  method <- ifelse(all(method %in% c("MxModel", "OpenMx")), "OpenMx", "lavaan")

  SemImputedData(
    data = data,
    model = model,
    method = method,
    conf.int = conf.int,
    conf.level = conf.level,
    original_data = original_data,
    n_imputations = n_imputations,
    fit_model = fit_model0
  )
})