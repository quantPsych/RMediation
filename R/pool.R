#' Pool Method
#'
#' Description of what the pool function is supposed to do.
#'
#' @param x Object to be pooled.
#' @param ... Additional arguments to methods.
#' @export
#' @usage pool(x, ...)
#' @rdname pool
#' @name pool
#' @aliases pool

pool <- function(x, ...) {
  UseMethod("pool", x)
}
