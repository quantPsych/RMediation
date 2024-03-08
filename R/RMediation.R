#' @importFrom generics tidy
#' @export
generics::tidy
# Ensure that .data is recognized as a global variable
utils::globalVariables(".data")