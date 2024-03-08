sem_mice <- function(model, mids, ...) {
    # Ensure 'mids' is a 'mids' object
    if (!inherits(mids, "mids")) {
        stop("'mids' must be a 'mids' object from the 'mice' package.")
    }
    # Ensure 'mxModel' is an OpenMx model object
    if (!inherits(model, "MxModel") ) {
        stop("'mids' must be an 'MxModel' object from the 'OpenMx' package.")
    } else {
       mx_mice(model, mids, ...)
    }

 if (!RMediation::is_valid_lav_syntax(model, mids$data)) {
    stop("The model is not a valid lavaan model syntax.")
 } else {
    lav_mice(model, mids, ...)
 }

dplyr::case_when(
    inherits(model, "MxModel") ~ mx_mice(model, mids, ...),
    RMediation::is_valid_lav_syntax(model, mids$data) ~ lav_mice(model, mids, ...),
    TRUE ~ stop("The model is not a valid lavaan or OpenMx model syntax.")
)

}

