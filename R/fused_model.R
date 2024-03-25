#' Create a `fused_model`
#'
#' @param mode Needs to be regression
#'
#' @param penalty sets the penalty parameter (lambda)
#'
#' @return
#'
#' A `fused_model` object.
#'
#' @export
#' @rdname fused_model
fused_model <- function(mode = "regression",  penalty = NULL) {
    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }

    # Capture the arguments in quosures
    args <- list(penalty = rlang::enquo(penalty))

    # Save some empty slots for future parts of the specification
    parsnip::new_model_spec(
      "fused_model",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }
