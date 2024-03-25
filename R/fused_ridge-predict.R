#' Predict from a `fused_ridge`
#'
#' @param object A `fused_ridge` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#' lambda <- 1
#'
#' # Fit
#' #mod <- fused_ridge(mpg ~ cyl + log(drat), train, lambda)
#'
#' # Predict, with preprocessing
#' #predict(mod, test)
#'
#'
#' @export
#' @rdname predict
predict.fused_ridge <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_fused_ridge_predict_types())
  predict_fused_ridge_bridge(type, object, forged$predictors)
}

valid_fused_ridge_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_fused_ridge_bridge <- function(type, model, predictors) {
  predictors <- as.matrix(predictors)

  predict_function <- get_fused_ridge_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_fused_ridge_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_fused_ridge_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_fused_ridge_numeric <- function(model, predictors) {
  predictions <-  as.vector(predictors %*% model$coef_values)
  hardhat::spruce_numeric(predictions)
}
