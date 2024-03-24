#' Fit a `fused_ridge`
#'
#' `fused_ridge()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param lambda A numeric value specifying the penalty parameter.
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `fused_ridge` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#' lambda <- 1
#'
#' # XY interface
#' # mod <- fused_ridge(predictors, outcome, lambda)
#'
#' # Formula interface
#' # mod2 <- fused_ridge(mpg ~ ., mtcars, lambda)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' # mod3 <- fused_ridge(rec, mtcars, lambda)
#'
#' @export
fused_ridge <- function(x, ...) {
  UseMethod("fused_ridge")
}

#' @export
#' @rdname fused_ridge
fused_ridge.default <- function(x, ...) {
  stop("`fused_ridge()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname fused_ridge
fused_ridge.data.frame <- function(x, y, lambda, ...) {
  processed <- hardhat::mold(x, y)
  fused_ridge_bridge(processed, lambda, ...)
}

# XY method - matrix

#' @export
#' @rdname fused_ridge
fused_ridge.matrix <- function(x, y, lambda, ...) {
  processed <- hardhat::mold(x, y)
  fused_ridge_bridge(processed, lambda, ...)
}

# Formula method

#' @export
#' @rdname fused_ridge
fused_ridge.formula <- function(formula, data, lambda, ...) {
  processed <- hardhat::mold(formula, data)
  fused_ridge_bridge(processed, lambda, ...)
}

# Recipe method

#' @export
#' @rdname fused_ridge
fused_ridge.recipe <- function(x, data, lambda, ...) {
  processed <- hardhat::mold(x, data)
  fused_ridge_bridge(processed, lambda, ...)
}

# ------------------------------------------------------------------------------
# Bridge

fused_ridge_bridge <- function(processed, lambda, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes

  # one can add validation, e.g.
  hardhat::validate_outcomes_are_univariate(outcome)
  hardhat::validate_outcomes_are_numeric(outcome)
  hardhat::validate_predictors_are_numeric(predictors)

  predictors <- as.matrix(predictors)
  outcome <- as.matrix(outcome)


  fit <- fused_ridge_impl(predictors, outcome, lambda)

  new_fused_ridge(
    coefs = fit$coefs,
    lambda = fit$lambda,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

fused_ridge_impl <- function(predictors, outcome, lambda) {

  # --- Define the Shrinkage-Weights
  shrinkw = apply(predictors,2,stats::sd)

  # --- Initialize CVXR::the Coefficients
  coeffs <- CVXR::Variable(ncol(predictors))
  # --- Define the Loss-Function
  loss <- CVXR::Minimize(sum((outcome - predictors %*% coeffs)^2) + lambda*sum(CVXR::diff(shrinkw*coeffs)^2))
  # --- Set the constraints
  constr <- list(coeffs >= 0, t(coeffs) %*% apply(predictors,2,mean) == mean(outcome))
  # --- Set the Problem
  prob <- CVXR::Problem(loss,constr)
  # --- Solve the Problem
  sol <- CVXR::solve(prob)
  # --- Get the betas
  beta <- sol$getValue(coeffs)

  list(coefs = beta, lambda = lambda)
}

