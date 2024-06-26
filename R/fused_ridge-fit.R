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
fused_ridge.default <- function(x, lambda, positive, ...) {
  stop("`fused_ridge()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname fused_ridge
fused_ridge.data.frame <- function(x, y, lambda, positive, ...) {
  processed <- hardhat::mold(x, y)
  fused_ridge_bridge(processed, lambda, positive,...)
}

# XY method - matrix

#' @export
#' @rdname fused_ridge
fused_ridge.matrix <- function(x, y, lambda, positive, ...) {
  processed <- hardhat::mold(x, y)
  fused_ridge_bridge(processed, lambda, positive, ...)
}

# Formula method

#' @export
#' @rdname fused_ridge
fused_ridge.formula <- function(formula, data, lambda, positive, ...) {
  processed <- hardhat::mold(formula, data)
  fused_ridge_bridge(processed, lambda, positive, ...)
}

# Recipe method

#' @export
#' @rdname fused_ridge
fused_ridge.recipe <- function(x, data, lambda, positive, ...) {
  processed <- hardhat::mold(x, data)
  fused_ridge_bridge(processed, lambda, positive, ...)
}

# ------------------------------------------------------------------------------
# Bridge

fused_ridge_bridge <- function(processed, lambda, positive, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes

  # one can add validation, e.g.
  hardhat::validate_outcomes_are_univariate(outcome)
  hardhat::validate_outcomes_are_numeric(outcome)
  hardhat::validate_predictors_are_numeric(predictors)

  coef_names <- colnames(predictors)
  predictors <- as.matrix(predictors)
  outcome <- as.matrix(outcome)

  fit <- fused_ridge_impl(predictors, outcome, lambda, positive)

  new_fused_ridge(
    coef_values =  fit$coefs,
    lambda = fit$lambda,
    positive = fit$positive,
    deriv2_smooth = fit$deriv2_smooth,
    coef_names = coef_names,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

fused_ridge_impl <- function(predictors, outcome, lambda, positive) {

  # --- Define the Shrinkage-Weights
  shrinkw = apply(predictors,2,stats::sd)

  # --- Initialize CVXR::the Coefficients
  coeffs <- CVXR::Variable(ncol(predictors))
  # --- Define the Loss-Function
  loss <- CVXR::Minimize(sum((outcome - predictors %*% coeffs)^2) + lambda*sum(CVXR::diff(shrinkw*coeffs)^2))
  # --- Set the constraints
if (positive) {
  constr <- list(coeffs >= 0, t(coeffs) %*% apply(predictors,2,mean) == mean(outcome))
  }
else {
  constr <- list(t(coeffs) %*% apply(predictors,2,mean) == mean(outcome))
}
  # --- Set the Problem
  prob <- CVXR::Problem(loss,constr)
  # --- Solve the Problem
  sol <- CVXR::solve(prob)
  # --- Get the betas
  beta <- sol$getValue(coeffs)
  # --- add smoothness
  deriv2_smooth <- sum(stats::splinefun(1:nrow(beta), beta)(1:nrow(beta),2)^2)

  list(coefs = beta, lambda = lambda, positive = positive, deriv2_smooth = deriv2_smooth)
}


# Make parsnip model
make_fused_model <- function() {
  # Step 1. Register the model, modes, and arguments ------------------------
  parsnip::set_new_model("fused_model")
  parsnip::set_model_mode(model = "fused_model", mode = "regression")
  parsnip::set_model_engine(
    "fused_model",
    mode = "regression",
    eng = "fused_ridge"
  )
  parsnip::set_dependency("fused_model", eng = "fused_ridge", pkg = "fused.ridge", mode = "regression")

  parsnip::set_model_arg(
    model = "fused_model",
    eng = "fused_ridge",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "fused_model",
    eng = "fused_ridge",
    parsnip = "positive",
    original = "positive",
    func = list(pkg = "dials", fun = "positive"),
    has_submodel = FALSE
  )

  # Step 2. Create the model function ---------------------------------------
  # Moved outside of the function

  # Step 3. Add a fit module ------------------------------------------------
  parsnip::set_fit(
    model = "fused_model",
    eng = "fused_ridge",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "fused.ridge", fun = "fused_ridge"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "fused_model",
    eng = "fused_ridge",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )


  # Step 4. Add modules for prediction --------------------------------------
  parsnip::set_pred(
    model = "fused_model",
    eng = "fused_ridge",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "fused.ridge", fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "numeric"
        )
    )
  )
}


.onLoad <- function(libname, pkgname) {
  make_fused_model()
}

