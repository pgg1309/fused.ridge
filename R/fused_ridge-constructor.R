new_fused_ridge <- function(coef_values, lambda, positive, coef_names, blueprint) {
  hardhat::new_model(
    coef_values = coef_values,
    coef_names = coef_names,
    lambda = lambda,
    positive = positive,
    deriv2_smooth = deriv2_smooth,
    blueprint = blueprint,
    class = "fused_ridge"
    )
}
