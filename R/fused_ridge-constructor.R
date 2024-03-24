new_fused_ridge <- function(coef_values, lambda, coef_names, blueprint) {
  hardhat::new_model(
    coef_values = coef_values,
    coef_names = coef_names,
    lambda = lambda,
    blueprint = blueprint,
    class = "fused_ridge"
    )
}
