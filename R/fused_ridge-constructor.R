new_fused_ridge <- function(coefs, lambda, blueprint) {
  hardhat::new_model(coefs = coefs, lambda = lambda, blueprint = blueprint, class = "fused_ridge")
}
