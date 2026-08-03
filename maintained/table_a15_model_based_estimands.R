# blair_cooper_coppock_humphreys_2019/maintained/table_a15_model_based_estimands.R
# Output: output/table_a15_model_based_estimands.csv
# Depends on: helpers.R
# Description: Diagnosis of the three-condition design of appendix section 3.2.4
#   under equal and unequal assignment probabilities, comparing a model-based
#   inquiry with the average of the arm-specific effects.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

f_Y <- function(Z, u) Z^.5 + u

model_estimand_function <- function(Y_Z_1, Y_Z_2, Y_Z_3) {
  YY <- c(Y_Z_1, Y_Z_2, Y_Z_3)
  XX <- rep(1:3, each = length(Y_Z_1))
  coef(lm(YY ~ XX))[2]
}

prob_each <- c(1, 1, 1) / 3

model_estimand <-
  declare_model(N = 10, u = rnorm(N)) +
  declare_potential_outcomes(Y ~ f_Y(Z, u), conditions = 1:3) +
  declare_inquiry(
    beta = model_estimand_function(Y_Z_1, Y_Z_2, Y_Z_3),
    aate = mean((f_Y(3, u) - f_Y(1, u)) / 2)
  ) +
  declare_assignment(Z = complete_ra(N = N, conditions = 1:3, prob_each = prob_each)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = lm_robust, inquiry = c("beta", "aate"), label = "ols")

designs <- redesign(model_estimand, prob_each = list(c(1, 1, 1) / 3, c(.4, .4, .2)))

diagnosis <- diagnose_design(
  designs,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a15_model_based_estimands.csv")

print(tab |> select(design, inquiry, mean_estimand, bias, rmse, power, coverage))
