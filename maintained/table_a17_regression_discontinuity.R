# blair_cooper_coppock_humphreys_2019/maintained/table_a17_regression_discontinuity.R
# Output: output/table_a17_regression_discontinuity.csv
# Depends on: helpers.R
# Description: Diagnosis of the regression discontinuity design of appendix
#   section 3.2.6, fitting a fourth order polynomial either side of the cutoff.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

cutoff <- 0.5

control <- function(X) as.vector(poly(X, 4, raw = TRUE) %*% c(0.7, -0.8, 0.5, 1))

treatment <- function(X) as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, 0.5, 0.8)) + 0.15

rdd <-
  declare_model(
    N = 1000,
    X = runif(N, 0, 1) - cutoff,
    noise = rnorm(N, 0, .1),
    Z = 1 * (X > 0)
  ) +
  declare_potential_outcomes(
    Y_Z_0 = control(X) + noise,
    Y_Z_1 = treatment(X) + noise
  ) +
  declare_inquiry(LATE = treatment(0) - control(0)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ poly(X, 4) * Z, term = "Z", .method = lm_robust, inquiry = "LATE"
  )

diagnosis <- diagnose_design(
  rdd,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a17_regression_discontinuity.csv")

print(tab |> select(inquiry, bias, rmse, power, coverage))
