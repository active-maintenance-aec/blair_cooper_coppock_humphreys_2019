# blair_cooper_coppock_humphreys_2019/maintained/table_a16_matching.R
# Output: output/table_a16_matching.csv
# Depends on: helpers.R
# Description: Diagnosis of the matching design of appendix section 3.2.5,
#   comparing the bias of a difference in means with the bias of nearest
#   neighbour matching when assignment depends on the covariates.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

population <- declare_model(N = 1000, X1 = rnorm(N), X2 = rnorm(N), X3 = rnorm(N))

potential_outcomes <- declare_potential_outcomes(Y ~ X1 + X2 + X3 + Z)

assignment <- declare_assignment(handler = function(data) {
  prob <- with(data, pnorm(X1 + X2 + X3))
  data$Z <- rbinom(nrow(data), 1, prob)
  data
})

reveal_Y <- declare_measurement(Y = reveal_outcomes(Y ~ Z))

# The inquiry comes after the data strategy because it is the effect among the
# units that happen to be treated.
inquiry <- declare_inquiry(att = mean(Y_Z_1[Z == 1] - Y_Z_0[Z == 1]))

estimator_d_i_m <- declare_estimator(Y ~ Z, inquiry = "att", label = "dim")

match_est <- function(data) {
  match_out <- with(data, Matching::Match(Y = Y, Tr = Z, X = cbind(X1, X2, X3)))
  tibble(term = "Z", estimate = match_out$est[1, 1])
}

estimator_m <- declare_estimator(
  handler = label_estimator(match_est), inquiry = "att", label = "matching"
)

matching <- population + potential_outcomes + assignment + reveal_Y + inquiry +
  estimator_d_i_m + estimator_m

diagnosis <- diagnose_design(
  matching,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a16_matching.csv")

print(tab |> select(inquiry, estimator, bias))
