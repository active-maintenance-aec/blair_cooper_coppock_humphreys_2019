# blair_cooper_coppock_humphreys_2019/maintained/table_a18_discovery.R
# Output: output/table_a18_discovery.csv
# Depends on: helpers.R
# Description: Diagnosis of the discovery design of appendix section 3.3, in
#   which a difference in conditional average effects is reported only when a
#   preliminary test is significant, either in a training split or in the full
#   sample.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

population <- declare_model(
  N = 200,
  X = draw_binary(prob = rep(0.5, N)),
  heterogenous_effect = sample(c(0, .5), 1, TRUE),
  train = draw_binary(prob = rep(0.5, N)),
  u = rnorm(N)
)

pos <- declare_potential_outcomes(Y ~ Z + heterogenous_effect * Z * X + u)

inquiry <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

assignment <- declare_assignment(Z = simple_ra(N = N, prob = 0.5))

reveal <- declare_measurement(Y = reveal_outcomes(Y ~ Z))

main_analysis <- declare_estimator(Y ~ Z, inquiry = "ATE", label = "Main")

explore <- declare_step(
  train_pval = tidy(lm_robust(Y ~ Z * X, subset = train == 1))[4, 4],
  all_pval = tidy(lm_robust(Y ~ Z * X))[4, 4],
  handler = fabricate
)

discovery_inquiry <- declare_inquiry(
  diff_in_CATEs = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]) - mean(Y_Z_1[X == 0] - Y_Z_0[X == 0])
)

test_sample_estimator <- function(data) {
  if (data$train_pval[1] < 0.05) {
    fit <- lm_robust(Y ~ Z * X, subset = train == 0, data = data)
    return(tidy(fit)[4, c("estimate", "p.value", "term")])
  }
  tibble(estimate = NA_real_, p.value = NA_real_, term = "Z:X")
}

test_sample_analysis <- declare_estimator(
  handler = label_estimator(test_sample_estimator),
  inquiry = "diff_in_CATEs",
  label = "Test sample"
)

full_sample_estimator <- function(data) {
  if (data$all_pval[1] < 0.05) {
    fit <- lm_robust(Y ~ Z * X, data = data)
    return(tidy(fit)[4, c("estimate", "p.value", "term")])
  }
  tibble(estimate = NA_real_, p.value = NA_real_, term = "Z:X")
}

full_sample_analysis <- declare_estimator(
  handler = label_estimator(full_sample_estimator),
  inquiry = "diff_in_CATEs",
  label = "Full sample"
)

discovery <- population + pos + inquiry + assignment + reveal +
  main_analysis + explore + discovery_inquiry + test_sample_analysis +
  full_sample_analysis

discovery_diagnosands <- declare_diagnosands(
  bias = mean((estimate - estimand), na.rm = TRUE),
  RMSE = sqrt(mean((estimate - estimand)^2, na.rm = TRUE)),
  frequency = mean(!is.na(estimate)),
  false_pos = mean(p.value[estimand == 0] < 0.05, na.rm = TRUE),
  false_neg = 1 - mean(p.value[estimand != 0] < 0.05, na.rm = TRUE)
)

diagnosis <- diagnose_design(
  discovery,
  diagnosands = discovery_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a18_discovery.csv")

print(tab |> select(estimator, term, bias, RMSE, frequency, false_pos, false_neg))
