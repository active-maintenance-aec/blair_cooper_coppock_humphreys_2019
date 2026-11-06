# blair_cooper_coppock_humphreys_2019/maintained/table_a18_discovery_intended.R
# Output: output/table_a18_discovery_intended.csv
# Depends on: helpers.R
# Description: The discovery design of appendix section 3.3 under two preliminary
#   filters, on the same draws. "as_printed" keeps the printed code's tidy(...)[4, 4],
#   which is the t statistic, and reproduces the published table. "intended" filters on
#   the p-value the appendix's prose describes. The filter is the only difference
#   between the two sets of estimators: one declaration computes both screens in one
#   step, so every estimator sees the same draw. The errata entry on the discovery
#   table reads its corrected values from this output. Assignment is complete, as
#   declare_assignment(prob = 0.5) was in the deposit's version, where randomizr
#   read a probability with no count as complete random assignment.

source(here::here("maintained", "helpers.R"))

set.seed(343)

# Conditional bias moves in the second decimal between runs of the published 5000 draws,
# so corrected values printed to two decimals need more.
n_draws <- 50000

population <- declare_model(
  N = 200,
  X = draw_binary(prob = rep(0.5, N)),
  heterogenous_effect = sample(c(0, .5), 1, TRUE),
  train = draw_binary(prob = rep(0.5, N)),
  u = rnorm(N),
  potential_outcomes(Y ~ Z + heterogenous_effect * Z * X + u)
)

assignment <- declare_assignment(Z = complete_ra(N = N, prob = 0.5))

reveal <- declare_measurement(Y = reveal_outcomes(Y ~ Z))

# Both screens, in the training split and in the full sample, are computed once.
# The statistic is tidy(...)[4, 4] as printed; the p-value is the same row's p.value.
explore <- declare_step(
  train_statistic = tidy(lm_robust(Y ~ Z * X, subset = train == 1))[4, 4][[1]],
  train_p_value = tidy(lm_robust(Y ~ Z * X, subset = train == 1))[4, "p.value"][[1]],
  all_statistic = tidy(lm_robust(Y ~ Z * X))[4, 4][[1]],
  all_p_value = tidy(lm_robust(Y ~ Z * X))[4, "p.value"][[1]],
  handler = fabricate
)

discovery_inquiry <- declare_inquiry(
  diff_in_CATEs = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]) - mean(Y_Z_1[X == 0] - Y_Z_0[X == 0])
)

discovery_estimator <- function(data, sample, screen) {
  prefix <- if (sample == "Full sample") "all" else "train"
  screen_value <- data[[str_c(prefix, if (screen == "as_printed") "_statistic" else "_p_value")]][1]
  if (screen_value < 0.05) {
    fit_data <- if (sample == "Full sample") data else filter(data, train == 0)
    fit <- lm_robust(Y ~ Z * X, data = fit_data)
    return(tidy(fit)[4, c("estimate", "p.value", "term")])
  }
  tibble(estimate = NA_real_, p.value = NA_real_, term = "Z:X")
}

full_as_printed <- declare_estimator(
  .method = discovery_estimator, sample = "Full sample", screen = "as_printed",
  inquiry = "diff_in_CATEs", label = "Full sample | as_printed"
)

test_as_printed <- declare_estimator(
  .method = discovery_estimator, sample = "Test sample", screen = "as_printed",
  inquiry = "diff_in_CATEs", label = "Test sample | as_printed"
)

full_intended <- declare_estimator(
  .method = discovery_estimator, sample = "Full sample", screen = "intended",
  inquiry = "diff_in_CATEs", label = "Full sample | intended"
)

test_intended <- declare_estimator(
  .method = discovery_estimator, sample = "Test sample", screen = "intended",
  inquiry = "diff_in_CATEs", label = "Test sample | intended"
)

discovery <- population + assignment + reveal + explore + discovery_inquiry +
  full_as_printed + test_as_printed + full_intended + test_intended

discovery_diagnosands <- declare_diagnosands(
  bias = mean((estimate - estimand), na.rm = TRUE),
  RMSE = sqrt(mean((estimate - estimand)^2, na.rm = TRUE)),
  frequency = mean(!is.na(estimate)),
  false_pos = mean(p.value[estimand == 0] < 0.05, na.rm = TRUE),
  false_neg = 1 - mean(p.value[estimand != 0] < 0.05, na.rm = TRUE)
)

# Diagnosis ----
diagnosis <- diagnose_design(
  discovery,
  diagnosands = discovery_diagnosands,
  sims = n_draws,
  bootstrap_sims = FALSE
)

tab <- get_diagnosands(diagnosis) |>
  as_tibble() |>
  separate_wider_delim(estimator, delim = " | ", names = c("estimator", "filter")) |>
  select(estimator, filter, bias, RMSE, frequency, false_pos, false_neg, n_sims) |>
  arrange(filter != "as_printed", estimator)

write_csv(tab, out_path("table_a18_discovery_intended.csv"))

print(tab)
