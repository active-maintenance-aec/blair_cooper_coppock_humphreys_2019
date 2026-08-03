# blair_cooper_coppock_humphreys_2019/maintained/table_a7_qca.R
# Output: output/table_a7_qca_simulated_data.csv,
#         output/table_a8_qca_estimates.csv,
#         output/table_a9_qca_diagnosis.csv,
#         output/table_a10_qca_error_rate_and_n.csv,
#         output/table_a11_qca_with_least_squares.csv
# Depends on: helpers.R
# Description: The qualitative comparative analysis design of appendix section
#   3.2.2: one simulated dataset, the estimates it produces, and three diagnoses
#   varying the measurement error rate, the number of cases, and the answer
#   strategy. All five tables come from one declaration, so they share a script.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

N <- 6
error_rate <- 0

cases <- declare_model(
  N = N,
  A = rbinom(N, 1, .2),
  B = rbinom(N, 1, .8)
)

counterfactuals <- declare_potential_outcomes(
  Y ~ 1 * (A == 0 & B == 1),
  conditions = list(A = 0:1, B = 0:1)
)

inquiry <- declare_inquiry(true_configuration = "a*B")

true_outcome <- declare_measurement(Y = reveal_outcomes(Y ~ A + B))

code_outcome <- declare_step(
  mistake = rbinom(N, 1, error_rate) == 1,
  Y_obs = (1 - mistake) * Y + mistake * (1 - Y),
  handler = fabricate
)

# The deposited code passes outcome = "Y_Obs" to truthTable() while the column
# the design creates is Y_obs. Under QCA 3.25 that spelling sends every call into
# the tryCatch and returns NO SOLUTION for all of them, so the published failure
# rates of 0.07 and 0.00 could not have come from it; the deposit's own printed
# draw of the estimates returns "B" rather than NO SOLUTION. The column's own
# name is used here.
# QCA 3.25 writes the absence of a condition as ~A, where the article and the
# deposited inquiry write it as a lower case a. The two notations denote the same
# configuration, and the diagnosand is a string equality between the estimate and
# the inquiry, so the estimator's answer is put back into the inquiry's notation
# before the two meet. Without this every correct rate is zero, because no
# estimate written in the new notation can ever equal the estimand.
in_inquiry_notation <- function(expression) {
  str_replace_all(expression, "~[A-Za-z]", \(match) str_to_lower(str_sub(match, 2)))
}

minimization_algorithm <- function(data, method, label) {
  estimate <- tryCatch(
    expr = {
      truth_table <- QCA::truthTable(
        data = data, outcome = "Y_obs", incl.cut = 0.5, conditions = c("A", "B")
      )
      solutions <- QCA::minimize(truth_table, include = "?", method = method)$solution
      in_inquiry_notation(paste(unique(unlist(solutions)), collapse = " + "))
    },
    error = function(e) "NO SOLUTION"
  )
  tibble(estimate = estimate, inquiry = "true_configuration", estimator = label)
}

QMC_estimator <- declare_estimator(
  handler = minimization_algorithm, method = "QMC", label = "Classic Quine-McCluskey"
)

CC_estimator <- declare_estimator(
  handler = minimization_algorithm, method = "CCubes", label = "Consistency Cubes"
)

QCA_design <- cases + counterfactuals + inquiry + true_outcome + code_outcome +
  QMC_estimator + CC_estimator

write_csv(as_tibble(draw_data(QCA_design)), out_path("table_a7_qca_simulated_data.csv"))
write_csv(as_tibble(draw_estimates(QCA_design)), out_path("table_a8_qca_estimates.csv"))

QCA_diagnosands <- declare_diagnosands(
  correct_rate = mean(estimand == estimate),
  failure_rate = mean(estimate == "NO SOLUTION")
)

diagnosis <- diagnose_design(
  QCA_design,
  diagnosands = QCA_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a9_qca_diagnosis.csv")
print(tab |> select(estimator, correct_rate, failure_rate))

# Varying measurement error and the number of cases ----
QCA_designs <- redesign(QCA_design, error_rate = c(0, 0.05), N = c(5, 50))

diagnosis_error <- diagnose_design(
  QCA_designs,
  diagnosands = QCA_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab_error <- write_diagnosands(diagnosis_error, "table_a10_qca_error_rate_and_n.csv")
print(tab_error |> select(estimator, error_rate, N, correct_rate, failure_rate))

# Adding a least squares answer strategy ----
linear_minimization <- function(data, label) {
  betas <- coef(lm(Y_obs ~ A * B, data = data))
  betas[is.na(betas)] <- 0
  predictions <- c(
    "a*b" = betas[1],
    "A*b" = betas[1] + betas[2],
    "a*B" = betas[1] + betas[3],
    "A*B" = betas[1] + betas[2] + betas[3] + betas[4]
  )
  configurations <- c("a*b", "A*b", "a*B", "A*B")[predictions > 0.5]
  if (!any(predictions > 0.5)) {
    estimate <- "NO SOLUTION"
  } else {
    sum_of_products <- try(QCA::simplify(paste(configurations, collapse = "+")), silent = TRUE)
    estimate <- if_else(inherits(sum_of_products, "try-error"), "NO SOLUTION",
                        in_inquiry_notation(as.character(sum_of_products)[1]))
  }
  tibble(estimate = estimate, inquiry = "true_configuration", estimator = label)
}

linear_estimator <- declare_estimator(handler = linear_minimization, label = "Least Squares")

QCA_design_with_lm <- QCA_design + linear_estimator

QCA_designs_with_lm <- redesign(QCA_design_with_lm, error_rate = c(0, 0.20), N = c(3, 50))

diagnosis_lm <- diagnose_design(
  QCA_designs_with_lm,
  diagnosands = QCA_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab_lm <- write_diagnosands(diagnosis_lm, "table_a11_qca_with_least_squares.csv")
print(tab_lm |> select(estimator, error_rate, N, correct_rate, failure_rate))
