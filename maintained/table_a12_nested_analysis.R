# blair_cooper_coppock_humphreys_2019/maintained/table_a12_nested_analysis.R
# Output: output/table_a12_nested_confounding.csv,
#         output/table_a13_nested_effort.csv,
#         output/table_a14_nested_thresholds.csv
# Depends on: helpers.R
# Description: The nested mixed methods design of appendix section 3.2.3,
#   diagnosed across the confounding between X and W, across the effort spent on
#   model testing and model building, and across the three decision thresholds.
#   All three tables come from one declaration, so they share a script.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

ate <- 0.50
rho <- 0.70
LNA_threshold <- 0.20
SNA_threshold_1 <- 1 / 3
SNA_threshold_2 <- 0.10
model_testing_effort <- 1
model_building_effort <- 1

LN_data <- declare_model(
  N = 1000,
  W = rbinom(N, 1, prob = 0.5),
  X = correlate(given = W, rho = rho, draw_binary, prob = 0.5),
  u = runif(N),
  Y_X_0 = W * (u > (1 + ate) / 2),
  Y_X_1 = W * (u > (1 - ate) / 2),
  Y = X * Y_X_1 + (1 - X) * Y_X_0,
  outcome = paste0(X, Y)
)

true_effect <- declare_inquiry(X_on_Y = mean(Y_X_1 - Y_X_0))

LNA <- declare_step(
  LN_res_var = lm_robust(Y ~ X)$res_var,
  LNA_satisfactory = LN_res_var < LNA_threshold,
  handler = fabricate
)

select_on_line <- declare_sampling(
  S = strata_rs(
    strata = outcome,
    strata_n = model_testing_effort * c("00" = 2, "01" = 0, "10" = 0, "11" = 2)
  ),
  filter = S == 1
)

select_on_and_off_line <- declare_sampling(
  S = strata_rs(
    strata = outcome,
    strata_n = model_building_effort * c("00" = 1, "01" = 1, "10" = 1, "11" = 1)
  ),
  filter = S == 1
)

SNA <- declare_step(handler = function(data) {
  LNA_satisfactory <- all(data$LNA_satisfactory)
  if (LNA_satisfactory) {
    SN_data <- select_on_line(data)
    SNA_fits_LNA <- mean(with(SN_data, Y_X_1 - Y_X_0)) > SNA_threshold_1
  } else {
    SNA_fits_LNA <- FALSE
  }
  if (LNA_satisfactory & SNA_fits_LNA) {
    data$situation <- "I: Original Model Accepted"
  } else {
    SN_data <- select_on_and_off_line(data)
    Y_on_X_W <- with(subset(SN_data, W == 1), mean(Y_X_1 - Y_X_0))
    Y_on_X_no_W <- with(subset(SN_data, W == 0), mean(Y_X_1 - Y_X_0))
    evidence_for_W <- Y_on_X_no_W - Y_on_X_W
    evidence_for_W <- if_else(is.na(evidence_for_W), 0, abs(evidence_for_W))
    new_model_discovered <- evidence_for_W > SNA_threshold_2
    data$situation <- if_else(
      new_model_discovered,
      "II: New Model Accepted",
      "IV: Original Model Rejected, No New Model"
    )
  }
  data
})

original_LNA <- declare_estimator(
  Y ~ X, .method = lm_robust, inquiry = true_effect, label = "Original Model"
)

new_LNA <- declare_estimator(
  Y ~ X, covariates = ~W, .method = lm_lin, inquiry = true_effect, label = "New Model"
)

final_analysis <- declare_estimator(handler = function(data) {
  situation <- unique(data$situation)
  if (situation == "I: Original Model Accepted") return(original_LNA(data))
  if (situation == "II: New Model Accepted") return(new_LNA(data))
  tibble(
    estimator = "No Model", term = "X", estimate = NA_real_, std.error = NA_real_,
    statistic = NA_real_, p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
    df = NA_real_, outcome = "Y", inquiry = "X_on_Y"
  )
})

nested_design <- LN_data + true_effect + LNA + SNA + final_analysis

nested_diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand, na.rm = TRUE),
  rmse = (mean((estimate - estimand)^2, na.rm = TRUE))^.5,
  mean_estimate = mean(estimate, na.rm = TRUE),
  mean_estimand = mean(estimand)
)

# Varying the confounding between X and W ----
confounding_designs <- redesign(nested_design, rho = c(0, 0.5))

diagnosis <- diagnose_design(
  confounding_designs,
  diagnosands = nested_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a12_nested_confounding.csv")
print(tab |> select(rho, estimator, bias, rmse, mean_estimate, mean_estimand))

# Varying the effort spent on testing and building ----
sna_designs <- redesign(
  nested_design,
  model_testing_effort = c(5, 1),
  model_building_effort = c(1, 5)
)

diagnosis_effort <- diagnose_design(
  sna_designs,
  diagnosands = nested_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

# The published table reports the share of simulations reaching the new model,
# which is the count of simulations that returned the New Model estimator
# divided by the number of simulations run.
effort_shares <- get_simulations(diagnosis_effort) |>
  as_tibble() |>
  count(model_testing_effort, model_building_effort, estimator, name = "n_sims_reached") |>
  mutate(prop_sims = n_sims_reached / published_sims) |>
  arrange(model_testing_effort, model_building_effort) |>
  filter(estimator == "New Model")

write_csv(effort_shares, out_path("table_a13_nested_effort.csv"))
print(effort_shares)

# Varying the three decision thresholds ----
threshold_designs <- redesign(
  nested_design,
  LNA_threshold = c(0.1, 0.8),
  SNA_threshold_1 = c(0.1, 0.8),
  SNA_threshold_2 = c(0.05, 0.3)
)

diagnosis_thresholds <- diagnose_design(
  threshold_designs,
  diagnosands = nested_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

threshold_shares <- get_simulations(diagnosis_thresholds) |>
  as_tibble() |>
  count(LNA_threshold, SNA_threshold_1, SNA_threshold_2, estimator, name = "n_sims_reached") |>
  mutate(prop_sims = n_sims_reached / published_sims) |>
  arrange(desc(prop_sims)) |>
  filter(estimator == "New Model")

write_csv(threshold_shares, out_path("table_a14_nested_thresholds.csv"))
print(threshold_shares)
