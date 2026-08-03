# blair_cooper_coppock_humphreys_2019/maintained/table_a21_bs_covariate_adjustment.R
# Output: output/table_a21_bs_covariate_adjustment.csv
# Depends on: helpers.R, bs_design.R
# Description: Appendix section 4.7, comparing four ways of handling a
#   community based organisation covariate in the Bjorkman and Svensson design,
#   when the covariate is independent of area health and when it correlates with
#   it at 0.8.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

source(here::here("maintained", "bs_design.R"))

conditional_estimator <- function(data, strategy, alpha = 0.1) {
  imbalanced_fit <- lm_robust(formula = CBO ~ Z, data = data, clusters = catchment_area)
  imbalanced <- imbalanced_fit$p.value["Z"] < alpha
  how_imbalanced <- imbalanced_fit$coefficients["Z"] / imbalanced_fit$coefficients["(Intercept)"]
  formula <- switch(
    strategy,
    "ignore" = weight_for_age ~ Z,
    "include" = weight_for_age ~ Z + CBO,
    "interact" = weight_for_age ~ Z + CBO + Z:CBO,
    "include if imbalanced" = if (imbalanced) weight_for_age ~ Z + CBO else weight_for_age ~ Z
  )
  cbind(
    tidy(lm_robust(formula = formula, data = data, clusters = catchment_area))[2, ],
    imbalanced = imbalanced,
    how_imbalanced = how_imbalanced
  )
}

ignore_CBO <- declare_estimator(
  handler = label_estimator(conditional_estimator), strategy = "ignore",
  inquiry = hh_weight_inquiry, label = "Ignore CBO"
)

include_CBO <- declare_estimator(
  handler = label_estimator(conditional_estimator), strategy = "include",
  inquiry = hh_weight_inquiry, label = "Include CBO"
)

interact_CBO <- declare_estimator(
  handler = label_estimator(conditional_estimator), strategy = "interact",
  inquiry = hh_weight_inquiry, label = "Interact CBO"
)

include_CBO_if_imbalanced <- declare_estimator(
  handler = label_estimator(conditional_estimator), strategy = "include if imbalanced",
  inquiry = hh_weight_inquiry, label = "Include CBO if imbalanced"
)

CBO_correlation <- 0

# redesign() leaves this column missing for the design it varied from, so the
# design's own value is kept here to fill it.
original_CBO_correlation <- CBO_correlation

fixed_pop_CBO <- declare_model(
  data = pop_data,
  catchment_area = modify_level(
    CBO = correlate(
      draw_handler = draw_binary, given = area_health,
      rho = CBO_correlation, prob = (.64 + .48) / 2
    )
  )
)

DM_replication_CBO_ind <-
  fixed_pop_CBO + pos_alive + pos_weight + cl_sampling + hh_sampling + assignment +
  reveal_outcomes_step + hh_weight_inquiry +
  ignore_CBO + include_CBO + interact_CBO + include_CBO_if_imbalanced

DM_replication_CBO_cor <- redesign(DM_replication_CBO_ind, CBO_correlation = .8)

diagnosis <- diagnose_design(
  DM_replication_CBO_ind,
  DM_replication_CBO_cor,
  diagnosands = declare_diagnosands(
    rmse_balanced = sqrt(mean((estimate[!imbalanced] - estimand[!imbalanced])^2)),
    rmse_imbalanced = sqrt(mean((estimate[imbalanced] - estimand[imbalanced])^2)),
    rmse = sqrt(mean((estimate - estimand)^2))
  ),
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- get_diagnosands(diagnosis) |>
  as_tibble() |>
  mutate(CBO_correlation = replace_na(CBO_correlation, original_CBO_correlation))

write_csv(tab, out_path("table_a21_bs_covariate_adjustment.csv"))

print(tab |> select(CBO_correlation, estimator, rmse, rmse_balanced, rmse_imbalanced))
