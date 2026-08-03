# blair_cooper_coppock_humphreys_2019/maintained/table_a5_process_tracing.R
# Output: output/table_a5_process_tracing.csv, output/table_a6_process_tracing_correlated_clues.csv
# Depends on: helpers.R
# Description: Diagnoses of the process tracing design of appendix section 3.2.1,
#   first with independent clues and then across three values of the correlation
#   between clues under the hypothesis. Both tables come from one declaration,
#   so they share a script.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

pr_SIW_H <- .75
pr_SIW_not_H <- .25
pr_SMG_H <- .30
pr_SMG_not_H <- .05
rho_H <- 0
rho_not_H <- 0

calculate_posterior <- function(data, p_H, p_clue_found_H, p_clue_found_not_H, test, label) {
  clue_found <- data[, test]
  p_E_H <- if_else(clue_found, p_clue_found_H, 1 - p_clue_found_H)
  p_E_not_H <- if_else(clue_found, p_clue_found_not_H, 1 - p_clue_found_not_H)
  tibble(
    posterior_H = p_E_H * p_H / (p_E_H * p_H + p_E_not_H * (1 - p_H)),
    clue_found = clue_found
  )
}

joint_prob <- function(p1, p2, rho) {
  r <- rho * (p1 * p2 * (1 - p1) * (1 - p2))^.5
  c(`00` = (1 - p1) * (1 - p2) + r,
    `01` = p2 * (1 - p1) - r,
    `10` = p1 * (1 - p2) - r,
    `11` = p1 * p2 + r)
}

calculate_posterior_joint <- function(data, p_H, p_clue_1_found_H, p_clue_1_found_not_H,
                                      p_clue_2_found_H, p_clue_2_found_not_H,
                                      rho_H, rho_not_H, test) {
  clue_found <- data[, test]
  p_E_H <- joint_prob(p1 = p_clue_1_found_H, p2 = p_clue_2_found_H, rho = rho_H)[clue_found]
  p_E_not_H <- joint_prob(p1 = p_clue_1_found_not_H, p2 = p_clue_2_found_not_H, rho = rho_not_H)[clue_found]
  tibble(
    posterior_H = p_E_H * p_H / (p_E_H * p_H + p_E_not_H * (1 - p_H)),
    clue_found = clue_found
  )
}

process_tracing_design <-
  declare_model(
    N = 195,
    X = rbinom(N, 1, .3) == 1,
    causal_process = sample(
      c("X_causes_Y", "X_causes_not_Y", "Y_regardless", "not_Y_regardless"),
      N, replace = TRUE, prob = c(.2, .1, .2, .5)
    ),
    Y = (X & causal_process == "X_causes_Y") |
      (!X & causal_process == "X_causes_not_Y") |
      (causal_process == "Y_regardless")
  ) +
  declare_sampling(
    S = strata_rs(strata = (X == 1 & Y == 1), strata_n = c("FALSE" = 0, "TRUE" = 1)),
    filter = S == 1
  ) +
  declare_inquiry(did_X_cause_Y = causal_process == "X_causes_Y") +
  declare_step(
    SIW_SMG = sample(
      c("00", "01", "10", "11"), 1,
      prob = {
        if (causal_process == "X_causes_Y") {
          joint_prob(pr_SIW_H, pr_SMG_H, rho_H)
        } else {
          joint_prob(pr_SIW_not_H, pr_SMG_not_H, rho_not_H)
        }
      }
    ),
    SIW_observed = SIW_SMG == "10" | SIW_SMG == "11",
    SMG_observed = SIW_SMG == "01" | SIW_SMG == "11",
    handler = fabricate,
    label = "Correlated Clues"
  ) +
  declare_estimator(
    test = "SIW_observed", p_H = .5,
    p_clue_found_H = pr_SIW_H, p_clue_found_not_H = pr_SIW_not_H,
    label = "Straw in Wind", inquiry = "did_X_cause_Y",
    handler = label_estimator(calculate_posterior)
  ) +
  declare_estimator(
    test = "SMG_observed", p_H = .5,
    p_clue_found_H = pr_SMG_H, p_clue_found_not_H = pr_SMG_not_H,
    label = "Smoking gun", inquiry = "did_X_cause_Y",
    handler = label_estimator(calculate_posterior)
  ) +
  declare_estimator(
    test = "SIW_SMG", p_H = .5,
    p_clue_1_found_H = pr_SIW_H, p_clue_1_found_not_H = pr_SIW_not_H,
    p_clue_2_found_H = pr_SMG_H, p_clue_2_found_not_H = pr_SMG_not_H,
    rho_H = rho_H, rho_not_H = rho_not_H,
    label = "Update from both clues", inquiry = "did_X_cause_Y",
    handler = label_estimator(calculate_posterior_joint)
  )

process_tracing_diagnosands <- declare_diagnosands(
  bias = mean(posterior_H - estimand),
  rmse = sqrt(mean((posterior_H - estimand)^2)),
  mean_estimand = mean(estimand),
  mean_posterior = mean(posterior_H),
  sd_posterior = sd(posterior_H)
)

diagnosis <- diagnose_design(
  process_tracing_design,
  diagnosands = process_tracing_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a5_process_tracing.csv")

print(tab |> select(estimator, bias, rmse, mean_estimand, mean_posterior, sd_posterior))

# Correlated clues ----
process_tracing_designs <- redesign(process_tracing_design, rho_H = c(-0.32, 0, 0.32))

diagnosis_correlated <- diagnose_design(
  process_tracing_designs,
  diagnosands = process_tracing_diagnosands,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab_correlated <- write_diagnosands(
  diagnosis_correlated, "table_a6_process_tracing_correlated_clues.csv"
)

print(tab_correlated |> select(rho_H, estimator, rmse))
