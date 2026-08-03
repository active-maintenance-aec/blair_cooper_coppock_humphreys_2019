# blair_cooper_coppock_humphreys_2019/maintained/table_3_declaration.R
# Output: output/table_3_declaration.csv
# Depends on: helpers.R
# Description: Table 3 of the article is a code listing rather than a set of
#   results: it walks through declaring a design, simulating one draw of it, and
#   diagnosing it. This script runs that listing, and the near identical listing
#   the deposit keeps in appendix_code.R, and records the bias each returns, so
#   that the printed code is checked to execute and to give the answer the text
#   claims for it.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

# Table 3, top panel: the declaration ----
p_U <- declare_model(N = 200, u = rnorm(N))
f_Y <- declare_potential_outcomes(Y ~ Z + u)
Q <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
p_S <- declare_sampling(S = complete_rs(N = N, n = 100), filter = S == 1)
p_Z <- declare_assignment(Z = complete_ra(N = N, m = 50))
R <- declare_measurement(Y = reveal_outcomes(Y ~ Z))
A <- declare_estimator(Y ~ Z, inquiry = "ATE")

design <- p_U + f_Y + Q + p_S + p_Z + R + A

# Table 3, middle panel: one simulated draw ----
u <- p_U()
D <- f_Y(u)
a_M <- Q(D)
d <- R(p_Z(p_S(D)))
a_A <- A(d)
one_draw_error <- a_A[["estimate"]] - a_M[["estimand"]]

# Table 3, bottom panel: the diagnosis ----
bias <- declare_diagnosands(bias = mean(estimate - estimand))

m <- 1000

diagnosis <- diagnose_design(design, sims = m, diagnosands = bias)

# The deposit's appendix_code.R repeats the same walkthrough at a larger
# population and a smaller number of simulations.
population_appendix <- declare_model(N = 1000, u = rnorm(N))
potential_outcomes_appendix <- declare_potential_outcomes(Y ~ 0.25 * Z + u)
inquiry_appendix <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
sampling_appendix <- declare_sampling(S = complete_rs(N = N, n = 100), filter = S == 1)
assignment_appendix <- declare_assignment(Z = complete_ra(N = N, m = 50))
reveal_appendix <- declare_measurement(Y = reveal_outcomes(Y ~ Z))
estimator_appendix <- declare_estimator(Y ~ Z, .method = lm_robust, inquiry = "ATE")

design_appendix <- population_appendix + potential_outcomes_appendix + inquiry_appendix +
  sampling_appendix + assignment_appendix + reveal_appendix + estimator_appendix

diagnosis_appendix <- diagnose_design(
  design = design_appendix, diagnosands = bias, sims = 500, bootstrap_sims = FALSE
)

tab <- bind_rows(
  get_diagnosands(diagnosis) |> as_tibble() |> mutate(listing = "Table 3", n_sims_run = m),
  get_diagnosands(diagnosis_appendix) |> as_tibble() |>
    mutate(listing = "appendix_code.R", n_sims_run = 500)
) |>
  mutate(one_draw_error = c(one_draw_error, NA_real_)) |>
  select(listing, inquiry, estimator, n_sims_run, bias, one_draw_error)

write_csv(tab, out_path("table_3_declaration.csv"))

print(tab)
