# blair_cooper_coppock_humphreys_2019/maintained/table_a1_simple_design.R
# Output: output/table_a1_simple_design.csv
# Depends on: helpers.R
# Description: Diagnosis of the simple confounded design of appendix section 1,
#   reporting RMSE, bias and mean estimand for OLS of Y on X.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

b <- 1

f_Y <- function(X, Z, b, e_y) b * X + Z + e_y

model <- declare_model(
  N = 1000,
  e_y = rnorm(N),
  e_x = rnorm(N),
  Z = rnorm(N),
  X = Z + e_x,
  Y = f_Y(X, Z, b, e_y)
)

inquiry <- declare_inquiry(
  effect = mean(f_Y(X, Z, b, e_y) - f_Y(X - 1, Z, b, e_y))
)

data_strategy <- declare_sampling(S = complete_rs(N = N, n = 100), filter = S == 1)

answer_strategy <- declare_estimator(Y ~ X, .method = lm_robust, inquiry = "effect")

design <- model + inquiry + data_strategy + answer_strategy

# The published table reports RMSE, bias and the mean estimand. The default
# diagnosands are a superset of those three, so all of them are computed and
# written out; the three the appendix prints are selected for display.
diagnosis <- diagnose_design(
  design,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a1_simple_design.csv")

print(tab |> select(inquiry, estimator, term, n_sims, rmse, bias, mean_estimand))
