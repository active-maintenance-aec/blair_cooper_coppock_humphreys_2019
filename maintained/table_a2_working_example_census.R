# blair_cooper_coppock_humphreys_2019/maintained/table_a2_working_example_census.R
# Output: output/table_a2_working_example_census.csv
# Depends on: helpers.R
# Description: Diagnosis of the blocked working-example design of appendix section
#   2.1, comparing the difference in means, block fixed effects and inverse
#   probability weighted block fixed effects estimators.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

# Ten units are treated in every block, so a unit's assignment probability is
# 10 divided by its own block size. Pre-1.0 DeclareDesign attached that
# probability to the data automatically; here it is computed explicitly from the
# same block sizes and block_m that generate the assignment, which is what the
# IPW-BFE estimator needs.
block_m_vector <- function(block) rep(10, length(unique(block)))

design <-
  declare_model(
    block = add_level(
      N = 1000,
      block_size = sample(c(25, 50), N, TRUE),
      block_effect = rnorm(N, block_size / 100, .1)
    ),
    individual = add_level(
      N = block_size,
      noise = rnorm(N)
    )
  ) +
  declare_potential_outcomes(formula = Y ~ block_effect * Z + noise) +
  declare_sampling(S = cluster_rs(clusters = block, n = 5), filter = S == 1) +
  declare_assignment(
    Z = block_ra(blocks = block, block_m = block_m_vector(block)),
    Z_cond_prob = obtain_condition_probabilities(
      assignment = Z, blocks = block, block_m = block_m_vector(block)
    )
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_estimator(
    Y ~ Z,
    .method = lm_robust, se_type = "classical", label = "DIM", inquiry = "ATE"
  ) +
  declare_estimator(
    Y ~ Z + block,
    .method = lm_robust, se_type = "classical", label = "BFE", inquiry = "ATE"
  ) +
  declare_estimator(
    Y ~ Z + block,
    weights = 1 / Z_cond_prob,
    .method = lm_robust, se_type = "classical", label = "IPW-BFE", inquiry = "ATE"
  )

diagnosis <- diagnose_design(
  design,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a2_working_example_census.csv")

print(tab |> select(estimator, term, n_sims, power, bias, rmse, coverage))
