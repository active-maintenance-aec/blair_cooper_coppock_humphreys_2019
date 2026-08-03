# blair_cooper_coppock_humphreys_2019/maintained/table_a3_survey_design.R
# Output: output/table_a3_survey_design.csv
# Depends on: helpers.R
# Description: Diagnosis of the likely-voter survey design of appendix section
#   3.1.1, reporting the bias of a sample mean taken among self-reported likely
#   voters for the level of support among actual voters.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

survey_design <-
  declare_model(
    N = 1000,
    latent_voting = rnorm(N),
    latent_HRC_support = .2 * latent_voting + rnorm(N) - .2,
    voter = rbinom(N, 1, prob = pnorm(latent_voting)),
    HRC_supporter = rbinom(N, 1, prob = pnorm(latent_HRC_support)),
    likely_voter = rbinom(N, 1, prob = pnorm(latent_voting + 0.75))
  ) +
  declare_inquiry(true_support = mean(HRC_supporter), subset = voter == 1) +
  declare_sampling(S = complete_rs(N = N, n = 500), filter = S == 1) +
  declare_estimator(
    HRC_supporter ~ 1,
    .method = lm_robust,
    subset = likely_voter == 1,
    term = "(Intercept)",
    inquiry = "true_support"
  )

# The published table reports bias only; the default diagnosands include it.
diagnosis <- diagnose_design(
  survey_design,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a3_survey_design.csv")

print(tab |> select(inquiry, estimator, n_sims, bias))
