# blair_cooper_coppock_humphreys_2019/maintained/table_a4_bayesian_descriptive.R
# Output: output/table_a4_bayesian_descriptive.csv
# Depends on: helpers.R
# Description: Diagnosis of the beta-binomial descriptive design of appendix
#   section 3.1.2 under flat and informative priors, at sample sizes 10 and 100.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

# The published design varies the sample size by redesign(). In the deposited
# code the sample size was written as a literal inside declare_sampling(), so
# the redesigned n = 100 design drew ten units like the n = 10 design and the
# two rows of the published table are the same diagnosis twice. Naming the
# sample size makes redesign() reach it, which is the intent the published
# column headings state.
sample_n <- 10

population <- declare_model(
  N = 1000,
  noise = rnorm(N, -.1, .05),
  prob_success = pnorm(noise),
  success = rbinom(N, 1, prob_success)
)

inquiry <- declare_inquiry(success_probability = mean(prob_success))

sampling <- declare_sampling(S = complete_rs(N = N, n = sample_n), filter = S == 1)

beta_binom <- function(data, alpha_0, beta_0) {
  n_successes <- with(data, sum(success))
  n_trials <- nrow(data)
  alpha <- n_successes + alpha_0 - 1
  beta <- n_trials - n_successes + beta_0 - 1
  tibble(
    post_mean = alpha / (alpha + beta),
    prior_sd = sqrt((alpha_0 * beta_0) / (((alpha_0 + beta_0)^2) * (alpha_0 + beta_0 + 1))),
    prior_mean = alpha_0 / (alpha_0 + beta_0),
    post_sd = sqrt((alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1)))
  )
}

estimator_flat_priors <- declare_estimator(
  handler = label_estimator(beta_binom),
  alpha_0 = 1, beta_0 = 1,
  inquiry = "success_probability",
  label = "flat priors"
)

estimator_info_priors <- declare_estimator(
  handler = label_estimator(beta_binom),
  alpha_0 = 10, beta_0 = 10,
  inquiry = "success_probability",
  label = "informative priors"
)

bayesian_design <-
  population + inquiry + sampling + estimator_flat_priors + estimator_info_priors

# The published table reports the five diagnosands below the priors. The article
# also states the mean and standard deviation of each prior in the text, so those
# two are computed here as well rather than left without a source.
bayesian_diagnosands <- declare_diagnosands(
  mean_est = mean(post_mean),
  mean_sd = mean(post_sd),
  bias = mean(post_mean - estimand),
  mean_shift = mean(post_mean - prior_mean),
  sd_shift = mean(post_sd - prior_sd),
  prior_mean = mean(prior_mean),
  prior_sd = mean(prior_sd)
)

bayesian_designs <- redesign(bayesian_design, sample_n = c(10, 100))

diagnosis <- diagnose_design(
  bayesian_designs,
  diagnosands = bayesian_diagnosands,
  sims = published_sims,
  bootstrap_sims = FALSE
)

tab <- write_diagnosands(diagnosis, "table_a4_bayesian_descriptive.csv")

print(tab |> select(sample_n, estimator, mean_est, mean_sd, bias, mean_shift, sd_shift,
                    prior_mean, prior_sd))
