# blair_cooper_coppock_humphreys_2019/maintained/table_a19_bs_diagnosis.R
# Output: output/table_a19_bs_diagnosis.csv,
#         output/figure_2_bs_sampling_distributions.csv
# Depends on: helpers.R, bs_design.R
# Description: The diagnosis of the Bjorkman and Svensson (2009) design
#   replication printed in appendix section 4.5, and the per-simulation estimates
#   that Figure 2 of the article plots. Figure 2 and this table are two readings
#   of one diagnosis, so they are produced together rather than diagnosed twice.
#   The design itself is declared in bs_design.R, which two later appendix
#   scripts also source.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

source(here::here("maintained", "bs_design.R"))

diagnosis <- diagnose_design(
  bjorkman_svensson_design,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- write_diagnosands(diagnosis, "table_a19_bs_diagnosis.csv")

print(tab |> select(inquiry, mean_estimand, mean_estimate, bias, power))

# Figure 2 plots the sampling distribution of each estimator, so the values it
# needs are the simulation-level estimates rather than the diagnosands.
simulations <- get_simulations(diagnosis) |>
  as_tibble() |>
  select(sim_ID, inquiry, estimator, term, estimate, estimand, std.error, p.value)

write_csv(simulations, out_path("figure_2_bs_sampling_distributions.csv"))
