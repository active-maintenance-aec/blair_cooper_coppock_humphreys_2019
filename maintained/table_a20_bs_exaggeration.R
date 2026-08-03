# blair_cooper_coppock_humphreys_2019/maintained/table_a20_bs_exaggeration.R
# Output: output/table_a20_bs_exaggeration.csv
# Depends on: helpers.R, bs_design.R
# Description: Appendix section 4.6, comparing the power and the exaggeration
#   ratio of the Bjorkman and Svensson design at its original 50 catchment areas
#   with a replication at 187.

source(here::here("maintained", "helpers.R"))

set.seed(published_seed)

source(here::here("maintained", "bs_design.R"))

# redesign() records the parameter it varied and leaves it missing for the design
# it was varied from, so the original design's own value is kept here to fill it.
original_catchment_areas <- N_catchment_areas

RPP_replication <- redesign(bjorkman_svensson_design, N_catchment_areas = 95 + 92)

diagnosis <- diagnose_design(
  bjorkman_svensson_design,
  RPP_replication,
  diagnosands = declare_diagnosands(
    exaggeration_ratio = mean(abs(estimate[p.value < 0.1]) / abs(estimand[p.value < 0.1])),
    power = mean(p.value < 0.05)
  ),
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

tab <- get_diagnosands(diagnosis) |>
  as_tibble() |>
  mutate(N_catchment_areas = replace_na(N_catchment_areas, original_catchment_areas))

write_csv(tab, out_path("table_a20_bs_exaggeration.csv"))

print(tab |> select(design, N_catchment_areas, estimator, exaggeration_ratio, power))
