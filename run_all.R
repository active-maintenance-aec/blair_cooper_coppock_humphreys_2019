# blair_cooper_coppock_humphreys_2019/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then the article's tables and figures, then the appendix's, then the ground
# truth, then the deposit's verification again. Every script is self-contained
# and can also be run on its own in a fresh session.
#
# The diagnoses use the published 5000 simulations and 5000 bootstrap replicates
# throughout. Where the platform allows forking the run takes a little over an
# hour; without it the run is sequential, slower, and gives the same numbers.
#
# Order note: figure_1_experimental_designs.R must run before the QCA appendix
# table. QCA loads the declared package, which replaces base::rbind.data.frame()
# for the rest of the session with a version the design library's three-arm
# estimator cannot use. The figure script asserts the condition rather than
# trusting this ordering.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone, unpacks it, and verifies both the
# archive file and its members either way.
source(here::here("download_original.R"))

# Article tables and figures ----
source(here::here("maintained", "table_2_tool_census.R"))
source(here::here("maintained", "table_3_declaration.R"))
source(here::here("maintained", "figure_1_experimental_designs.R"))

# Appendix tables ----
source(here::here("maintained", "table_a1_simple_design.R"))
source(here::here("maintained", "table_a2_working_example_census.R"))
source(here::here("maintained", "table_a3_survey_design.R"))
source(here::here("maintained", "table_a4_bayesian_descriptive.R"))
source(here::here("maintained", "table_a5_process_tracing.R"))
source(here::here("maintained", "table_a7_qca.R"))
source(here::here("maintained", "table_a12_nested_analysis.R"))
source(here::here("maintained", "table_a15_model_based_estimands.R"))
source(here::here("maintained", "table_a16_matching.R"))
source(here::here("maintained", "table_a17_regression_discontinuity.R"))
source(here::here("maintained", "table_a18_discovery.R"))
source(here::here("maintained", "table_a19_bs_diagnosis.R"))
source(here::here("maintained", "table_a20_bs_exaggeration.R"))
source(here::here("maintained", "table_a21_bs_covariate_adjustment.R"))

# Article Figure 2 ----
# Reads the per-simulation estimates written by the appendix section 4.5 script,
# so it runs after it.
source(here::here("maintained", "figure_2_bs_sampling_distributions.R"))

# The deposited code, run in a scratch copy ----
# Records, per script and for both the deposit as shipped and a copy stripped to
# data plus code, whether it runs and where it stops.
source(here::here("ground_truth", "run_archive.R"))

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
source(here::here("ground_truth", "build_ground_truth.R"))

# In-text claims ----
# Prints every published claim beside the number the pipeline gives for it, in
# the order the article states them.
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ and
# original_extracted/ were intact before anything ran. Nothing above writes to
# either, and this second pass is what demonstrates it rather than assuming it.
# verify_original.R is sourced directly, not download_original.R, because
# download_original.R rebuilds original_extracted/ from the zip and would erase
# the damage this step exists to find.
source(here::here("verify_original.R"))
