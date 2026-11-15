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

# Scripts named in MAINTAINED_SKIP (space-separated, repository-relative) are not run.
# The shared CI sets it for scripts whose peak memory exceeds the 16 GB runner; their
# committed outputs stand, and later scripts read those. Unset, everything runs.
maintained_skip <- strsplit(trimws(Sys.getenv("MAINTAINED_SKIP")), "[[:space:]]+")[[1]]
run_script <- function(...) {
  path <- file.path(...)
  if (path %in% maintained_skip) {
    message("MAINTAINED_SKIP: skipped ", path)
    return(invisible())
  }
  # The time and path go to the log before the script runs, so a run killed for memory
  # still says which script it was in. message() writes to stderr, which is unbuffered;
  # print() output can still sit in a buffer when the process is killed.
  message("run_all: starting ", path, " ", format(Sys.time(), "%H:%M:%S"))
  source(here::here(...))
}

# Deposited archive ----
# Downloads from Dataverse on a fresh clone, unpacks it, and verifies both the
# archive file and its members either way.
source(here::here("download_original.R"))

# Article tables and figures ----
run_script("maintained", "table_2_tool_census.R")
run_script("maintained", "table_3_declaration.R")
run_script("maintained", "figure_1_experimental_designs.R")

# Appendix tables ----
run_script("maintained", "table_a1_simple_design.R")
run_script("maintained", "table_a2_working_example_census.R")
run_script("maintained", "table_a3_survey_design.R")
run_script("maintained", "table_a4_bayesian_descriptive.R")
run_script("maintained", "table_a5_process_tracing.R")
run_script("maintained", "table_a7_qca.R")
run_script("maintained", "table_a12_nested_analysis.R")
run_script("maintained", "table_a15_model_based_estimands.R")
run_script("maintained", "table_a16_matching.R")
run_script("maintained", "table_a17_regression_discontinuity.R")
run_script("maintained", "table_a18_discovery.R")
run_script("maintained", "table_a18_discovery_intended.R")
run_script("maintained", "table_a19_bs_diagnosis.R")
run_script("maintained", "table_a20_bs_exaggeration.R")
run_script("maintained", "table_a21_bs_covariate_adjustment.R")

# Article Figure 2 ----
# Reads the per-simulation estimates written by the appendix section 4.5 script,
# so it runs after it.
run_script("maintained", "figure_2_bs_sampling_distributions.R")

# The deposited code, run in a scratch copy ----
# Records, per script and for both the deposit as shipped and a copy stripped to
# data plus code, whether it runs and where it stops.
run_script("ground_truth", "run_archive.R")

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
run_script("ground_truth", "build_ground_truth.R")

# In-text claims ----
# Prints every published claim beside the number the pipeline gives for it, in
# the order the article states them.
run_script("maintained", "in_text_claims.R")

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ and
# original_extracted/ were intact before anything ran. Nothing above writes to
# either, and this second pass is what demonstrates it rather than assuming it.
# verify_original.R is sourced directly, not download_original.R, because
# download_original.R rebuilds original_extracted/ from the zip and would erase
# the damage this step exists to find.
source(here::here("verify_original.R"))
