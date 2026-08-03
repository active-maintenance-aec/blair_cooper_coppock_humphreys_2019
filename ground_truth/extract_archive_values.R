# blair_cooper_coppock_humphreys_2019/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: the unpacked deposit (original_extracted/, or ARCHIVE_RUN_DIR)
# Description: Read the numbers the deposit itself produced, one row per claim.
#
#   The deposit's scripts cannot execute under the current DeclareDesign, so
#   there is no run to read output from. What the deposit does ship is the saved
#   diagnosis object behind every table it prints, one .rds per diagnosis at the
#   published 5000 simulations, plus the coding sheet behind the article's own
#   Table 2. Those are the deposit's own numbers at full precision, and they are
#   what this script reads. Nothing here recomputes anything.
#
#   The archive directory is taken from ARCHIVE_RUN_DIR when that is set, and
#   otherwise from the copy download_original.R unpacks, so the script runs for
#   anyone who has cloned the repository and run run_all.R.

library(tidyverse)
library(here)

here::i_am("ground_truth/extract_archive_values.R")

archive_dir <- Sys.getenv(
  "ARCHIVE_RUN_DIR",
  unset = here::here("original_extracted", "replication_archive")
)

stopifnot(dir.exists(file.path(archive_dir, "rds_files")))

# Claim keys ----
# A claim key is the table, then the identifiers that pick out a row of that
# table, then the quantity, joined by a pipe. The archive and the rewrite label
# their identifier columns differently and carry the same identifier values, so
# the same key is built on both sides from whichever columns hold them.
long_claims <- function(dat, table_label, id_cols, value_cols) {
  dat |>
    select(all_of(c(id_cols, value_cols))) |>
    mutate(across(all_of(id_cols), as.character)) |>
    pivot_longer(all_of(value_cols), names_to = "quantity", values_to = "value") |>
    unite("identifier", all_of(c(id_cols, "quantity")), sep = "|") |>
    mutate(claim_id = str_c(table_label, identifier, sep = "|")) |>
    select(claim_id, value)
}

read_deposited_diagnosands <- function(stem) {
  readRDS(file.path(archive_dir, "rds_files", str_c(stem, "_5000_sims.rds")))$diagnosands_df |>
    as_tibble()
}

read_deposited_simulations <- function(stem) {
  readRDS(file.path(archive_dir, "rds_files", str_c(stem, "_5000_sims.rds")))$simulations_df |>
    as_tibble()
}

# The deposited diagnosis objects ----
archive_diagnosands <- bind_rows(
  long_claims(
    read_deposited_diagnosands("1_simple_design"),
    "a1", character(0), c("rmse", "bias", "mean_estimand")
  ),
  long_claims(
    read_deposited_diagnosands("2_design_census"),
    "a2", "estimator_label", c("power", "bias", "rmse", "coverage", "mean_estimand")
  ),
  long_claims(
    read_deposited_diagnosands("3_1_survey_designs"),
    "a3", character(0), "bias"
  ),
  long_claims(
    read_deposited_diagnosands("3_2_Bayesian_descriptive_inference"),
    "a4", c("n", "estimator_label"),
    c("mean_est", "mean_sd", "bias", "mean_shift", "sd_shift")
  ),
  long_claims(
    read_deposited_diagnosands("3_3_process_tracing1"),
    "a5", "estimator_label",
    c("bias", "rmse", "mean_estimand", "mean_posterior", "sd_posterior")
  ),
  long_claims(
    read_deposited_diagnosands("3_3_process_tracing2"),
    "a6", c("rho_H", "estimator_label"), "rmse"
  ),
  long_claims(
    read_deposited_diagnosands("3_4_QCA1"),
    "a9", "estimator_label", c("correct_rate", "failure_rate")
  ),
  long_claims(
    read_deposited_diagnosands("3_4_QCA2"),
    "a10", c("error_rate", "N", "estimator_label"), c("correct_rate", "failure_rate")
  ),
  long_claims(
    read_deposited_diagnosands("3_4_QCA3"),
    "a11", c("error_rate", "N", "estimator_label"), c("correct_rate", "failure_rate")
  ),
  long_claims(
    read_deposited_diagnosands("3_5_nested1"),
    "a12", c("rho", "estimator_label"),
    c("bias", "rmse", "mean_estimate", "mean_estimand")
  ),
  long_claims(
    read_deposited_diagnosands("3_6_model_based_estimands"),
    "a15", c("design_label", "estimand_label"),
    c("mean_estimand", "bias", "rmse", "power", "coverage")
  ),
  long_claims(
    read_deposited_diagnosands("3_7_matching"),
    "a16", "estimator_label", "bias"
  ),
  long_claims(
    read_deposited_diagnosands("3_8_regression_discontinuity"),
    "a17", character(0), c("bias", "rmse", "power", "coverage")
  ),
  long_claims(
    read_deposited_diagnosands("3_10_discovery"),
    "a18", "estimator_label",
    c("bias", "RMSE", "frequency", "false_pos", "false_neg")
  ),
  long_claims(
    read_deposited_diagnosands("4_B_and_S_replication1"),
    "a19", "estimator_label", c("mean_estimand", "mean_estimate", "bias", "power")
  ),
  long_claims(
    read_deposited_diagnosands("4_B_and_S_replication2"),
    "a20", c("design_label", "estimator_label"), c("exaggeration_ratio", "power")
  ),
  long_claims(
    read_deposited_diagnosands("4_B_and_S_replication3"),
    "a21", c("design_label", "estimator_label"),
    c("rmse", "rmse_balanced", "rmse_imbalanced")
  )
)

# Appendix section 3.2.3 reports the share of simulations that reach a given
# model rather than a diagnosand, so those two tables are counted out of the
# simulations the deposited diagnosis holds.
share_reaching_model <- function(stem, table_label, id_cols) {
  simulations <- read_deposited_simulations(stem)
  n_sims <- n_distinct(simulations$sim_ID)
  simulations |>
    count(across(all_of(c(id_cols, "estimator_label"))), name = "n_reached") |>
    filter(estimator_label == "New Model") |>
    mutate(prop_sims = n_reached / n_sims) |>
    long_claims(table_label, id_cols, "prop_sims")
}

archive_shares <- bind_rows(
  share_reaching_model(
    "3_5_nested2", "a13", c("model_testing_effort", "model_building_effort")
  ),
  share_reaching_model(
    "3_5_nested3", "a14", c("LNA_threshold", "SNA_threshold_1", "SNA_threshold_2")
  )
)

# Figure 1 of the article ----
# The deposit saves the two diagnoses the figure draws from. Its own scripts
# write no numbers for it, so these are the only values behind the plotted lines.
archive_figure_1 <- bind_rows(
  long_claims(
    read_deposited_diagnosands("3_9_experimental_3x1") |>
      filter(estimand_label == "ate_Y_2_1"),
    "figure_1", "estimand_label", c("bias", "rmse", "power")
  ),
  long_claims(
    read_deposited_diagnosands("3_9_experimental_2x2") |>
      filter(design_label == "factorial_5", estimand_label %in% c("ate_A", "interaction")),
    "figure_1", "estimand_label", c("bias", "rmse", "power")
  )
)

# The article's Table 2 ----
# The deposit ships no script for this table. The coding sheet behind it carries
# a "Sum:" row the authors computed in the spreadsheet, and that is the deposit's
# own count wherever it is filled in.
census <- readxl::read_excel(
  file.path(archive_dir, "tool_census", "tool_census.xlsx"),
  sheet = "Included",
  col_types = "text"
)

archive_census <- census |>
  filter(Tool == "Sum:") |>
  select(-Tool, -URL, -Comments) |>
  pivot_longer(everything(), names_to = "column", values_to = "value") |>
  mutate(
    claim_id = str_c("table_2", column, "n_tools_able", sep = "|"),
    value = as.numeric(value)
  ) |>
  select(claim_id, value)

archive_values <- bind_rows(
  archive_diagnosands, archive_shares, archive_figure_1, archive_census
) |>
  rename(value_script = value)

stopifnot(!anyDuplicated(archive_values$claim_id))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(str_glue("Archive values extracted: {nrow(archive_values)} claims."))
