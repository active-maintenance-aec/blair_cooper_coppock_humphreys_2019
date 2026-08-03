# blair_cooper_coppock_humphreys_2019/ground_truth/build_ground_truth.R
# Output: ground_truth/blair_cooper_coppock_humphreys_2019_ground_truth.csv
# Depends on: ground_truth/published_claims.csv, ground_truth/defect_loci.csv,
#             ground_truth/extract_archive_values.R, maintained/output/
# Description: Build the comparison table. Every published number is read from
#   the article or its appendix and lives in published_claims.csv, which is the
#   only file here carrying a hand-typed value. The deposit's own numbers come
#   from extract_archive_values.R and the rewrite's from maintained/output/, both
#   read by script. Nothing is transcribed from a run and no verdict is typed.
#
#   The build stops rather than reporting a gap: if a published float has no
#   claim, if a claim that should have a computed counterpart finds none, or if a
#   disagreeing row has no recorded defect locus.

library(tidyverse)
library(here)

here::i_am("ground_truth/build_ground_truth.R")

published <- read_csv(here::here("ground_truth", "published_claims.csv"), show_col_types = FALSE)
defect_loci <- read_csv(here::here("ground_truth", "defect_loci.csv"), show_col_types = FALSE)

# The archive's numbers, regenerated in this session ----
# A generated file the ground truth reads drifts exactly as a typed column
# drifts, so it is rebuilt here rather than read from the last run's copy.
source(here::here("ground_truth", "extract_archive_values.R"))

archive_values <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)

# The rewrite's numbers ----
out_path <- function(...) here::here("maintained", "output", ...)

read_output <- function(file) read_csv(out_path(file), show_col_types = FALSE)

bayesian <- read_output("table_a4_bayesian_descriptive.csv")
bs_diagnosis <- read_output("table_a19_bs_diagnosis.csv")
qca_diagnosis <- read_output("table_a9_qca_diagnosis.csv")
qca_error_and_n <- read_output("table_a10_qca_error_rate_and_n.csv")
exaggeration <- read_output("table_a20_bs_exaggeration.csv")

rewrite_diagnosands <- bind_rows(
  long_claims(read_output("table_a1_simple_design.csv"),
              "a1", character(0), c("rmse", "bias", "mean_estimand")),
  long_claims(read_output("table_a2_working_example_census.csv"),
              "a2", "estimator", c("power", "bias", "rmse", "coverage", "mean_estimand")),
  long_claims(read_output("table_a3_survey_design.csv"), "a3", character(0), "bias"),
  long_claims(bayesian, "a4", c("sample_n", "estimator"),
              c("mean_est", "mean_sd", "bias", "mean_shift", "sd_shift",
                "prior_mean", "prior_sd")),
  long_claims(read_output("table_a5_process_tracing.csv"),
              "a5", "estimator",
              c("bias", "rmse", "mean_estimand", "mean_posterior", "sd_posterior")),
  long_claims(read_output("table_a6_process_tracing_correlated_clues.csv"),
              "a6", c("rho_H", "estimator"), "rmse"),
  long_claims(qca_diagnosis, "a9", "estimator", c("correct_rate", "failure_rate")),
  long_claims(qca_error_and_n,
              "a10", c("error_rate", "N", "estimator"), c("correct_rate", "failure_rate")),
  long_claims(read_output("table_a11_qca_with_least_squares.csv"),
              "a11", c("error_rate", "N", "estimator"), c("correct_rate", "failure_rate")),
  long_claims(read_output("table_a12_nested_confounding.csv"),
              "a12", c("rho", "estimator"),
              c("bias", "rmse", "mean_estimate", "mean_estimand")),
  long_claims(read_output("table_a13_nested_effort.csv"),
              "a13", c("model_testing_effort", "model_building_effort"), "prop_sims"),
  long_claims(read_output("table_a14_nested_thresholds.csv"),
              "a14", c("LNA_threshold", "SNA_threshold_1", "SNA_threshold_2"), "prop_sims"),
  long_claims(read_output("table_a15_model_based_estimands.csv"),
              "a15", c("design", "inquiry"),
              c("mean_estimand", "bias", "rmse", "power", "coverage")),
  long_claims(read_output("table_a16_matching.csv"), "a16", "estimator", c("bias", "rmse")),
  long_claims(read_output("table_a17_regression_discontinuity.csv"),
              "a17", character(0), c("bias", "rmse", "power", "coverage")),
  long_claims(read_output("table_a18_discovery.csv"),
              "a18", "estimator", c("bias", "RMSE", "frequency", "false_pos", "false_neg")),
  long_claims(bs_diagnosis, "a19", "estimator",
              c("mean_estimand", "mean_estimate", "bias", "power", "coverage")),
  long_claims(exaggeration, "a20", c("design", "estimator"),
              c("exaggeration_ratio", "power")),
  long_claims(read_output("table_a21_bs_covariate_adjustment.csv"),
              "a21", c("design", "estimator"), c("rmse", "rmse_balanced", "rmse_imbalanced"))
)

# Tables printed as cells rather than as diagnosands ----
qca_draw <- read_output("table_a7_qca_simulated_data.csv") |>
  mutate(
    row = str_c(A, B, Y_A_0_B_0, Y_A_1_B_0, Y_A_0_B_1, Y_A_1_B_1, Y, mistake, Y_obs, sep = "|"),
    claim_id = str_c("a7", ID, "row", sep = "|")
  ) |>
  select(claim_id, value_chr = row)

qca_estimates <- read_output("table_a8_qca_estimates.csv") |>
  mutate(claim_id = str_c("a8", estimator, "estimate", sep = "|")) |>
  select(claim_id, value_chr = estimate)

census <- read_output("table_2_tool_census.csv") |>
  mutate(claim_id = str_c("table_2", coalesce(column, row_label), "n_tools_able", sep = "|")) |>
  select(claim_id, value = n_tools_able)

census_counts <- read_output("table_2_tool_census_counts.csv") |>
  mutate(claim_id = str_c("text", quantity, sep = "|")) |>
  select(claim_id, value = n)

# Figure 1 prints no numbers, so its row reads the estimates the figure is drawn
# from at the interaction size the sentence names.
figure_1 <- read_output("figure_1_experimental_designs.csv")

main_effect_bias <- figure_1 |>
  filter(diagnosand == "Bias", estimator %in% c("DIM (Z_2 - Z_1)", "No_Interaction"))

figure_1_claims <- tibble(
  claim_id = "figure_1|zero_interaction_bias",
  value = main_effect_bias |> filter(interaction == 0) |> pull(value) |> abs() |> max()
)

figure_2_claims <- read_output("figure_2_bs_sampling_distributions.csv") |>
  summarize(value = mean(estimate, na.rm = TRUE), .by = estimator) |>
  mutate(claim_id = str_c("figure_2", estimator, "mean_estimate", sep = "|")) |>
  select(claim_id, value)

rewrite_values <- bind_rows(
  rewrite_diagnosands, qca_draw, qca_estimates,
  census, census_counts, figure_1_claims, figure_2_claims
)

# Claims derived from cells the tables already print ----
# Each is written once, as a function of a lookup from claim id to value, and
# evaluated against the rewrite and against the deposit in turn. Writing the
# derivation once is what stops a number the article states in prose from
# disagreeing with the table cell it restates; evaluating it against both sources
# is what shows whether a number the article states is reachable from the deposit
# at all. A claim naming a quantity a source does not carry evaluates to NA for
# that source rather than to a verdict.
lookup_values <- function(values) {
  lookup <- values |> filter(!is.na(value)) |> select(claim_id, value) |> deframe()
  function(id) if (id %in% names(lookup)) lookup[[id]] else NA_real_
}

derived_claims <- list(
  `a2|prose|pate` = \(v) v("a2|BFE|mean_estimand"),
  `a4|prose|flat_prior_mean` = \(v) v("a4|10|flat priors|prior_mean"),
  `a4|prose|flat_prior_sd` = \(v) v("a4|10|flat priors|prior_sd"),
  `a4|prose|info_prior_mean` = \(v) v("a4|10|informative priors|prior_mean"),
  `a4|prose|info_prior_sd` = \(v) v("a4|10|informative priors|prior_sd"),
  `a4|prose|sd_reduction` = \(v) 100 *
    (v("a4|10|flat priors|mean_sd") - v("a4|10|informative priors|mean_sd")) /
    v("a4|10|flat priors|mean_sd"),
  `a4|prose|bias_increase` = \(v) 100 *
    (abs(v("a4|10|informative priors|bias")) - abs(v("a4|10|flat priors|bias"))) /
    abs(v("a4|10|flat priors|bias")),
  `text|qca_correct_rate_prose` = \(v) v("a9|Classic Quine-McCluskey|correct_rate"),
  `a10|prose|perfect_recovery` = \(v) min(c(
    v("a10|0|50|Classic Quine-McCluskey|correct_rate"),
    v("a10|0|50|Consistency Cubes|correct_rate")
  )),
  `a19|Mortality estimate|prose_mean_estimand` = \(v) v("a19|Mortality estimate|mean_estimand"),
  `a19|Mortality estimate|prose_coverage` = \(v) v("a19|Mortality estimate|coverage"),
  `a19|Mortality estimate|prose_power` = \(v) v("a19|Mortality estimate|power"),
  `a20|prose|mortality_exaggeration` = \(v) v("a20|bjorkman_svensson_design|Mortality estimate|exaggeration_ratio"),
  `a20|prose|weight_exaggeration` = \(v) v("a20|bjorkman_svensson_design|Weight estimate|exaggeration_ratio"),
  `a20|prose|rpp_mortality_exaggeration` = \(v) v("a20|RPP_replication|Mortality estimate|exaggeration_ratio")
)

evaluate_derived <- function(values) {
  look_up <- lookup_values(values)
  imap(derived_claims, \(fn, id) tibble(claim_id = id, value = fn(look_up))) |>
    list_rbind()
}

# Claims about shape, sign and order ----
all_true <- function(...) {
  parts <- c(...)
  if (anyNA(parts)) NA else all(parts)
}

descriptive_claims <- list(
  `a2|descriptive|dim_bfe_negative_bias` = \(v) all_true(v("a2|DIM|bias") < 0, v("a2|BFE|bias") < 0),
  `a2|descriptive|ipw_undercoverage` = \(v) all_true(
    v("a2|IPW-BFE|power") > v("a2|BFE|power"), v("a2|IPW-BFE|coverage") < 0.95
  ),
  `a4|descriptive|informative_more_certain` = \(v) all_true(
    v("a4|10|informative priors|mean_sd") < v("a4|10|flat priors|mean_sd"),
    abs(v("a4|10|informative priors|bias")) > abs(v("a4|10|flat priors|bias"))
  ),
  `a5|descriptive|smoking_gun_sd` = \(v) all_true(
    v("a5|Smoking gun|sd_posterior") < v("a5|Straw in Wind|sd_posterior")
  ),
  `a5|descriptive|siw_lower_rmse` = \(v) all_true(
    v("a5|Straw in Wind|rmse") < v("a5|Smoking gun|rmse")
  ),
  `a5|descriptive|both_clues_minimise_rmse` = \(v) all_true(
    v("a5|Update from both clues|rmse") < v("a5|Straw in Wind|rmse"),
    v("a5|Update from both clues|rmse") < v("a5|Smoking gun|rmse")
  ),
  `a6|descriptive|negative_correlation_better` = \(v) all_true(
    v("a6|-0.32|Update from both clues|rmse") < v("a6|0.32|Update from both clues|rmse")
  ),
  `a9|descriptive|ccubes_answers_more_often` = \(v) all_true(
    v("a9|Consistency Cubes|failure_rate") < v("a9|Classic Quine-McCluskey|failure_rate")
  ),
  `a12|descriptive|original_model_biased` = \(v) all_true(
    abs(v("a12|0.5|Original Model|bias")) > abs(v("a12|0|Original Model|bias"))
  ),
  `a13|descriptive|testing_effort_hurts` = \(v) all_true(
    v("a13|1|5|prop_sims") > v("a13|5|1|prop_sims")
  ),
  `a16|descriptive|matching_improves_mse` = \(v) all_true(
    v("a16|matching|rmse") < v("a16|dim|rmse"), abs(v("a16|matching|bias")) > 0
  ),
  `a18|descriptive|principled_less_biased` = \(v) all_true(
    abs(v("a18|Test sample|bias")) < abs(v("a18|Full sample|bias"))
  ),
  `a18|descriptive|principled_noisier` = \(v) all_true(
    v("a18|Test sample|RMSE") > v("a18|Full sample|RMSE")
  ),
  `a21|descriptive|interaction_dominated` = \(v) all_true(
    v("a21|DM_replication_CBO_ind|Interact CBO|rmse") >
      max(c(v("a21|DM_replication_CBO_ind|Ignore CBO|rmse"),
            v("a21|DM_replication_CBO_ind|Include CBO|rmse"),
            v("a21|DM_replication_CBO_ind|Include CBO if imbalanced|rmse"))),
    v("a21|DM_replication_CBO_cor|Interact CBO|rmse") >
      max(c(v("a21|DM_replication_CBO_cor|Ignore CBO|rmse"),
            v("a21|DM_replication_CBO_cor|Include CBO|rmse"),
            v("a21|DM_replication_CBO_cor|Include CBO if imbalanced|rmse")))
  )
)

evaluate_descriptive <- function(values) {
  look_up <- lookup_values(values)
  imap(
    descriptive_claims,
    \(fn, id) tibble(claim_id = id, value_chr = as.character(fn(look_up)))
  ) |>
    list_rbind()
}

archive_lookup_values <- archive_values |> rename(value = value_script)

archive_derived <- evaluate_derived(archive_lookup_values) |> rename(value_script = value)

archive_descriptive <- evaluate_descriptive(archive_lookup_values) |>
  rename(value_script_chr = value_chr)

# The one claim about the shape of a plotted line rather than about an ordering
# of table cells. The deposit's saved diagnosis carries all nine factorial
# designs, but its own scripts write no numbers for the figure, so this is
# checked against the rewrite only.
figure_1_shape <- tibble(
  claim_id = "figure_1|factorial_bias_increases",
  value_chr = as.character(
    main_effect_bias |>
      filter(estimator == "No_Interaction") |>
      arrange(interaction) |>
      pull(value) |>
      diff() |>
      (\(differences) all(differences > 0))()
  )
)

rewrite_values <- bind_rows(
  rewrite_values,
  evaluate_derived(rewrite_values),
  evaluate_descriptive(rewrite_values),
  figure_1_shape
) |>
  rename(value_rewrite = value, value_rewrite_chr = value_chr)

archive_values <- bind_rows(archive_values, archive_derived, archive_descriptive)

# Joining ----
# Both sides are keyed on the claim id, never on a published value. The helper
# asserts that the key is unique on both sides and that the join neither adds nor
# drops a row of the left side.
join_on_claim_id <- function(left, right) {
  stopifnot(!anyDuplicated(left$claim_id), !anyDuplicated(right$claim_id))
  joined <- left_join(left, right, by = "claim_id")
  stopifnot(nrow(joined) == nrow(left))
  joined
}

ground_truth <- published |>
  join_on_claim_id(rewrite_values) |>
  join_on_claim_id(archive_values)

# Gate: every claim that should have a computed counterpart has one ----
missing_counterpart <- ground_truth |>
  filter(
    claim_type %in% c("pipeline", "descriptive"),
    expect_rewrite == 1,
    is.na(value_rewrite), is.na(value_rewrite_chr), value_paper != "NaN"
  )

if (nrow(missing_counterpart) > 0) {
  stop(str_glue(
    "Pipeline or descriptive claims with no computed counterpart ",
    "({nrow(missing_counterpart)}): {str_c(missing_counterpart$claim_id, collapse = ', ')}"
  ))
}

undeclared_absence <- ground_truth |>
  filter(claim_type %in% c("pipeline", "descriptive"), expect_rewrite == 0, is.na(notes))

if (nrow(undeclared_absence) > 0) {
  stop(str_glue(
    "Claims exempted from needing a counterpart with no reason recorded: ",
    "{str_c(undeclared_absence$claim_id, collapse = ', ')}"
  ))
}

# Comparison ----
# A value agrees when the computed number, printed to the precision the article
# printed, gives the article's digits. The precision is read off the published
# string, because a double does not record how many decimals were printed.
published_decimals <- function(value_paper) {
  digits <- str_remove(value_paper, "%$")
  if_else(
    str_detect(digits, fixed(".")),
    nchar(str_extract(digits, "(?<=\\.)[0-9]+")),
    0L
  )
}

# A missing value means one of two different things and they must not share a
# display. A diagnosand taken over a set of estimates that are all missing is
# undefined: the appendix prints those cells as NaN, DeclareDesign 1.1.1 returns
# NA, and the two say the same thing about the same quantity. Everywhere else a
# missing value means the source carries no counterpart to the claim at all,
# which is not a verdict and must leave the comparison empty rather than record a
# disagreement. Printing both as the string "NA" made every definitional and
# structural claim a mismatch against its own article.
at_published_precision <- function(value, value_chr, value_paper) {
  decimals <- published_decimals(value_paper)
  suffix <- if_else(str_detect(value_paper, "%$"), "%", "")
  case_when(
    !is.na(value_chr) ~ value_chr,
    is.nan(value) ~ "NaN",
    is.na(value) & value_paper == "NaN" ~ "NaN",
    is.na(value) ~ NA_character_,
    .default = str_c(sprintf(str_c("%.", decimals, "f"), value), suffix)
  )
}

agrees <- function(computed, published) {
  !is.na(computed) & !is.na(published) & computed == published
}

ground_truth <- ground_truth |>
  mutate(
    value_rewrite_display = at_published_precision(value_rewrite, value_rewrite_chr, value_paper),
    value_script_display = at_published_precision(value_script, value_script_chr, value_paper),
    match = case_when(
      is.na(value_paper) | is.na(value_script_display) ~ NA_integer_,
      agrees(value_script_display, value_paper) ~ 1L,
      .default = 0L
    ),
    match_rewrite = case_when(
      is.na(value_paper) | is.na(value_rewrite_display) ~ NA_integer_,
      agrees(value_rewrite_display, value_paper) ~ 1L,
      .default = 0L
    )
  )

# Gate: every disagreement has a recorded defect locus ----
# A pipeline claim the rewrite is not expected to answer needs one too. That is
# the case where the deposit cannot support the claim at all, which is a finding
# and not a plumbing gap, and it would otherwise sit in the unverifiable bucket
# with no record of why.
disagreements <- ground_truth |>
  filter(match_rewrite == 0 | (claim_type == "pipeline" & expect_rewrite == 0))

unrecorded <- setdiff(disagreements$claim_id, defect_loci$claim_id)

if (length(unrecorded) > 0) {
  stop(str_glue(
    "Rows with a disagreement or an unanswerable pipeline claim and no defect ",
    "locus ({length(unrecorded)}): {str_c(head(unrecorded, 30), collapse = ', ')}"
  ))
}

unused_loci <- setdiff(defect_loci$claim_id, disagreements$claim_id)

if (length(unused_loci) > 0) {
  stop(str_glue(
    "Defect loci recorded for rows that now agree ({length(unused_loci)}): ",
    "{str_c(head(unused_loci, 30), collapse = ', ')}"
  ))
}

# The note's verdict clause is computed from the same comparison that sets the
# verdict, so a note cannot describe a value the verdict contradicts.
ground_truth <- ground_truth |>
  left_join(defect_loci, by = "claim_id") |>
  mutate(
    verdict_clause = case_when(
      is.na(match_rewrite) ~ "Not verifiable against the rewrite.",
      match_rewrite == 1 ~ str_glue("Rewrite gives {value_rewrite_display}, as published."),
      .default = str_glue("Rewrite gives {value_rewrite_display} against a published {value_paper}.")
    ),
    notes = str_squish(str_c(coalesce(notes, ""), " ", verdict_clause, " ", coalesce(defect_note, "")))
  ) |>
  select(
    paper_id, table_figure = published_float, location, claim_id, claim_type, claim,
    value_script = value_script_display, value_paper, match,
    value_rewrite = value_rewrite_display, match_rewrite, defect_locus, notes
  )

# Gate: every published float is covered ----
# The inventory is the article's own float list plus the appendix's own section
# structure, because the appendix numbers none of its tables and a coverage check
# against what the pipeline happens to produce would be circular.
published_float_inventory <- c(
  "Table 1", "Table 2", "Table 3", "Table 4", "Figure 1", "Figure 2",
  "Appendix S1", "Appendix S2.1", "Appendix S3.1.1", "Appendix S3.1.2",
  "Appendix S3.2.1a", "Appendix S3.2.1b", "Appendix S3.2.2a", "Appendix S3.2.2b",
  "Appendix S3.2.2c", "Appendix S3.2.2d", "Appendix S3.2.2e", "Appendix S3.2.3a",
  "Appendix S3.2.3b", "Appendix S3.2.3c", "Appendix S3.2.4", "Appendix S3.2.5",
  "Appendix S3.2.6", "Appendix S3.3", "Appendix S4.5", "Appendix S4.6", "Appendix S4.7"
)

uncovered <- setdiff(published_float_inventory, ground_truth$table_figure)

if (length(uncovered) > 0) {
  stop(str_glue("Published floats with no ground truth row: {str_c(uncovered, collapse = ', ')}"))
}

unlisted <- setdiff(ground_truth$table_figure, published_float_inventory)

if (length(unlisted) > 0) {
  stop(str_glue("Ground truth rows for floats not in the inventory: {str_c(unlisted, collapse = ', ')}"))
}

# Gate: the second instrument agrees ----
# maintained/in_text_claims.R reaches the same claimed numbers from the same
# outputs by its own path, doing its own selection and its own rounding. Running
# it here and comparing line by line is what makes the two independent rather
# than merely duplicated: a disagreement means one of the two is wrong.
in_text_lines <- capture.output(source(here::here("maintained", "in_text_claims.R")))

in_text_claims <- tibble(line = in_text_lines) |>
  filter(str_starts(line, "CLAIM ")) |>
  transmute(
    claim_id = str_match(line, "^CLAIM (.*) = ")[, 2],
    value_in_text = str_match(line, " = (.*)$")[, 2]
  )

stopifnot(!anyDuplicated(in_text_claims$claim_id))

must_have_block <- ground_truth |> filter(claim_type %in% c("pipeline", "descriptive"))

blockless <- setdiff(must_have_block$claim_id, in_text_claims$claim_id)

if (length(blockless) > 0) {
  stop(str_glue(
    "Pipeline or descriptive claims with no block in in_text_claims.R ",
    "({length(blockless)}): {str_c(head(blockless, 30), collapse = ', ')}"
  ))
}

# in_text_claims.R never reads the article, so where a diagnosand is undefined it
# prints the NA the pipeline gave it while this file prints the NaN the article
# printed. The two describe the same cell, and the equivalence is written here
# rather than pushed into the claims file, which must not know the published
# value.
same_value <- function(a, b) {
  a == b | (a == "NaN" & b == "NA") | (a == "NA" & b == "NaN")
}

instrument_disagreements <- ground_truth |>
  inner_join(in_text_claims, by = "claim_id") |>
  filter(!is.na(value_rewrite), !same_value(value_rewrite, value_in_text))

if (nrow(instrument_disagreements) > 0) {
  stop(str_glue(
    "The ground truth and in_text_claims.R disagree on ",
    "{nrow(instrument_disagreements)} claims: ",
    "{str_c(instrument_disagreements$claim_id, ' (', instrument_disagreements$value_rewrite, ' vs ', instrument_disagreements$value_in_text, ')', collapse = '; ')}"
  ))
}

write_csv(
  ground_truth,
  here::here("ground_truth", "blair_cooper_coppock_humphreys_2019_ground_truth.csv")
)

print(ground_truth |> count(claim_type, match_rewrite))

print(str_glue(
  "Ground truth: {nrow(ground_truth)} claims over ",
  "{n_distinct(ground_truth$table_figure)} published floats. ",
  "Archive matches published: {sum(ground_truth$match == 1, na.rm = TRUE)} of ",
  "{sum(!is.na(ground_truth$match))} comparable. ",
  "Rewrite matches published: {sum(ground_truth$match_rewrite == 1, na.rm = TRUE)} of ",
  "{sum(!is.na(ground_truth$match_rewrite))} comparable."
))
