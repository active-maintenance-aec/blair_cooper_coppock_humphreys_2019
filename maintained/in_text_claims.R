# blair_cooper_coppock_humphreys_2019/maintained/in_text_claims.R
# Output: none (prints an audit trail)
# Depends on: helpers.R, maintained/output/
# Description: One entry per published claim, in the order the article and its
#   appendix state them. Each entry carries the sentence the article prints, then
#   reads the pipeline output that backs it and prints the number in the
#   article's own units and rounding. Nothing is re-estimated and the ground
#   truth is not consulted: this file and ground_truth/build_ground_truth.R reach
#   the same claimed numbers by separate paths through the same outputs, and
#   build_ground_truth.R compares the two. A disagreement means one of them is
#   wrong.

source(here::here("maintained", "helpers.R"))

options(width = 200)

read_output <- function(file) read_csv(out_path(file), show_col_types = FALSE)

# One cell of one output table, selected by the identifiers that name its row.
# The guard is the point: a selection that picks no row, or more than one, stops
# here rather than printing a number that belongs to something else.
#
# The two formals are dot-prefixed because the identifiers arrive by name in the
# dots. A table with a column called quantity, which table_2_tool_census_counts
# has, would otherwise bind that column's name to the formal and leave the
# selection empty, so every row would match and the guard would fire.
cell <- function(.dat, .quantity, ...) {
  conditions <- list(...)
  rows <- rep(TRUE, nrow(.dat))
  for (column in names(conditions)) rows <- rows & .dat[[column]] == conditions[[column]]
  stopifnot(sum(rows, na.rm = TRUE) == 1)
  .dat[[.quantity]][which(rows)]
}

# Every printed line carries the claim identifier as its label, so the audit trail
# can be read straight down and build_ground_truth.R can check it line by line.
claim <- function(id, value, decimals = 2L, percent = FALSE) {
  printed <- if (is.character(value) || is.logical(value)) {
    as.character(value)
  } else if (is.nan(value)) {
    "NaN"
  } else if (is.na(value)) {
    "NA"
  } else {
    str_c(sprintf(str_c("%.", decimals, "f"), value), if (percent) "%" else "")
  }
  cat("CLAIM ", id, " = ", printed, "\n", sep = "")
}

# Pipeline outputs ----
census <- read_output("table_2_tool_census.csv")
census_counts <- read_output("table_2_tool_census_counts.csv")
figure_1 <- read_output("figure_1_experimental_designs.csv")
figure_2_sims <- read_output("figure_2_bs_sampling_distributions.csv")
a1 <- read_output("table_a1_simple_design.csv")
a2 <- read_output("table_a2_working_example_census.csv")
a3 <- read_output("table_a3_survey_design.csv")
a4 <- read_output("table_a4_bayesian_descriptive.csv")
a5 <- read_output("table_a5_process_tracing.csv")
a6 <- read_output("table_a6_process_tracing_correlated_clues.csv")
a7 <- read_output("table_a7_qca_simulated_data.csv")
a8 <- read_output("table_a8_qca_estimates.csv")
a9 <- read_output("table_a9_qca_diagnosis.csv")
a10 <- read_output("table_a10_qca_error_rate_and_n.csv")
a11 <- read_output("table_a11_qca_with_least_squares.csv")
a12 <- read_output("table_a12_nested_confounding.csv")
a13 <- read_output("table_a13_nested_effort.csv")
a14 <- read_output("table_a14_nested_thresholds.csv")
a15 <- read_output("table_a15_model_based_estimands.csv")
a16 <- read_output("table_a16_matching.csv")
a17 <- read_output("table_a17_regression_discontinuity.csv")
a18 <- read_output("table_a18_discovery.csv")
a19 <- read_output("table_a19_bs_diagnosis.csv")
a20 <- read_output("table_a20_bs_exaggeration.csv")
a21 <- read_output("table_a21_bs_covariate_adjustment.csv")


# Elements of a research design ----
# Table 1 lists examples of diagnosands against the elements of a design. No quantity is printed, so there is nothing to check.

# Existing tools cannot declare many core elements of designs ----

# Table 2 reports 0 of the 30 admitted tools for 'Effect and block size correlated'
claim("table_2|M: effect sizes correlated with block sizes|n_tools_able", cell(census, "n_tools_able", column = "M: effect sizes correlated with block sizes"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Estimand'
claim("table_2|I: Estimand|n_tools_able", cell(census, "n_tools_able", column = "I: Estimand"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Sampling procedure'
claim("table_2|R: Sampling strategy|n_tools_able", cell(census, "n_tools_able", column = "R: Sampling strategy"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Assignment procedure'
claim("table_2|R: Exact randomization procedure|n_tools_able", cell(census, "n_tools_able", column = "R: Exact randomization procedure"), 0L)

# Table 2 reports 1 of the 30 admitted tools for 'Block sizes vary'
claim("table_2|M: Heterogeneous block sizes|n_tools_able", cell(census, "n_tools_able", column = "M: Heterogeneous block sizes"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Probability weighting'
claim("table_2|A: Inverse-probability weights|n_tools_able", cell(census, "n_tools_able", column = "A: Inverse-probability weights"), 0L)

# Table 2 reports 28 of the 30 admitted tools for 'Power (DIM estimator)'
claim("table_2|D: Power of DIM|n_tools_able", cell(census, "n_tools_able", column = "D: Power of DIM"), 0L)

# Table 2 reports 13 of the 30 admitted tools for 'Power (BFE estimator)'
claim("table_2|D: Power of BFE|n_tools_able", cell(census, "n_tools_able", column = "D: Power of BFE"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Power (IPW-BFE estimator)'
claim("table_2|D: Power of IPW-BFE|n_tools_able", cell(census, "n_tools_able", column = "D: Power of IPW-BFE"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Bias (any estimator)'
claim("table_2|D: Bias|n_tools_able", cell(census, "n_tools_able", column = "D: Bias"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'Coverage (any estimator)'
claim("table_2|D: Coverage|n_tools_able", cell(census, "n_tools_able", column = "D: Coverage"), 0L)

# Table 2 reports 0 of the 30 admitted tools for 'SD of estimates (any estimator)'
claim("table_2|SD of estimates (any estimator)|n_tools_able", cell(census, "n_tools_able", row_label = "SD of estimates (any estimator)"), 0L)

# Thirty of the 143 tools we identified were admissible for the survey.
claim("text|n_candidates", cell(census_counts, "n", quantity = "n_candidates"), 0L)

# Thirty of the 143 tools we identified were admissible for the survey.
claim("text|n_admitted", cell(census_counts, "n", quantity = "n_admitted"), 0L)

# A procedure for declaring and diagnosing research designs ----
# Table 3's bottom panel prints the code that diagnoses the declared design for bias over m draws. No quantity is printed, so there is nothing to check.

# Declaring and diagnosing research designs in practice ----

# We declare and diagnose this design and confirm that neither design exhibits bias when the
# true interaction term is equal to zero.
claim("figure_1|zero_interaction_bias",
      max(abs(figure_1$value[figure_1$diagnosand == "Bias" & figure_1$interaction == 0 &
                         figure_1$estimator %in% c("DIM (Z_2 - Z_1)", "No_Interaction")])), 2L)

# However, when the interaction between the two treatments is stronger, the factorial design
# renders estimates of the effect of treatment 1 that are more and more biased relative to the
# 'pure' main effect estimand.
claim("figure_1|factorial_bias_increases",
      all(diff(figure_1$value[figure_1$diagnosand == "Bias" &
                          figure_1$estimator == "No_Interaction"][
  order(figure_1$interaction[figure_1$diagnosand == "Bias" &
                               figure_1$estimator == "No_Interaction"])]) > 0))

# Improving designs and assessing alternatives ----
# Table 4 numbers four scenarios 1 to 4 by whether the model or the answer strategy differs from the author's. No quantity is printed, so there is nothing to check.

# Application: design replication of Bjorkman and Svensson (2009) ----

# The dashed line represents the average answer, i.e., the answer we expect the design to
# provide.
claim("figure_2|Mortality estimate|mean_estimate", mean(figure_2_sims$estimate[figure_2_sims$estimator == "Mortality estimate"], na.rm = TRUE), 2L)

# The dashed line represents the average answer, i.e., the answer we expect the design to
# provide.
claim("figure_2|Weight estimate|mean_estimate", mean(figure_2_sims$estimate[figure_2_sims$estimator == "Weight estimate"], na.rm = TRUE), 2L)

# Appendix 1: declaration of a simple design ----

# Appendix S1, single row: rmse
claim("a1|rmse", cell(a1, "rmse"), 2L)

# Appendix S1, single row: bias
claim("a1|bias", cell(a1, "bias"), 2L)

# Appendix S1, single row: mean_estimand
claim("a1|mean_estimand", cell(a1, "mean_estimand"), 2L)

# Appendix 2.1: the working example behind the tool survey ----

# When rounded to the third decimal place, the PATE is ~ .406 with a standard deviation of 1.01,
# producing a Cohen's d of approximately .4.
# The three estimators share one inquiry, so the mean estimand is one number and the guard says so.
claim("a2|prose|pate",
      {
        stopifnot(n_distinct(a2$mean_estimand) == 1)
        cell(a2, "mean_estimand", estimator = "BFE")
      }, 3L)

# The table above illustrates that the DIM and BFE estimators are negatively biased: they tend
# to underestimate the actual size of the treatment effect.
claim("a2|descriptive|dim_bfe_negative_bias",
      cell(a2, "bias", estimator = "DIM") < 0 && cell(a2, "bias", estimator = "BFE") < 0)

# While its power is much higher, the coverage is below the nominal level of 0.95, indicating
# that the standard error estimator is anticonservative.
claim("a2|descriptive|ipw_undercoverage",
      cell(a2, "power", estimator = "IPW-BFE") > cell(a2, "power", estimator = "BFE") &&
  cell(a2, "coverage", estimator = "IPW-BFE") < 0.95)

# Appendix S2.1, BFE: power
claim("a2|BFE|power", cell(a2, "power", estimator = "BFE"), 2L)

# Appendix S2.1, BFE: bias
claim("a2|BFE|bias", cell(a2, "bias", estimator = "BFE"), 2L)

# Appendix S2.1, BFE: rmse
claim("a2|BFE|rmse", cell(a2, "rmse", estimator = "BFE"), 2L)

# Appendix S2.1, BFE: coverage
claim("a2|BFE|coverage", cell(a2, "coverage", estimator = "BFE"), 2L)

# Appendix S2.1, DIM: power
claim("a2|DIM|power", cell(a2, "power", estimator = "DIM"), 2L)

# Appendix S2.1, DIM: bias
claim("a2|DIM|bias", cell(a2, "bias", estimator = "DIM"), 2L)

# Appendix S2.1, DIM: rmse
claim("a2|DIM|rmse", cell(a2, "rmse", estimator = "DIM"), 2L)

# Appendix S2.1, DIM: coverage
claim("a2|DIM|coverage", cell(a2, "coverage", estimator = "DIM"), 2L)

# Appendix S2.1, IPW-BFE: power
claim("a2|IPW-BFE|power", cell(a2, "power", estimator = "IPW-BFE"), 2L)

# Appendix S2.1, IPW-BFE: bias
claim("a2|IPW-BFE|bias", cell(a2, "bias", estimator = "IPW-BFE"), 2L)

# Appendix S2.1, IPW-BFE: rmse
claim("a2|IPW-BFE|rmse", cell(a2, "rmse", estimator = "IPW-BFE"), 2L)

# Appendix S2.1, IPW-BFE: coverage
claim("a2|IPW-BFE|coverage", cell(a2, "coverage", estimator = "IPW-BFE"), 2L)

# Appendix 3.1.1: survey designs ----

# Appendix S3.1.1, single row: bias
claim("a3|bias", cell(a3, "bias"), 2L)

# Appendix 3.1.2: Bayesian descriptive inference ----

# first, we stipulate uniform priors, with a mean of 0.50 and a standard deviation of 0.29
claim("a4|prose|flat_prior_sd", cell(a4, "prior_sd", sample_n = 10, estimator = "flat priors"), 2L)

# first, we stipulate uniform priors, with a mean of 0.50 and a standard deviation of 0.29
claim("a4|prose|flat_prior_mean", cell(a4, "prior_mean", sample_n = 10, estimator = "flat priors"), 2L)

# in the second, we place more prior probability mass at 0.50, with a standard deviation of 0.11
claim("a4|prose|info_prior_sd", cell(a4, "prior_sd", sample_n = 10, estimator = "informative priors"), 2L)

# in the second, we place more prior probability mass at 0.50, with a standard deviation of 0.11
claim("a4|prose|info_prior_mean", cell(a4, "prior_mean", sample_n = 10, estimator = "informative priors"), 2L)

# the informative priors decrease the posterior standard deviation by 40% relative to the
# uniform priors, but increase the bias by 33%
claim("a4|prose|sd_reduction",
      100 * (cell(a4, "mean_sd", sample_n = 10, estimator = "flat priors") -
         cell(a4, "mean_sd", sample_n = 10, estimator = "informative priors")) /
  cell(a4, "mean_sd", sample_n = 10, estimator = "flat priors"), 0L, percent = TRUE)

# the informative priors decrease the posterior standard deviation by 40% relative to the
# uniform priors, but increase the bias by 33%
claim("a4|prose|bias_increase",
      100 * (abs(cell(a4, "bias", sample_n = 10, estimator = "informative priors")) -
         abs(cell(a4, "bias", sample_n = 10, estimator = "flat priors"))) /
  abs(cell(a4, "bias", sample_n = 10, estimator = "flat priors")), 0L, percent = TRUE)

# The diagnosis shows that the informative prior approach yields more certain and more biased
# inferences than the uniform prior approach.
claim("a4|descriptive|informative_more_certain",
      cell(a4, "mean_sd", sample_n = 10, estimator = "informative priors") <
    cell(a4, "mean_sd", sample_n = 10, estimator = "flat priors") &&
  abs(cell(a4, "bias", sample_n = 10, estimator = "informative priors")) >
    abs(cell(a4, "bias", sample_n = 10, estimator = "flat priors")))

# Appendix S3.1.2, 10|flat priors: mean_est
claim("a4|10|flat priors|mean_est", cell(a4, "mean_est", sample_n = 10, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 10|flat priors: mean_sd
claim("a4|10|flat priors|mean_sd", cell(a4, "mean_sd", sample_n = 10, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 10|flat priors: bias
claim("a4|10|flat priors|bias", cell(a4, "bias", sample_n = 10, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 10|flat priors: mean_shift
claim("a4|10|flat priors|mean_shift", cell(a4, "mean_shift", sample_n = 10, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 10|flat priors: sd_shift
claim("a4|10|flat priors|sd_shift", cell(a4, "sd_shift", sample_n = 10, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 10|informative priors: mean_est
claim("a4|10|informative priors|mean_est", cell(a4, "mean_est", sample_n = 10, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 10|informative priors: mean_sd
claim("a4|10|informative priors|mean_sd", cell(a4, "mean_sd", sample_n = 10, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 10|informative priors: bias
claim("a4|10|informative priors|bias", cell(a4, "bias", sample_n = 10, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 10|informative priors: mean_shift
claim("a4|10|informative priors|mean_shift", cell(a4, "mean_shift", sample_n = 10, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 10|informative priors: sd_shift
claim("a4|10|informative priors|sd_shift", cell(a4, "sd_shift", sample_n = 10, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 100|flat priors: mean_est
claim("a4|100|flat priors|mean_est", cell(a4, "mean_est", sample_n = 100, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 100|flat priors: mean_sd
claim("a4|100|flat priors|mean_sd", cell(a4, "mean_sd", sample_n = 100, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 100|flat priors: bias
claim("a4|100|flat priors|bias", cell(a4, "bias", sample_n = 100, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 100|flat priors: mean_shift
claim("a4|100|flat priors|mean_shift", cell(a4, "mean_shift", sample_n = 100, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 100|flat priors: sd_shift
claim("a4|100|flat priors|sd_shift", cell(a4, "sd_shift", sample_n = 100, estimator = "flat priors"), 2L)

# Appendix S3.1.2, 100|informative priors: mean_est
claim("a4|100|informative priors|mean_est", cell(a4, "mean_est", sample_n = 100, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 100|informative priors: mean_sd
claim("a4|100|informative priors|mean_sd", cell(a4, "mean_sd", sample_n = 100, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 100|informative priors: bias
claim("a4|100|informative priors|bias", cell(a4, "bias", sample_n = 100, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 100|informative priors: mean_shift
claim("a4|100|informative priors|mean_shift", cell(a4, "mean_shift", sample_n = 100, estimator = "informative priors"), 2L)

# Appendix S3.1.2, 100|informative priors: sd_shift
claim("a4|100|informative priors|sd_shift", cell(a4, "sd_shift", sample_n = 100, estimator = "informative priors"), 2L)

# Appendix 3.2.1: process tracing with independent clues ----

# the standard deviation in posterior inferences generated by the smoking gun strategy is much
# lower than that provided by the straw-in-the-wind
claim("a5|descriptive|smoking_gun_sd",
      cell(a5, "sd_posterior", estimator = "Smoking gun") <
  cell(a5, "sd_posterior", estimator = "Straw in Wind"))

# on average the RMSE is lower when one only searches for straws-in-the-wind, because they are
# more commonly observed
claim("a5|descriptive|siw_lower_rmse",
      cell(a5, "rmse", estimator = "Straw in Wind") < cell(a5, "rmse", estimator = "Smoking gun"))

# Clearly, however, RMSE is minimized by conditioning on both CPOs.
claim("a5|descriptive|both_clues_minimise_rmse",
      cell(a5, "rmse", estimator = "Update from both clues") ==
  min(a5$rmse))

# Appendix S3.2.1a, Smoking gun: bias
claim("a5|Smoking gun|bias", cell(a5, "bias", estimator = "Smoking gun"), 2L)

# Appendix S3.2.1a, Smoking gun: rmse
claim("a5|Smoking gun|rmse", cell(a5, "rmse", estimator = "Smoking gun"), 2L)

# Appendix S3.2.1a, Smoking gun: mean_estimand
claim("a5|Smoking gun|mean_estimand", cell(a5, "mean_estimand", estimator = "Smoking gun"), 2L)

# Appendix S3.2.1a, Smoking gun: mean_posterior
claim("a5|Smoking gun|mean_posterior", cell(a5, "mean_posterior", estimator = "Smoking gun"), 2L)

# Appendix S3.2.1a, Smoking gun: sd_posterior
claim("a5|Smoking gun|sd_posterior", cell(a5, "sd_posterior", estimator = "Smoking gun"), 2L)

# Appendix S3.2.1a, Straw in Wind: bias
claim("a5|Straw in Wind|bias", cell(a5, "bias", estimator = "Straw in Wind"), 2L)

# Appendix S3.2.1a, Straw in Wind: rmse
claim("a5|Straw in Wind|rmse", cell(a5, "rmse", estimator = "Straw in Wind"), 2L)

# Appendix S3.2.1a, Straw in Wind: mean_estimand
claim("a5|Straw in Wind|mean_estimand", cell(a5, "mean_estimand", estimator = "Straw in Wind"), 2L)

# Appendix S3.2.1a, Straw in Wind: mean_posterior
claim("a5|Straw in Wind|mean_posterior", cell(a5, "mean_posterior", estimator = "Straw in Wind"), 2L)

# Appendix S3.2.1a, Straw in Wind: sd_posterior
claim("a5|Straw in Wind|sd_posterior", cell(a5, "sd_posterior", estimator = "Straw in Wind"), 2L)

# Appendix S3.2.1a, Update from both clues: bias
claim("a5|Update from both clues|bias", cell(a5, "bias", estimator = "Update from both clues"), 2L)

# Appendix S3.2.1a, Update from both clues: rmse
claim("a5|Update from both clues|rmse", cell(a5, "rmse", estimator = "Update from both clues"), 2L)

# Appendix S3.2.1a, Update from both clues: mean_estimand
claim("a5|Update from both clues|mean_estimand", cell(a5, "mean_estimand", estimator = "Update from both clues"), 2L)

# Appendix S3.2.1a, Update from both clues: mean_posterior
claim("a5|Update from both clues|mean_posterior", cell(a5, "mean_posterior", estimator = "Update from both clues"), 2L)

# Appendix S3.2.1a, Update from both clues: sd_posterior
claim("a5|Update from both clues|sd_posterior", cell(a5, "sd_posterior", estimator = "Update from both clues"), 2L)

# Appendix 3.2.1: process tracing with correlated clues ----

# We see that if two CPOs are sought expected errors are lower when these are negatively
# correlated with each other.
claim("a6|descriptive|negative_correlation_better",
      cell(a6, "rmse", rho_H = -0.32, estimator = "Update from both clues") <
  cell(a6, "rmse", rho_H = 0.32, estimator = "Update from both clues"))

# Appendix S3.2.1b, -0.32|Update from both clues: rmse
claim("a6|-0.32|Update from both clues|rmse", cell(a6, "rmse", rho_H = -0.32, estimator = "Update from both clues"), 2L)

# Appendix S3.2.1b, 0.32|Update from both clues: rmse
claim("a6|0.32|Update from both clues|rmse", cell(a6, "rmse", rho_H = 0.32, estimator = "Update from both clues"), 2L)

# Appendix 3.2.2: one draw of the QCA data ----

# One draw of the QCA data, case 1: A, B, the four potential outcomes, Y, mistake, Y_obs
claim("a7|1|row",
      paste(cell(a7, "A", ID = 1), cell(a7, "B", ID = 1), cell(a7, "Y_A_0_B_0", ID = 1),
        cell(a7, "Y_A_1_B_0", ID = 1), cell(a7, "Y_A_0_B_1", ID = 1), cell(a7, "Y_A_1_B_1", ID = 1),
        cell(a7, "Y", ID = 1), cell(a7, "mistake", ID = 1), cell(a7, "Y_obs", ID = 1), sep = "|"))

# One draw of the QCA data, case 2: A, B, the four potential outcomes, Y, mistake, Y_obs
claim("a7|2|row",
      paste(cell(a7, "A", ID = 2), cell(a7, "B", ID = 2), cell(a7, "Y_A_0_B_0", ID = 2),
        cell(a7, "Y_A_1_B_0", ID = 2), cell(a7, "Y_A_0_B_1", ID = 2), cell(a7, "Y_A_1_B_1", ID = 2),
        cell(a7, "Y", ID = 2), cell(a7, "mistake", ID = 2), cell(a7, "Y_obs", ID = 2), sep = "|"))

# One draw of the QCA data, case 3: A, B, the four potential outcomes, Y, mistake, Y_obs
claim("a7|3|row",
      paste(cell(a7, "A", ID = 3), cell(a7, "B", ID = 3), cell(a7, "Y_A_0_B_0", ID = 3),
        cell(a7, "Y_A_1_B_0", ID = 3), cell(a7, "Y_A_0_B_1", ID = 3), cell(a7, "Y_A_1_B_1", ID = 3),
        cell(a7, "Y", ID = 3), cell(a7, "mistake", ID = 3), cell(a7, "Y_obs", ID = 3), sep = "|"))

# One draw of the QCA data, case 4: A, B, the four potential outcomes, Y, mistake, Y_obs
claim("a7|4|row",
      paste(cell(a7, "A", ID = 4), cell(a7, "B", ID = 4), cell(a7, "Y_A_0_B_0", ID = 4),
        cell(a7, "Y_A_1_B_0", ID = 4), cell(a7, "Y_A_0_B_1", ID = 4), cell(a7, "Y_A_1_B_1", ID = 4),
        cell(a7, "Y", ID = 4), cell(a7, "mistake", ID = 4), cell(a7, "Y_obs", ID = 4), sep = "|"))

# One draw of the QCA data, case 5: A, B, the four potential outcomes, Y, mistake, Y_obs
claim("a7|5|row",
      paste(cell(a7, "A", ID = 5), cell(a7, "B", ID = 5), cell(a7, "Y_A_0_B_0", ID = 5),
        cell(a7, "Y_A_1_B_0", ID = 5), cell(a7, "Y_A_0_B_1", ID = 5), cell(a7, "Y_A_1_B_1", ID = 5),
        cell(a7, "Y", ID = 5), cell(a7, "mistake", ID = 5), cell(a7, "Y_obs", ID = 5), sep = "|"))

# One draw of the QCA data, case 6: A, B, the four potential outcomes, Y, mistake, Y_obs
claim("a7|6|row",
      paste(cell(a7, "A", ID = 6), cell(a7, "B", ID = 6), cell(a7, "Y_A_0_B_0", ID = 6),
        cell(a7, "Y_A_1_B_0", ID = 6), cell(a7, "Y_A_0_B_1", ID = 6), cell(a7, "Y_A_1_B_1", ID = 6),
        cell(a7, "Y", ID = 6), cell(a7, "mistake", ID = 6), cell(a7, "Y_obs", ID = 6), sep = "|"))

# Appendix 3.2.2: one draw of the QCA estimates ----

# One draw of the QCA estimates, Classic Quine-McCluskey
claim("a8|Classic Quine-McCluskey|estimate",
      cell(a8, "estimate", estimator = "Classic Quine-McCluskey"))

# One draw of the QCA estimates, Consistency Cubes
claim("a8|Consistency Cubes|estimate",
      cell(a8, "estimate", estimator = "Consistency Cubes"))

# Appendix 3.2.2: QCA diagnosis ----

# The consistency cubes algorithm does return answers more often than the classic algorithm.
claim("a9|descriptive|ccubes_answers_more_often",
      cell(a9, "failure_rate", estimator = "Consistency Cubes") <
  cell(a9, "failure_rate", estimator = "Classic Quine-McCluskey"))

# Appendix S3.2.2c, Classic Quine-McCluskey: correct_rate
claim("a9|Classic Quine-McCluskey|correct_rate", cell(a9, "correct_rate", estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2c, Classic Quine-McCluskey: failure_rate
claim("a9|Classic Quine-McCluskey|failure_rate", cell(a9, "failure_rate", estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2c, Consistency Cubes: correct_rate
claim("a9|Consistency Cubes|correct_rate", cell(a9, "correct_rate", estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2c, Consistency Cubes: failure_rate
claim("a9|Consistency Cubes|failure_rate", cell(a9, "failure_rate", estimator = "Consistency Cubes"), 2L)

# Around 39% of the time, both classic QMC and CCubes return exactly the right causal
# configuration.
claim("text|qca_correct_rate_prose", cell(a9, "correct_rate", estimator = "Classic Quine-McCluskey"), 2L)

# Appendix 3.2.2: QCA by measurement error and sample size ----

# When there is no error and the sample is large enough to ensure sufficient variation in the
# data, QMC and CCubes successfully recover the correct configuration 100% of the time.
claim("a10|prose|perfect_recovery", min(a10$correct_rate[a10$error_rate == 0 & a10$N == 50]), 2L)

# Appendix S3.2.2d, 0|5|Classic Quine-McCluskey: correct_rate
claim("a10|0|5|Classic Quine-McCluskey|correct_rate", cell(a10, "correct_rate", error_rate = 0, N = 5, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0|5|Classic Quine-McCluskey: failure_rate
claim("a10|0|5|Classic Quine-McCluskey|failure_rate", cell(a10, "failure_rate", error_rate = 0, N = 5, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0|5|Consistency Cubes: correct_rate
claim("a10|0|5|Consistency Cubes|correct_rate", cell(a10, "correct_rate", error_rate = 0, N = 5, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0|5|Consistency Cubes: failure_rate
claim("a10|0|5|Consistency Cubes|failure_rate", cell(a10, "failure_rate", error_rate = 0, N = 5, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0.05|5|Classic Quine-McCluskey: correct_rate
claim("a10|0.05|5|Classic Quine-McCluskey|correct_rate", cell(a10, "correct_rate", error_rate = 0.05, N = 5, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0.05|5|Classic Quine-McCluskey: failure_rate
claim("a10|0.05|5|Classic Quine-McCluskey|failure_rate", cell(a10, "failure_rate", error_rate = 0.05, N = 5, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0.05|5|Consistency Cubes: correct_rate
claim("a10|0.05|5|Consistency Cubes|correct_rate", cell(a10, "correct_rate", error_rate = 0.05, N = 5, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0.05|5|Consistency Cubes: failure_rate
claim("a10|0.05|5|Consistency Cubes|failure_rate", cell(a10, "failure_rate", error_rate = 0.05, N = 5, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0|50|Classic Quine-McCluskey: correct_rate
claim("a10|0|50|Classic Quine-McCluskey|correct_rate", cell(a10, "correct_rate", error_rate = 0, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0|50|Classic Quine-McCluskey: failure_rate
claim("a10|0|50|Classic Quine-McCluskey|failure_rate", cell(a10, "failure_rate", error_rate = 0, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0|50|Consistency Cubes: correct_rate
claim("a10|0|50|Consistency Cubes|correct_rate", cell(a10, "correct_rate", error_rate = 0, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0|50|Consistency Cubes: failure_rate
claim("a10|0|50|Consistency Cubes|failure_rate", cell(a10, "failure_rate", error_rate = 0, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0.05|50|Classic Quine-McCluskey: correct_rate
claim("a10|0.05|50|Classic Quine-McCluskey|correct_rate", cell(a10, "correct_rate", error_rate = 0.05, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0.05|50|Classic Quine-McCluskey: failure_rate
claim("a10|0.05|50|Classic Quine-McCluskey|failure_rate", cell(a10, "failure_rate", error_rate = 0.05, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2d, 0.05|50|Consistency Cubes: correct_rate
claim("a10|0.05|50|Consistency Cubes|correct_rate", cell(a10, "correct_rate", error_rate = 0.05, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2d, 0.05|50|Consistency Cubes: failure_rate
claim("a10|0.05|50|Consistency Cubes|failure_rate", cell(a10, "failure_rate", error_rate = 0.05, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix 3.2.2: QCA with a least squares answer strategy ----

# Appendix S3.2.2e, 0|3|Classic Quine-McCluskey: correct_rate
claim("a11|0|3|Classic Quine-McCluskey|correct_rate", cell(a11, "correct_rate", error_rate = 0, N = 3, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0|3|Classic Quine-McCluskey: failure_rate
claim("a11|0|3|Classic Quine-McCluskey|failure_rate", cell(a11, "failure_rate", error_rate = 0, N = 3, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0|3|Consistency Cubes: correct_rate
claim("a11|0|3|Consistency Cubes|correct_rate", cell(a11, "correct_rate", error_rate = 0, N = 3, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0|3|Consistency Cubes: failure_rate
claim("a11|0|3|Consistency Cubes|failure_rate", cell(a11, "failure_rate", error_rate = 0, N = 3, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0|3|Least Squares: correct_rate
claim("a11|0|3|Least Squares|correct_rate", cell(a11, "correct_rate", error_rate = 0, N = 3, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0|3|Least Squares: failure_rate
claim("a11|0|3|Least Squares|failure_rate", cell(a11, "failure_rate", error_rate = 0, N = 3, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0.2|3|Classic Quine-McCluskey: correct_rate
claim("a11|0.2|3|Classic Quine-McCluskey|correct_rate", cell(a11, "correct_rate", error_rate = 0.2, N = 3, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0.2|3|Classic Quine-McCluskey: failure_rate
claim("a11|0.2|3|Classic Quine-McCluskey|failure_rate", cell(a11, "failure_rate", error_rate = 0.2, N = 3, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0.2|3|Consistency Cubes: correct_rate
claim("a11|0.2|3|Consistency Cubes|correct_rate", cell(a11, "correct_rate", error_rate = 0.2, N = 3, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0.2|3|Consistency Cubes: failure_rate
claim("a11|0.2|3|Consistency Cubes|failure_rate", cell(a11, "failure_rate", error_rate = 0.2, N = 3, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0.2|3|Least Squares: correct_rate
claim("a11|0.2|3|Least Squares|correct_rate", cell(a11, "correct_rate", error_rate = 0.2, N = 3, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0.2|3|Least Squares: failure_rate
claim("a11|0.2|3|Least Squares|failure_rate", cell(a11, "failure_rate", error_rate = 0.2, N = 3, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0|50|Classic Quine-McCluskey: correct_rate
claim("a11|0|50|Classic Quine-McCluskey|correct_rate", cell(a11, "correct_rate", error_rate = 0, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0|50|Classic Quine-McCluskey: failure_rate
claim("a11|0|50|Classic Quine-McCluskey|failure_rate", cell(a11, "failure_rate", error_rate = 0, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0|50|Consistency Cubes: correct_rate
claim("a11|0|50|Consistency Cubes|correct_rate", cell(a11, "correct_rate", error_rate = 0, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0|50|Consistency Cubes: failure_rate
claim("a11|0|50|Consistency Cubes|failure_rate", cell(a11, "failure_rate", error_rate = 0, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0|50|Least Squares: correct_rate
claim("a11|0|50|Least Squares|correct_rate", cell(a11, "correct_rate", error_rate = 0, N = 50, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0|50|Least Squares: failure_rate
claim("a11|0|50|Least Squares|failure_rate", cell(a11, "failure_rate", error_rate = 0, N = 50, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0.2|50|Classic Quine-McCluskey: correct_rate
claim("a11|0.2|50|Classic Quine-McCluskey|correct_rate", cell(a11, "correct_rate", error_rate = 0.2, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0.2|50|Classic Quine-McCluskey: failure_rate
claim("a11|0.2|50|Classic Quine-McCluskey|failure_rate", cell(a11, "failure_rate", error_rate = 0.2, N = 50, estimator = "Classic Quine-McCluskey"), 2L)

# Appendix S3.2.2e, 0.2|50|Consistency Cubes: correct_rate
claim("a11|0.2|50|Consistency Cubes|correct_rate", cell(a11, "correct_rate", error_rate = 0.2, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0.2|50|Consistency Cubes: failure_rate
claim("a11|0.2|50|Consistency Cubes|failure_rate", cell(a11, "failure_rate", error_rate = 0.2, N = 50, estimator = "Consistency Cubes"), 2L)

# Appendix S3.2.2e, 0.2|50|Least Squares: correct_rate
claim("a11|0.2|50|Least Squares|correct_rate", cell(a11, "correct_rate", error_rate = 0.2, N = 50, estimator = "Least Squares"), 2L)

# Appendix S3.2.2e, 0.2|50|Least Squares: failure_rate
claim("a11|0.2|50|Least Squares|failure_rate", cell(a11, "failure_rate", error_rate = 0.2, N = 50, estimator = "Least Squares"), 2L)

# Appendix 3.2.3: nested analysis by confounding ----

# As one might expect, the original model is biased if there is truly confounding.
claim("a12|descriptive|original_model_biased",
      abs(cell(a12, "bias", rho = 0.5, estimator = "Original Model")) >
  abs(cell(a12, "bias", rho = 0, estimator = "Original Model")))

# Appendix S3.2.3a, 0|New Model: bias
claim("a12|0|New Model|bias", cell(a12, "bias", rho = 0, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0|New Model: rmse
claim("a12|0|New Model|rmse", cell(a12, "rmse", rho = 0, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0|New Model: mean_estimate
claim("a12|0|New Model|mean_estimate", cell(a12, "mean_estimate", rho = 0, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0|New Model: mean_estimand
claim("a12|0|New Model|mean_estimand", cell(a12, "mean_estimand", rho = 0, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0|No Model: bias
claim("a12|0|No Model|bias", cell(a12, "bias", rho = 0, estimator = "No Model"), 0L)

# Appendix S3.2.3a, 0|No Model: rmse
claim("a12|0|No Model|rmse", cell(a12, "rmse", rho = 0, estimator = "No Model"), 0L)

# Appendix S3.2.3a, 0|No Model: mean_estimate
claim("a12|0|No Model|mean_estimate", cell(a12, "mean_estimate", rho = 0, estimator = "No Model"), 0L)

# Appendix S3.2.3a, 0|No Model: mean_estimand
claim("a12|0|No Model|mean_estimand", cell(a12, "mean_estimand", rho = 0, estimator = "No Model"), 2L)

# Appendix S3.2.3a, 0|Original Model: bias
claim("a12|0|Original Model|bias", cell(a12, "bias", rho = 0, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0|Original Model: rmse
claim("a12|0|Original Model|rmse", cell(a12, "rmse", rho = 0, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0|Original Model: mean_estimate
claim("a12|0|Original Model|mean_estimate", cell(a12, "mean_estimate", rho = 0, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0|Original Model: mean_estimand
claim("a12|0|Original Model|mean_estimand", cell(a12, "mean_estimand", rho = 0, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0.5|New Model: bias
claim("a12|0.5|New Model|bias", cell(a12, "bias", rho = 0.5, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0.5|New Model: rmse
claim("a12|0.5|New Model|rmse", cell(a12, "rmse", rho = 0.5, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0.5|New Model: mean_estimate
claim("a12|0.5|New Model|mean_estimate", cell(a12, "mean_estimate", rho = 0.5, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0.5|New Model: mean_estimand
claim("a12|0.5|New Model|mean_estimand", cell(a12, "mean_estimand", rho = 0.5, estimator = "New Model"), 2L)

# Appendix S3.2.3a, 0.5|No Model: bias
claim("a12|0.5|No Model|bias", cell(a12, "bias", rho = 0.5, estimator = "No Model"), 0L)

# Appendix S3.2.3a, 0.5|No Model: rmse
claim("a12|0.5|No Model|rmse", cell(a12, "rmse", rho = 0.5, estimator = "No Model"), 0L)

# Appendix S3.2.3a, 0.5|No Model: mean_estimate
claim("a12|0.5|No Model|mean_estimate", cell(a12, "mean_estimate", rho = 0.5, estimator = "No Model"), 0L)

# Appendix S3.2.3a, 0.5|No Model: mean_estimand
claim("a12|0.5|No Model|mean_estimand", cell(a12, "mean_estimand", rho = 0.5, estimator = "No Model"), 2L)

# Appendix S3.2.3a, 0.5|Original Model: bias
claim("a12|0.5|Original Model|bias", cell(a12, "bias", rho = 0.5, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0.5|Original Model: rmse
claim("a12|0.5|Original Model|rmse", cell(a12, "rmse", rho = 0.5, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0.5|Original Model: mean_estimate
claim("a12|0.5|Original Model|mean_estimate", cell(a12, "mean_estimate", rho = 0.5, estimator = "Original Model"), 2L)

# Appendix S3.2.3a, 0.5|Original Model: mean_estimand
claim("a12|0.5|Original Model|mean_estimand", cell(a12, "mean_estimand", rho = 0.5, estimator = "Original Model"), 2L)

# Appendix 3.2.3: nested analysis by effort ----

# increasing the effort devoted to testing decreases our chances of happening on the right model
claim("a13|descriptive|testing_effort_hurts",
      cell(a13, "prop_sims", model_testing_effort = 1, model_building_effort = 5) >
        cell(a13, "prop_sims", model_testing_effort = 5, model_building_effort = 1))

# Share of simulations reaching the New Model at testing and building effort 1 and 1
claim("a13|1|1|prop_sims", cell(a13, "prop_sims", model_testing_effort = 1, model_building_effort = 1), 4L)

# Share of simulations reaching the New Model at testing and building effort 1 and 5
claim("a13|1|5|prop_sims", cell(a13, "prop_sims", model_testing_effort = 1, model_building_effort = 5), 4L)

# Share of simulations reaching the New Model at testing and building effort 5 and 1
claim("a13|5|1|prop_sims", cell(a13, "prop_sims", model_testing_effort = 5, model_building_effort = 1), 4L)

# Share of simulations reaching the New Model at testing and building effort 5 and 5
claim("a13|5|5|prop_sims", cell(a13, "prop_sims", model_testing_effort = 5, model_building_effort = 5), 4L)

# Appendix 3.2.3: nested analysis by decision threshold ----

# Share of simulations reaching the New Model at thresholds 0.1|0.8|0.05
claim("a14|0.1|0.8|0.05|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.1, SNA_threshold_1 = 0.8, SNA_threshold_2 = 0.05), 4L)

# Share of simulations reaching the New Model at thresholds 0.1|0.8|0.3
claim("a14|0.1|0.8|0.3|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.1, SNA_threshold_1 = 0.8, SNA_threshold_2 = 0.3), 4L)

# Share of simulations reaching the New Model at thresholds 0.1|0.1|0.05
claim("a14|0.1|0.1|0.05|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.1, SNA_threshold_1 = 0.1, SNA_threshold_2 = 0.05), 4L)

# Share of simulations reaching the New Model at thresholds 0.1|0.1|0.3
claim("a14|0.1|0.1|0.3|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.1, SNA_threshold_1 = 0.1, SNA_threshold_2 = 0.3), 4L)

# Share of simulations reaching the New Model at thresholds 0.8|0.1|0.3
claim("a14|0.8|0.1|0.3|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.8, SNA_threshold_1 = 0.1, SNA_threshold_2 = 0.3), 4L)

# Share of simulations reaching the New Model at thresholds 0.8|0.8|0.3
claim("a14|0.8|0.8|0.3|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.8, SNA_threshold_1 = 0.8, SNA_threshold_2 = 0.3), 4L)

# Share of simulations reaching the New Model at thresholds 0.8|0.1|0.05
claim("a14|0.8|0.1|0.05|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.8, SNA_threshold_1 = 0.1, SNA_threshold_2 = 0.05), 4L)

# Share of simulations reaching the New Model at thresholds 0.8|0.8|0.05
claim("a14|0.8|0.8|0.05|prop_sims", cell(a14, "prop_sims", LNA_threshold = 0.8, SNA_threshold_1 = 0.8, SNA_threshold_2 = 0.05), 4L)

# Appendix 3.2.4: model based estimands ----

# Appendix S3.2.4, design_1|aate: mean_estimand
claim("a15|design_1|aate|mean_estimand", cell(a15, "mean_estimand", design = "design_1", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_1|aate: bias
claim("a15|design_1|aate|bias", cell(a15, "bias", design = "design_1", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_1|aate: rmse
claim("a15|design_1|aate|rmse", cell(a15, "rmse", design = "design_1", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_1|aate: power
claim("a15|design_1|aate|power", cell(a15, "power", design = "design_1", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_1|aate: coverage
claim("a15|design_1|aate|coverage", cell(a15, "coverage", design = "design_1", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_1|beta: mean_estimand
claim("a15|design_1|beta|mean_estimand", cell(a15, "mean_estimand", design = "design_1", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_1|beta: bias
claim("a15|design_1|beta|bias", cell(a15, "bias", design = "design_1", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_1|beta: rmse
claim("a15|design_1|beta|rmse", cell(a15, "rmse", design = "design_1", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_1|beta: power
claim("a15|design_1|beta|power", cell(a15, "power", design = "design_1", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_1|beta: coverage
claim("a15|design_1|beta|coverage", cell(a15, "coverage", design = "design_1", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_2|aate: mean_estimand
claim("a15|design_2|aate|mean_estimand", cell(a15, "mean_estimand", design = "design_2", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_2|aate: bias
claim("a15|design_2|aate|bias", cell(a15, "bias", design = "design_2", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_2|aate: rmse
claim("a15|design_2|aate|rmse", cell(a15, "rmse", design = "design_2", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_2|aate: power
claim("a15|design_2|aate|power", cell(a15, "power", design = "design_2", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_2|aate: coverage
claim("a15|design_2|aate|coverage", cell(a15, "coverage", design = "design_2", inquiry = "aate"), 2L)

# Appendix S3.2.4, design_2|beta: mean_estimand
claim("a15|design_2|beta|mean_estimand", cell(a15, "mean_estimand", design = "design_2", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_2|beta: bias
claim("a15|design_2|beta|bias", cell(a15, "bias", design = "design_2", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_2|beta: rmse
claim("a15|design_2|beta|rmse", cell(a15, "rmse", design = "design_2", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_2|beta: power
claim("a15|design_2|beta|power", cell(a15, "power", design = "design_2", inquiry = "beta"), 2L)

# Appendix S3.2.4, design_2|beta: coverage
claim("a15|design_2|beta|coverage", cell(a15, "coverage", design = "design_2", inquiry = "beta"), 2L)

# Appendix 3.2.5: matching on observables ----

# Our diagnosis shows that matching improves mean-squared-error relative to a naive
# difference-in-means estimator of the treatment effect on the treated (ATT), but can
# nevertheless remain biased if the matching algorithm does not successfully pair units with
# equal probabilities of assignment.
claim("a16|descriptive|matching_improves_mse",
      cell(a16, "rmse", estimator = "matching") < cell(a16, "rmse", estimator = "dim") &&
  abs(cell(a16, "bias", estimator = "matching")) > 0)

# Appendix S3.2.5, dim: bias
claim("a16|dim|bias", cell(a16, "bias", estimator = "dim"), 2L)

# Appendix S3.2.5, matching: bias
claim("a16|matching|bias", cell(a16, "bias", estimator = "matching"), 2L)

# Appendix 3.2.6: regression discontinuity ----

# Appendix S3.2.6, single row: bias
claim("a17|bias", cell(a17, "bias"), 2L)

# Appendix S3.2.6, single row: rmse
claim("a17|rmse", cell(a17, "rmse"), 2L)

# Appendix S3.2.6, single row: power
claim("a17|power", cell(a17, "power"), 2L)

# Appendix S3.2.6, single row: coverage
claim("a17|coverage", cell(a17, "coverage"), 2L)

# Appendix 3.3: designs for discovery research ----

# We see that the principled discovery method, using training and testing data, provides
# essentially unbiased estimates, whereas the comparison method tends to provide biased
# estimates.
claim("a18|descriptive|principled_less_biased",
      abs(cell(a18, "bias", estimator = "Test sample")) <
  abs(cell(a18, "bias", estimator = "Full sample")))

# A smaller sample is used in the final test when adopting a principled discovery approach, and
# so on average the estimates are much noisier than under the unprincipled comparison.
claim("a18|descriptive|principled_noisier",
      cell(a18, "RMSE", estimator = "Test sample") > cell(a18, "RMSE", estimator = "Full sample"))

# Appendix S3.3, Main: bias
claim("a18|Main|bias", cell(a18, "bias", estimator = "Main"), 2L)

# Appendix S3.3, Main: RMSE
claim("a18|Main|RMSE", cell(a18, "RMSE", estimator = "Main"), 2L)

# Appendix S3.3, Main: frequency
claim("a18|Main|frequency", cell(a18, "frequency", estimator = "Main"), 2L)

# Appendix S3.3, Main: false_pos
claim("a18|Main|false_pos", cell(a18, "false_pos", estimator = "Main"), 0L)

# Appendix S3.3, Main: false_neg
claim("a18|Main|false_neg", cell(a18, "false_neg", estimator = "Main"), 2L)

# Appendix S3.3, Full sample: bias
claim("a18|Full sample|bias", cell(a18, "bias", estimator = "Full sample"), 2L)

# Appendix S3.3, Full sample: RMSE
claim("a18|Full sample|RMSE", cell(a18, "RMSE", estimator = "Full sample"), 2L)

# Appendix S3.3, Full sample: frequency
claim("a18|Full sample|frequency", cell(a18, "frequency", estimator = "Full sample"), 2L)

# Appendix S3.3, Full sample: false_pos
claim("a18|Full sample|false_pos", cell(a18, "false_pos", estimator = "Full sample"), 2L)

# Appendix S3.3, Full sample: false_neg
claim("a18|Full sample|false_neg", cell(a18, "false_neg", estimator = "Full sample"), 2L)

# Appendix S3.3, Test sample: bias
claim("a18|Test sample|bias", cell(a18, "bias", estimator = "Test sample"), 2L)

# Appendix S3.3, Test sample: RMSE
claim("a18|Test sample|RMSE", cell(a18, "RMSE", estimator = "Test sample"), 2L)

# Appendix S3.3, Test sample: frequency
claim("a18|Test sample|frequency", cell(a18, "frequency", estimator = "Test sample"), 2L)

# Appendix S3.3, Test sample: false_pos
claim("a18|Test sample|false_pos", cell(a18, "false_pos", estimator = "Test sample"), 2L)

# Appendix S3.3, Test sample: false_neg
claim("a18|Test sample|false_neg", cell(a18, "false_neg", estimator = "Test sample"), 2L)

# Appendix 4.5: diagnosis of the original design ----

# Appendix S4.5, Mortality estimate: mean_estimand
claim("a19|Mortality estimate|mean_estimand", cell(a19, "mean_estimand", estimator = "Mortality estimate"), 2L)

# Appendix S4.5, Mortality estimate: mean_estimate
claim("a19|Mortality estimate|mean_estimate", cell(a19, "mean_estimate", estimator = "Mortality estimate"), 2L)

# Appendix S4.5, Mortality estimate: bias
claim("a19|Mortality estimate|bias", cell(a19, "bias", estimator = "Mortality estimate"), 2L)

# Appendix S4.5, Mortality estimate: power
claim("a19|Mortality estimate|power", cell(a19, "power", estimator = "Mortality estimate"), 2L)

# Appendix S4.5, Weight estimate: mean_estimand
claim("a19|Weight estimate|mean_estimand", cell(a19, "mean_estimand", estimator = "Weight estimate"), 2L)

# Appendix S4.5, Weight estimate: mean_estimate
claim("a19|Weight estimate|mean_estimate", cell(a19, "mean_estimate", estimator = "Weight estimate"), 2L)

# Appendix S4.5, Weight estimate: bias
claim("a19|Weight estimate|bias", cell(a19, "bias", estimator = "Weight estimate"), 2L)

# Appendix S4.5, Weight estimate: power
claim("a19|Weight estimate|power", cell(a19, "power", estimator = "Weight estimate"), 2L)

# Considering the under-5 mortality rate first, we see that the true population average
# treatment effect is 0.006 percentage points.
claim("a19|Mortality estimate|prose_mean_estimand", cell(a19, "mean_estimand", estimator = "Mortality estimate"), 3L)

# The coverage is correct, at 95%.
claim("a19|Mortality estimate|prose_coverage", cell(a19, "coverage", estimator = "Mortality estimate"), 2L)

# The simulation presented above shows that we are relatively under-powered for the mortality
# estimand, only 0.1% of simulations returned a statistically significant result.
claim("a19|Mortality estimate|prose_power", cell(a19, "power", estimator = "Mortality estimate"), 3L)

# Appendix 4.6: increasing sample size ----

# on average, statistically significant estimates tend to exaggerate the true effect of the
# intervention on mortality by a factor of two and on weight-for-age by a factor of four
claim("a20|prose|mortality_exaggeration", cell(a20, "exaggeration_ratio", design = "bjorkman_svensson_design", estimator = "Mortality estimate"), 0L)

# on average, statistically significant estimates tend to exaggerate the true effect of the
# intervention on mortality by a factor of two and on weight-for-age by a factor of four
claim("a20|prose|weight_exaggeration", cell(a20, "exaggeration_ratio", design = "bjorkman_svensson_design", estimator = "Weight estimate"), 0L)

# using the same sample size as that employed in Raffler, Posner, and Parkerson (2019) reduces
# the exaggeration ratio on the mortality estimand to where it should be, around one
claim("a20|prose|rpp_mortality_exaggeration", cell(a20, "exaggeration_ratio", design = "RPP_replication", estimator = "Mortality estimate"), 0L)

# Appendix S4.6, bjorkman_svensson_design|Mortality estimate: exaggeration_ratio
claim("a20|bjorkman_svensson_design|Mortality estimate|exaggeration_ratio", cell(a20, "exaggeration_ratio", design = "bjorkman_svensson_design", estimator = "Mortality estimate"), 2L)

# Appendix S4.6, bjorkman_svensson_design|Mortality estimate: power
claim("a20|bjorkman_svensson_design|Mortality estimate|power", cell(a20, "power", design = "bjorkman_svensson_design", estimator = "Mortality estimate"), 2L)

# Appendix S4.6, bjorkman_svensson_design|Weight estimate: exaggeration_ratio
claim("a20|bjorkman_svensson_design|Weight estimate|exaggeration_ratio", cell(a20, "exaggeration_ratio", design = "bjorkman_svensson_design", estimator = "Weight estimate"), 2L)

# Appendix S4.6, bjorkman_svensson_design|Weight estimate: power
claim("a20|bjorkman_svensson_design|Weight estimate|power", cell(a20, "power", design = "bjorkman_svensson_design", estimator = "Weight estimate"), 2L)

# Appendix S4.6, RPP_replication|Mortality estimate: exaggeration_ratio
claim("a20|RPP_replication|Mortality estimate|exaggeration_ratio", cell(a20, "exaggeration_ratio", design = "RPP_replication", estimator = "Mortality estimate"), 2L)

# Appendix S4.6, RPP_replication|Mortality estimate: power
claim("a20|RPP_replication|Mortality estimate|power", cell(a20, "power", design = "RPP_replication", estimator = "Mortality estimate"), 2L)

# Appendix S4.6, RPP_replication|Weight estimate: exaggeration_ratio
claim("a20|RPP_replication|Weight estimate|exaggeration_ratio", cell(a20, "exaggeration_ratio", design = "RPP_replication", estimator = "Weight estimate"), 2L)

# Appendix S4.6, RPP_replication|Weight estimate: power
claim("a20|RPP_replication|Weight estimate|power", cell(a20, "power", design = "RPP_replication", estimator = "Weight estimate"), 2L)

# Appendix 4.7: adding covariates ----

# We note first that including the interaction term as done by the replicators is a strictly
# dominated strategy from the standpoint of reducing mean squared error: irrespective of whether
# CBO presence is correlated with health outcomes or imbalanced, the RMSE expected under this
# strategy is higher than under any other strategy.
claim("a21|descriptive|interaction_dominated",
      cell(a21, "rmse", design = "DM_replication_CBO_ind", estimator = "Interact CBO") ==
    max(a21$rmse[a21$design == "DM_replication_CBO_ind"]) &&
  cell(a21, "rmse", design = "DM_replication_CBO_cor", estimator = "Interact CBO") ==
    max(a21$rmse[a21$design == "DM_replication_CBO_cor"]))

# Appendix S4.7, DM_replication_CBO_ind|Ignore CBO: rmse
claim("a21|DM_replication_CBO_ind|Ignore CBO|rmse", cell(a21, "rmse", design = "DM_replication_CBO_ind", estimator = "Ignore CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Ignore CBO: rmse_balanced
claim("a21|DM_replication_CBO_ind|Ignore CBO|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_ind", estimator = "Ignore CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Ignore CBO: rmse_imbalanced
claim("a21|DM_replication_CBO_ind|Ignore CBO|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_ind", estimator = "Ignore CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Include CBO: rmse
claim("a21|DM_replication_CBO_ind|Include CBO|rmse", cell(a21, "rmse", design = "DM_replication_CBO_ind", estimator = "Include CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Include CBO: rmse_balanced
claim("a21|DM_replication_CBO_ind|Include CBO|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_ind", estimator = "Include CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Include CBO: rmse_imbalanced
claim("a21|DM_replication_CBO_ind|Include CBO|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_ind", estimator = "Include CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Include CBO if imbalanced: rmse
claim("a21|DM_replication_CBO_ind|Include CBO if imbalanced|rmse", cell(a21, "rmse", design = "DM_replication_CBO_ind", estimator = "Include CBO if imbalanced"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Include CBO if imbalanced: rmse_balanced
claim("a21|DM_replication_CBO_ind|Include CBO if imbalanced|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_ind", estimator = "Include CBO if imbalanced"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Include CBO if imbalanced: rmse_imbalanced
claim("a21|DM_replication_CBO_ind|Include CBO if imbalanced|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_ind", estimator = "Include CBO if imbalanced"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Interact CBO: rmse
claim("a21|DM_replication_CBO_ind|Interact CBO|rmse", cell(a21, "rmse", design = "DM_replication_CBO_ind", estimator = "Interact CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Interact CBO: rmse_balanced
claim("a21|DM_replication_CBO_ind|Interact CBO|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_ind", estimator = "Interact CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_ind|Interact CBO: rmse_imbalanced
claim("a21|DM_replication_CBO_ind|Interact CBO|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_ind", estimator = "Interact CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Ignore CBO: rmse
claim("a21|DM_replication_CBO_cor|Ignore CBO|rmse", cell(a21, "rmse", design = "DM_replication_CBO_cor", estimator = "Ignore CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Ignore CBO: rmse_balanced
claim("a21|DM_replication_CBO_cor|Ignore CBO|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_cor", estimator = "Ignore CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Ignore CBO: rmse_imbalanced
claim("a21|DM_replication_CBO_cor|Ignore CBO|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_cor", estimator = "Ignore CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Include CBO: rmse
claim("a21|DM_replication_CBO_cor|Include CBO|rmse", cell(a21, "rmse", design = "DM_replication_CBO_cor", estimator = "Include CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Include CBO: rmse_balanced
claim("a21|DM_replication_CBO_cor|Include CBO|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_cor", estimator = "Include CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Include CBO: rmse_imbalanced
claim("a21|DM_replication_CBO_cor|Include CBO|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_cor", estimator = "Include CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Include CBO if imbalanced: rmse
claim("a21|DM_replication_CBO_cor|Include CBO if imbalanced|rmse", cell(a21, "rmse", design = "DM_replication_CBO_cor", estimator = "Include CBO if imbalanced"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Include CBO if imbalanced: rmse_balanced
claim("a21|DM_replication_CBO_cor|Include CBO if imbalanced|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_cor", estimator = "Include CBO if imbalanced"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Include CBO if imbalanced: rmse_imbalanced
claim("a21|DM_replication_CBO_cor|Include CBO if imbalanced|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_cor", estimator = "Include CBO if imbalanced"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Interact CBO: rmse
claim("a21|DM_replication_CBO_cor|Interact CBO|rmse", cell(a21, "rmse", design = "DM_replication_CBO_cor", estimator = "Interact CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Interact CBO: rmse_balanced
claim("a21|DM_replication_CBO_cor|Interact CBO|rmse_balanced", cell(a21, "rmse_balanced", design = "DM_replication_CBO_cor", estimator = "Interact CBO"), 2L)

# Appendix S4.7, DM_replication_CBO_cor|Interact CBO: rmse_imbalanced
claim("a21|DM_replication_CBO_cor|Interact CBO|rmse_imbalanced", cell(a21, "rmse_imbalanced", design = "DM_replication_CBO_cor", estimator = "Interact CBO"), 2L)


# Definitional, structural and transcribed numerals ----
# These have no pipeline counterpart by design. A design parameter is checked
# against the script that declares it, a transcribed number is named with the
# source it came from, and a structural numeral is simply recorded.

declared_in <- function(script, literal) {
  found <- any(str_detect(read_lines(here::here("maintained", script)), fixed(literal)))
  stopifnot(found)
  str_c("declared in ", script, " as ", literal)
}

# Table 1 lists examples of diagnosands against the elements of a design
# Article prints: (no quantity)
claim("table_1|none", "no quantity printed")

# Table 3's bottom panel prints the code that diagnoses the declared design for bias over m draws
# Article prints: (no quantity)
claim("table_3|bias", "no quantity printed")

# p_U <- declare_population(N = 200, u = rnorm(N))
# Article prints: 200
claim("table_3|population_N", declared_in("table_3_declaration.R", "declare_model(N = 200"))

# p_S <- declare_sampling(n = 100)
# Article prints: 100
claim("table_3|sample_n", declared_in("table_3_declaration.R", "n = 100"))

# p_Z <- declare_assignment(m = 50)
# Article prints: 50
claim("table_3|treated_m", declared_in("table_3_declaration.R", "m = 50"))

# Table 4 numbers four scenarios 1 to 4 by whether the model or the answer strategy differs from the author's
# Article prints: (no quantity)
claim("table_4|none", "no quantity printed")

# Results correspond to design tool census concluded in July 2017 and do not include tools
# published since then.
# Article prints: 2017
claim("text|census_date", "July 2017, stated in the note to Table 2")

# We assessed the first 30 results using these terms.
# Article prints: 30
claim("text|search_results_assessed", "30, stated as the search procedure in appendix section 2.2")

# One prominent checklist in medicine includes 22 features, while other proposals range from
# nine to 60 components.
# Article prints: 22
claim("text|design_checklist_features", "22, from the medical checklist the article cites")

# One prominent checklist in medicine includes 22 features, while other proposals range from
# nine to 60 components.
# Article prints: 9 to 60
claim("text|checklist_range", "9 to 60, from the other proposals the article cites")

# The statistic s = 1(p <= 0.05), interpreted as "statistically significant at the 5% level", is a
# diagnostic statistic that does not refer to an estimand.
# Article prints: 0.05
claim("text|significance_level", "0.05, the convention the article names in defining the statistic")

# When rounded to the third decimal place, the PATE is ~ .406 with a standard deviation of 1.01,
# producing a Cohen's d of approximately .4. Thus, when a tool asked for an effect size without
# specifying what kind of effect, we entered a value of .4.
# Article prints: 0.4
claim("a2|prose|cohens_d", "0.4, the effect size entered into the surveyed tools")

# When rounded to the third decimal place, the PATE is ~ .406 with a standard deviation of 1.01,
# producing a Cohen's d of approximately .4.
# Article prints: 1.01
# No deposited script computes a standard deviation of an outcome and no file in output/ carries
# one, so there is nothing to read. The sentence does not say which standard deviation it reports.
claim("a2|prose|outcome_sd", "no output file carries a standard deviation of an outcome")

# There are 1000 city blocks to choose from, each of which contains exactly 25 or 50 households,
# with the j'th block size distributed categorically. Specifically, the expected sample size of the
# study is N = 5 x E[n] = 5 x 37.5 = 187.5.
# Article prints: 37.5
block_sizes <- c(25, 50)
blocks_sampled <- 5
invisible(declared_in("table_a2_working_example_census.R", "sample(c(25, 50), N, TRUE)"))
invisible(declared_in("table_a2_working_example_census.R", "cluster_rs(clusters = block, n = 5)"))
claim("a2|prose|expected_block_size", mean(block_sizes), 1L)

# There are 1000 city blocks to choose from, each of which contains exactly 25 or 50 households,
# with the j'th block size distributed categorically. Specifically, the expected sample size of the
# study is N = 5 x E[n] = 5 x 37.5 = 187.5.
# Article prints: 187.5
claim("a2|prose|expected_n", blocks_sampled * mean(block_sizes), 1L)

# In our simulations, we estimate the true standard error to be 0, which is close to the standard
# error reported in the original paper of 0.026.
# Article prints: 0.026
claim("text|bs_original_se", "0.026, from Bjorkman and Svensson (2009)")

# We assume that there is a base rate of survival of approximately 86%, and that treatment increases
# the probability of survival by approximately 5 percentage points. In logits, this is moving from
# plogis(1.81) = 86% to plogis(1.81 + 0.5) = 91%.
# Article prints: 1.81
# The declaration writes the intercept as a survival probability and the shift as a logit, so the
# four numbers of this sentence are recovered from those two rather than typed. The logit of 0.86 is
# 1.8153, which the sentence prints as 1.81, so it is printed here at four places rather than two.
invisible(declared_in("bs_design.R", "plogis(qlogis(0.86) + 0.5 * Z"))
claim("text|bs_logit", qlogis(0.86), 4L)

# We assume that there is a base rate of survival of approximately 86% ...
# Article prints: 86%
claim("text|bs_base_survival", 100 * plogis(qlogis(0.86)), 0L, percent = TRUE)

# ... to plogis(1.81 + 0.5) = 91%.
# Article prints: 91%
claim("text|bs_treated_survival", 100 * plogis(qlogis(0.86) + 0.5), 0L, percent = TRUE)

# ... treatment increases the probability of survival by approximately 5 percentage points.
# Article prints: 5
claim("text|bs_survival_effect", 100 * (plogis(qlogis(0.86) + 0.5) - plogis(qlogis(0.86))), 0L)

# Including those other 189 clusters would only further increase power.
# Article prints: 189
claim("text|rpp_other_clusters", "189, from Raffler, Posner and Parkerson (2019)")

# the eighteen community-based organizations who carried out the original 'power to the people'
# (P2P) intervention were active in 64% of the treatment communities and 48% of the control
# communities
# Article prints: 64%
claim("text|cbo_prevalence", "64% and 48%, from Donato and Garcia Mosqueira (2016)")

# Researchers may be interested in whether their design meets the standard power target of 0.80.
# Article prints: 0.80
claim("text|power_target", "0.80, a convention the article names in passing")

# reference values such as statistical power of 0.8
# Article prints: 0.8
claim("text|alpha_reference", "0.8, a convention the article names in passing")

# some have proposed lowering the threshold for statistical significance to a smaller value
# (e.g., 0.005)
# Article prints: 0.005
claim("text|small_p_value", "0.005, a threshold the article attributes to others")

# an assignment strategy in which subjects are assigned to a control condition, treatment 1, or
# treatment 2, each with probability 1/3
# Article prints: 1/3
claim("text|factorial_prob_three_arm", declared_in("figure_1_experimental_designs.R", "m_arms = 3"))

# an alternative strategy in which we assign subjects to each of four possible combinations of
# factors with probability 1/4
# Article prints: 1/4
claim("text|factorial_prob_2x2", declared_in("figure_1_experimental_designs.R", "two_by_two_designer"))

# we let the interaction between treatments vary over the range -0.2 to +0.2
# Article prints: -0.2 to +0.2
claim("text|interaction_range", declared_in("figure_1_experimental_designs.R", "seq(-0.2, 0.2, length.out = 9)"))

# evidence that is more likely to be seen when the hypothesis is true, Pr(E1|H) = 0.75
# Article prints: 0.75
claim("text|pr_E1_H", declared_in("table_a5_process_tracing.R", "pr_SIW_H <- .75"))

# the probability of observing the first CPO when the hypothesis is false, Pr(E1|not H) = 0.25
# Article prints: 0.25
claim("text|pr_E1_notH", declared_in("table_a5_process_tracing.R", "pr_SIW_not_H <- .25"))

# the probability of observing the evidence when the hypothesis that X caused Y is true,
# Pr(E2|H) is 0.30
# Article prints: 0.30
claim("text|pr_E2_H", declared_in("table_a5_process_tracing.R", "pr_SMG_H <- .30"))

# the probability of observing the evidence when the hypothesis is false, Pr(E2|not H) is only
# 0.05
# Article prints: 0.05
claim("text|pr_E2_notH", declared_in("table_a5_process_tracing.R", "pr_SMG_not_H <- .05"))

# Configurations that predict the presence of Y with probability greater than 0.5 are then
# included in the set of sufficient conditions.
# Article prints: 0.5
claim("text|qca_inclusion_cut", declared_in("table_a7_qca.R", "incl.cut = 0.5"))

# child mortality, defined as the number of deaths per 1,000 live births among under-5 year-olds
# Article prints: 1,000
claim("text|bs_mortality_definition", "1,000 live births, the unit of the original study's mortality measure")

# As in the original article we stratify sampling on catchment area and cluster-assign
# households in 25 of the 50 catchment areas to the intervention.
# Article prints: 25
# The 25 is not written anywhere: fifty catchment areas are sampled and cluster_ra() assigns half
# of them at its default probability, so both halves of the sentence are checked here.
claim("text|bs_clusters",
      str_c(declared_in("bs_design.R", "N_catchment_areas <- 50"), ", and ",
            declared_in("bs_design.R", "cluster_ra(clusters = catchment_area)"),
            ", which treats half of them"))

# we redesign the original study to include 187 clusters instead of 50
# Article prints: 187
claim("text|rpp_clusters", as.character(max(a20$N_catchment_areas)))

# the ratio of the average absolute value of statistically significant estimates to the true
# effect, at the a = 0.05 level
# Article prints: 0.05
# The sentence says 0.05 and the deposited diagnosand selects on p.value < 0.1, which the rewrite
# carries forward unchanged because it is the estimator the published table was computed with.
# The printed string is the threshold the code uses, not the one the sentence names.
claim("text|exaggeration_alpha", declared_in("table_a20_bs_exaggeration.R", "p.value < 0.1"))
