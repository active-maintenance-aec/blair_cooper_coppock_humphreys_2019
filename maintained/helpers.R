# blair_cooper_coppock_humphreys_2019/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages, paths and shared helpers for every script in maintained/.

# QCA and Matching are deliberately not attached. Matching attaches MASS, which
# masks dplyr::select, and namespacing it is enough to avoid that.
#
# QCA is a different matter and namespacing is not enough. Loading it loads the
# declared package, whose load hook writes over fourteen bindings in base and
# stats, among them rbind.data.frame(), which then errors on any list argument
# that is not a data frame. DesignLibrary::multi_arm_designer()'s estimator
# builds its result with exactly that call. Reaching QCA through its namespace
# still loads it, so the replacement happens on the first QCA:: call and lasts
# for the rest of the session. What protects the figure is running it first, and
# the assertion in figure_1_experimental_designs.R is what makes that visible
# rather than accidental.
library(here)
library(DeclareDesign)
library(DesignLibrary)
library(estimatr)
library(broom)
library(knitr)
library(kableExtra)
library(future)
library(tidyverse)

here::i_am("maintained/helpers.R")

# The published diagnoses all use 5000 simulations and 5000 bootstrap replicates,
# set in the archive's knitr_setup.R. Every script below uses the same counts.
published_sims <- 5000
published_bootstrap_sims <- 5000

# The archive carries two seeds. run_replication.R sets 90095 before its loop,
# and every script it sources then sources knitr_setup.R, which sets 90405. The
# second one is therefore the seed each deposited script actually runs under.
published_seed <- 90405

# diagnose_design() spreads simulations over the future package and returns the
# same numbers under any plan, which was checked against a sequential run at
# several worker counts. Forking is used where the platform allows it; elsewhere
# the run is sequential, slower, and numerically identical.
if (future::supportsMulticore()) {
  plan(multicore, workers = max(1, availableCores() - 1))
} else {
  plan(sequential)
}

out_path <- function(...) here::here("maintained", "output", ...)

# get_diagnosands() returns one row per design-inquiry-estimator with the
# diagnosand estimates and their bootstrap standard errors in parallel columns.
# Every table in maintained/output/ is written from this, so the CSVs and the
# report cannot disagree about what was computed.
write_diagnosands <- function(diagnosis, file) {
  tab <- get_diagnosands(diagnosis) |> as_tibble()
  write_csv(tab, out_path(file))
  tab
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
