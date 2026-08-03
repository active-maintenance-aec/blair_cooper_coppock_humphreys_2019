# blair_cooper_coppock_humphreys_2019/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_write_calls.csv
# Depends on: the unpacked deposit (original_extracted/, or ARCHIVE_RUN_DIR)
# Description: Run every script the deposit ships, twice: once against the
#   deposit as it was downloaded, and once against a copy stripped to data plus
#   code. Record for each script whether it completed and, if not, where it
#   stopped. Also record every uncommented call in the deposit that writes to
#   disk, and which deposited files a run would overwrite.
#
#   Both passes run in scratch copies under tempdir() and never in the deposit
#   itself. That matters here rather than in principle: the deposit's scripts
#   write into rds_files/, figure/ and output/, and every file in those three
#   directories is a deposited member, so running the deposit where it sits
#   would overwrite part of the deposit with the output of the run.
#
#   The stripped pass removes the deposit's own saved diagnosis objects and
#   printed logs. Every script reads a saved object back after computing it, so
#   a script that fails to compute can still finish by loading the shipped copy,
#   and the stripped pass is what separates those two outcomes.

library(tidyverse)
library(here)

here::i_am("ground_truth/run_archive.R")

archive_dir <- Sys.getenv(
  "ARCHIVE_RUN_DIR",
  unset = here::here("original_extracted", "replication_archive")
)

stopifnot(dir.exists(file.path(archive_dir, "R")))

script_names <- setdiff(
  list.files(file.path(archive_dir, "R"), pattern = "\\.R$"),
  "knitr_setup.R"
)

# Write calls in the deposited code ----
# A line counts as a write only if it is not commented out, because a commented
# call produces nothing and an archive that produces nothing cannot be compared
# against anything.
write_call_pattern <- str_c(
  "write_rds|saveRDS|write\\.csv|write_csv|write\\.table|ggsave|pdf\\(|png\\(",
  "|sink\\(|writeLines|write_lines|file\\.copy|unlink|dir\\.create|cat\\("
)

deposited_r_files <- list.files(
  archive_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE
)

write_calls <- deposited_r_files |>
  set_names(\(paths) str_remove(paths, str_c(archive_dir, "/"))) |>
  map(\(path) tibble(line = seq_along(read_lines(path)), text = read_lines(path))) |>
  list_rbind(names_to = "script") |>
  filter(
    str_detect(text, write_call_pattern),
    !str_detect(str_trim(text), "^#")
  ) |>
  mutate(text = str_trim(text))

write_csv(write_calls, here::here("ground_truth", "archive_write_calls.csv"))

# Two passes ----
prepare_copy <- function(label, strip) {
  destination <- file.path(tempdir(), str_c("bcch_archive_", label))
  unlink(destination, recursive = TRUE)
  dir.create(destination, recursive = TRUE)
  file.copy(
    list.files(archive_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    destination, recursive = TRUE
  )
  if (strip) {
    unlink(file.path(destination, c("rds_files", "output", "figure")), recursive = TRUE)
    dir.create(file.path(destination, "rds_files"))
    dir.create(file.path(destination, "output"))
    dir.create(file.path(destination, "figure"))
  }
  destination
}

# Each script runs in its own R process, so a script that leaves the session in a
# broken state cannot change the verdict on the next one, and the deposit's own
# working directory assumption (it is written to run from R/) is honoured. The
# preamble is the one run_replication.R sets: recompute every diagnosis, at the
# published simulation and bootstrap counts, under the deposit's own seed.
#
# exec matters. Without it the shell stays alive as the parent, the timeout kills
# the shell, and the R process it launched carries on as an orphan for as long as
# it likes. With it the shell is replaced by R, so the process the timeout can
# reach is the one doing the work.
script_timeout_seconds <- 600

run_one <- function(directory, script) {
  preamble <- str_glue(
    "set.seed(90095); do_all_diagnoses <- TRUE; sims <- 5000; b_sims <- 5000; ",
    "source(\"{script}\", echo = FALSE)"
  )
  command <- str_glue(
    "cd {shQuote(file.path(directory, 'R'))} && exec Rscript -e {shQuote(preamble)} 2>&1"
  )
  output <- suppressWarnings(
    system(command, intern = TRUE, timeout = script_timeout_seconds)
  )
  status <- attr(output, "status")
  first_error <- output[str_detect(output, "^Error")][1]
  timed_out <- !is.null(status) && status == 124
  tibble(
    script = script,
    completed = as.integer(is.null(status) || status == 0),
    # The timeout is tested first. A script killed part way through prints an
    # error of its own on the way down, so taking the first error line would
    # record an interrupted worker as the reason the script stopped and hide the
    # fact that it was still running when it was killed.
    stopped_at = case_when(
      timed_out ~ str_glue("Did not finish within {script_timeout_seconds} seconds."),
      !is.na(first_error) ~ str_trunc(first_error, 200),
      .default = NA_character_
    )
  )
}

# The deposited scripts are single threaded and write to distinct files, so the
# pass runs them side by side. Sequentially a pass is bounded by the sum of the
# timeouts rather than by the longest one, which is hours of waiting for an
# answer that does not depend on the order.
run_pass <- function(label, strip) {
  directory <- prepare_copy(label, strip)
  before <- file.info(list.files(directory, recursive = TRUE, full.names = TRUE))
  status <- parallel::mclapply(
    script_names,
    \(s) run_one(directory, s),
    mc.cores = max(1, parallel::detectCores() - 1)
  ) |>
    list_rbind()
  after <- file.info(list.files(directory, recursive = TRUE, full.names = TRUE))
  overwritten <- rownames(before)[
    before$mtime != after[rownames(before), "mtime"] & !is.na(after[rownames(before), "mtime"])
  ]
  attr(status, "overwritten") <- str_remove(overwritten, str_c(directory, "/"))
  status |> mutate(pass = label, .before = 1)
}

as_shipped <- run_pass("as_shipped", strip = FALSE)
stripped <- run_pass("stripped", strip = TRUE)

archive_run_status <- bind_rows(as_shipped, stripped) |>
  mutate(
    deposited_files_overwritten = if_else(
      pass == "as_shipped",
      str_c(attr(as_shipped, "overwritten"), collapse = "; "),
      NA_character_
    )
  )

write_csv(archive_run_status, here::here("ground_truth", "archive_run_status.csv"))

print(str_glue(
  "Deposited scripts: {nrow(as_shipped)}. Completed as shipped: ",
  "{sum(as_shipped$completed)}. Completed stripped: {sum(stripped$completed)}. ",
  "Uncommented write calls in the deposit: {nrow(write_calls)}. ",
  "Deposited files a run as shipped overwrites: {length(attr(as_shipped, 'overwritten'))}."
))
