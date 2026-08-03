# blair_cooper_coppock_humphreys_2019/verify_original.R
# Output: none (stops on any disagreement)
# Depends on: original_manifest.csv, original_members_manifest.csv
# Description: Check that the deposit on disk is the deposit Dataverse served,
#   and that the unpacked working tree is the deposit's own contents. Verifies
#   byte sizes and MD5s, and stops if either directory holds anything the
#   manifests do not list.
#
#   This runs twice per full reproduction. download_original.R sources it as a
#   precondition, and run_all.R sources it again as its own last step, because a
#   precondition cannot catch a script that damages the deposit mid-run. The
#   deposit's own code writes into rds_files/, figure/ and output/, all of which
#   are deposited members, so this failure mode is live rather than theoretical.

library(tidyverse)
library(here)

here::i_am("verify_original.R")

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)
members <- read_csv(here::here("original_members_manifest.csv"), show_col_types = FALSE)

# The deposited file ----
deposit <- manifest |>
  mutate(
    path = here::here("original", file),
    bytes_local = file.size(path),
    md5_local = unname(tools::md5sum(path)),
    bytes_ok = !is.na(bytes_local) & bytes_local == bytes,
    md5_ok = !is.na(md5_local) & md5_local == md5_served,
    published_agrees = md5_served == md5_published
  )

if (!all(deposit$bytes_ok) || !all(deposit$md5_ok)) {
  stop(str_glue("original/ does not match original_manifest.csv: ",
                "{paste(deposit$file[!(deposit$bytes_ok & deposit$md5_ok)], collapse = ', ')}"))
}

# all.files is not optional here: without it a stray dotfile passes silently,
# and dotfiles can be genuine deposited members.
deposit_strays <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)

if (length(deposit_strays) > 0) {
  stop(str_glue("original/ holds files the manifest does not list: ",
                "{paste(deposit_strays, collapse = ', ')}"))
}

# The unpacked members ----
extract_dir <- here::here("original_extracted")

if (!dir.exists(extract_dir)) {
  stop("original_extracted/ is missing. Run download_original.R to rebuild it.")
}

extracted <- tibble(
  member = list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
) |>
  mutate(
    bytes_extracted = file.size(file.path(extract_dir, member)),
    md5_extracted = unname(tools::md5sum(file.path(extract_dir, member)))
  )

member_check <- members |>
  full_join(extracted, by = "member") |>
  mutate(
    ok = !is.na(md5) & !is.na(md5_extracted) & md5 == md5_extracted &
      bytes == bytes_extracted
  )

if (!all(member_check$ok)) {
  bad <- member_check |> filter(!ok) |> pull(member)
  stop(str_glue("original_extracted/ does not match original_members_manifest.csv ",
                "({sum(!member_check$ok)} members): {paste(head(bad, 10), collapse = ', ')}"))
}

print(str_glue(
  "Deposit verified: {nrow(deposit)} file and {nrow(member_check)} members match on ",
  "size and MD5; {sum(!deposit$published_agrees)} carry a published checksum that ",
  "disagrees with what Dataverse serves."
))
