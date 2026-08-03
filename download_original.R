# blair_cooper_coppock_humphreys_2019/download_original.R
# Output: original/ (the deposited archive, not redistributed in this repo),
#         original_extracted/ (the unpacked working tree, rebuilt from scratch)
# Depends on: original_manifest.csv, verify_original.R
# Description: Fetch the deposited archive from Harvard Dataverse, unpack it, and
#   verify both. Run this once before running anything in maintained/.
#   Re-running is free: a file already present with the right size and checksum
#   is not downloaded again.
#
#   The manifest carries two checksums. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. Here the two agree,
#   but they do not always, so verification runs against md5_served and any
#   disagreement is reported rather than hidden by a check that would fail for
#   the wrong reason.
#
#   The deposit is a single zip, so a check that original/ holds nothing extra is
#   a check on one file and says nothing about the 620 members inside it. Those
#   get their own manifest, and the zip is unpacked into original_extracted/,
#   which this script rebuilds from scratch and nothing else writes to. That
#   separation matters here: the deposit's own scripts write back into
#   rds_files/, figure/ and output/, all of which are deposited members, so a run
#   of the deposited code in place would overwrite part of the deposit.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/XYT1VB"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than any tabular
# representation Dataverse might derive.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    bytes_local = file.size(path),
    needs_download = is.na(md5_local) | md5_local != md5_served |
      is.na(bytes_local) | bytes_local != bytes
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Unpack ----
# original_extracted/ is rebuilt from the zip every time, so nothing a script
# leaves behind can survive into the next run.
extract_dir <- here::here("original_extracted")
unlink(extract_dir, recursive = TRUE)
dir.create(extract_dir)
unzip(here::here("original", manifest$file[[1]]), exdir = extract_dir)

# Verify ----
# The gate itself lives in verify_original.R, which run_all.R sources again after
# the run so that damage done mid-run cannot wait until the next reproduction.
source(here::here("verify_original.R"))

print(str_glue("Archive: {dataset_doi}"))
