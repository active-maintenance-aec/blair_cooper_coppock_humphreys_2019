# blair_cooper_coppock_humphreys_2019/maintained/table_2_tool_census.R
# Output: output/table_2_tool_census.csv, output/table_2_tool_census_tools.csv,
#         output/table_2_tool_census_counts.csv
# Depends on: helpers.R, original/bcch-ddrd-replication-archive.zip
# Description: Table 2 of the article, the census of existing design tools,
#   rebuilt from the spreadsheet in the deposit. The deposit ships no script for
#   this table, so the counts are recomputed here from the per-tool coding sheet.

source(here::here("maintained", "helpers.R"))

# download_original.R unpacks the deposited zip into original_extracted/ and
# verify_original.R checks every member against its manifest, so the sheet is
# read from there. Nothing in maintained/ writes into either directory.
census <- readxl::read_excel(
  here::here("original_extracted", "replication_archive", "tool_census", "tool_census.xlsx"),
  sheet = "Included",
  col_types = "text"
)

# The sheet's last three rows are a blank separator and two summary rows.
tools <- census |>
  filter(!is.na(Tool), !Tool %in% c("Average:", "Sum:"))

# Blanks in this sheet are written both as empty cells and as the literal text
# "NA", so both count as "the tool cannot do this".
is_coded <- function(x) !is.na(x) & x != "NA"

# Panel (a) counts the tools whose coding column is 1. Panel (b) counts the
# tools for which a power figure was obtained at all, which is what the columns
# recording each tool's power estimate carry.
declare_counts <- tribble(
  ~panel, ~row_label, ~column,
  "(a) Declare design elements", "Effect and block size correlated", "M: effect sizes correlated with block sizes",
  "(a) Declare design elements", "Estimand", "I: Estimand",
  "(a) Declare design elements", "Sampling procedure", "R: Sampling strategy",
  "(a) Declare design elements", "Assignment procedure", "R: Exact randomization procedure",
  "(a) Declare design elements", "Block sizes vary", "M: Heterogeneous block sizes",
  "(a) Declare design elements", "Probability weighting", "A: Inverse-probability weights"
) |>
  mutate(n_tools_able = map_int(column, \(col) sum(tools[[col]] == "1", na.rm = TRUE)))

# The published panel (b) has a sixth row, "SD of estimates (any estimator)".
# The coding sheet has no column for it, so it is carried here with no count
# rather than dropped, and the ground truth records that the deposit cannot
# support that cell.
diagnose_counts <- tribble(
  ~panel, ~row_label, ~column,
  "(b) Diagnosis capabilities", "Power (DIM estimator)", "D: Power of DIM",
  "(b) Diagnosis capabilities", "Power (BFE estimator)", "D: Power of BFE",
  "(b) Diagnosis capabilities", "Power (IPW-BFE estimator)", "D: Power of IPW-BFE",
  "(b) Diagnosis capabilities", "Bias (any estimator)", "D: Bias",
  "(b) Diagnosis capabilities", "Coverage (any estimator)", "D: Coverage",
  "(b) Diagnosis capabilities", "SD of estimates (any estimator)", NA_character_
) |>
  mutate(
    n_tools_able = map_int(
      column,
      \(col) if (is.na(col)) NA_integer_ else sum(is_coded(tools[[col]]))
    )
  )

tab <- bind_rows(declare_counts, diagnose_counts) |>
  mutate(n_tools = nrow(tools)) |>
  select(panel, row_label, column, n_tools_able, n_tools)

write_csv(tab, out_path("table_2_tool_census.csv"))

write_csv(tools |> select(-Comments), out_path("table_2_tool_census_tools.csv"))

# The two counts the appendix states in prose: the tools the search identified
# and the tools admitted to the survey. The second sheet of the same workbook
# lists the tools that were considered and excluded.
excluded <- readxl::read_excel(
  here::here("original_extracted", "replication_archive", "tool_census", "tool_census.xlsx"),
  sheet = "Excluded",
  col_types = "text"
)

counts <- tibble(
  quantity = c("n_candidates", "n_admitted"),
  n = c(nrow(tools) + sum(!is.na(excluded$Tool)), nrow(tools))
)

write_csv(counts, out_path("table_2_tool_census_counts.csv"))

print(counts)

print(tab, n = nrow(tab), width = 200)
