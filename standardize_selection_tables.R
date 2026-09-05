# Standardize selection tables
#
# Input:  tab-delimited .txt files in input_dir
# Output: tab-delimited .txt files in output_dir
#
# Existing columns are preserved. The existing NLP column is replaced with
# the number of nonlinear-phenomena codes found in the raw NLP value.
# BP, DC, FJ, SH, and SB are populated as 1/0 indicators.
# E-Clicks in frame is added as a blank column for manual completion.

library(tidyverse)

# ---- User settings ---------------------------------------------------------
input_dir  <- "raw_tables"          # folder containing raw .txt files
output_dir <- "standardized_tables" # folder for standardized files

# Set to TRUE if you want the output columns arranged like the reference file.
# Existing values are not changed; only column order is changed.
match_reference_order <- TRUE

# The requested code columns, in the desired order.
phenomena_codes <- c("BP", "DC", "FJ", "SH", "SB")

# ---- Helper functions ------------------------------------------------------

read_selection_table <- function(path) {
  readr::read_tsv(
    file = path,
    na = c("", "NA"),
    quote = "",
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    name_repair = "minimal",
    trim_ws = FALSE
  )
}

standardize_selection_table <- function(dat) {
  if (!"NLP" %in% names(dat)) {
    stop("The table does not contain an 'NLP' column.")
  }

  raw_nlp <- dat$NLP %>%
    tidyr::replace_na("") %>%
    stringr::str_to_upper()

  # A code counts only when it appears as a two-letter code in the NLP field.
  # Word boundaries prevent accidental matches inside unrelated text.
  code_flags <- purrr::map_dfc(
    phenomena_codes,
    ~ tibble::tibble(
      !!.x := as.integer(stringr::str_detect(
        raw_nlp,
        stringr::regex(paste0("\\b", .x, "\\b"), ignore_case = TRUE)
      ))
    )
  )

  # NLP is the total number of distinct nonlinear-phenomena types.
  nlp_total <- rowSums(code_flags)

  # Remove the old NLP and any pre-existing generated columns so rerunning the
  # script does not duplicate columns or retain stale values.
  dat <- dat %>%
    dplyr::select(
      -dplyr::any_of(c("NLP", phenomena_codes, "E-Clicks in frame", "E-Clicks in Frame"))
    )

  # Add the derived columns. E-Clicks is intentionally blank for every row.
  dat <- dat %>%
    dplyr::mutate(
      NLP = nlp_total,
      !!!code_flags,
      `E-Clicks in frame` = NA_character_
    )

  if (match_reference_order) {
    reference_order <- c(
      "Selection", "View", "Channel", "Begin Time (s)", "End Time (s)",
      "Low Freq (Hz)", "High Freq (Hz)", "Inband Power (dB FS)",
      "Delta Time (s)", "Peak Freq (Hz)", "SNR", "SQ", "NLP",
      phenomena_codes, "E-Clicks in frame", "Notes"
    )

    # Put known reference columns first, then retain any other columns at the
    # end so no unexpected source columns are discarded.
    dat <- dat %>%
      dplyr::select(
        dplyr::any_of(reference_order),
        dplyr::everything()
      )
  }

  dat
}

# ---- Process every raw table ------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

input_files <- list.files(
  path = input_dir,
  pattern = "\\.txt$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(input_files) == 0) {
  stop("No .txt files were found in input_dir: ", normalizePath(input_dir, mustWork = FALSE))
}

purrr::walk(input_files, function(path) {
  standardized <- read_selection_table(path) %>%
    standardize_selection_table()

  output_path <- file.path(output_dir, basename(path))

  readr::write_tsv(
    standardized,
    file = output_path,
    na = "",
    quote = "none",
    append = FALSE
  )

  message("Wrote: ", output_path)
})
