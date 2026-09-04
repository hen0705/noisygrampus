# Standardize Raven Selection Tables
#
# This script reads Raven selection tables exported as tab-delimited text and
# writes standardized tab-delimited tables. Columns not named below are kept
# unchanged and in their original order.
#
# Rules:
#   * NLP is the number of recognized NLP types in the cell. Examples:
#       "Yes - BP"       -> 1
#       "NLP - BP, DC"   -> 2
#       "SH"             -> 0
#       "No" or blank    -> 0
#   * Notes is 1 when a nonblank note is present and 0 otherwise. If Notes is
#     missing, it is added.
#   * SNR is preserved, including existing blanks.
#   * SQ is preserved, including existing blanks.
#   * Echolocation clicks in frame is added if missing and left blank.
#
# Usage from Positron/RStudio:
#   source("standardize_raven_selection_tables.R")
#   standardize_raven_tables("raw_tables", "standardized_tables")
#
# Or from a terminal:
#   Rscript standardize_raven_selection_tables.R raw_tables standardized_tables

standardize_raven_table <- function(input_file, output_file,
                                     nlp_codes = c("SH", "BP", "DC", "SB")) {
  if (!file.exists(input_file)) {
    stop("Input file does not exist: ", input_file)
  }

  # check.names = FALSE preserves Raven's original column names.
  tbl <- utils::read.delim(
    input_file,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    na.strings = character(0),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE
  )

  if (!"NLP" %in% names(tbl)) {
    stop("The table is missing the required NLP column: ", input_file)
  }

  # Count complete NLP codes only. This prevents words such as "No" from
  # being treated as detections and keeps the rule explicit/reproducible.
  count_nlp <- function(x) {
    x <- trimws(as.character(x))
    vapply(x, function(one_cell) {
      if (is.na(one_cell) || !nzchar(one_cell)) return(0L)
      # Perl-compatible boundaries ensure BP is counted but the BP inside a
      # longer word is not.  gregexpr() is part of base R.
      code_pattern <- paste0("(?<![A-Z])(", paste(nlp_codes, collapse = "|"), ")(?![A-Z])")
      hits <- gregexpr(code_pattern, toupper(one_cell), perl = TRUE)[[1]]
      as.integer(if (identical(hits, -1L)) 0L else length(hits))
    }, integer(1))
  }

  tbl[["NLP"]] <- count_nlp(tbl[["NLP"]])

  if (!"Notes" %in% names(tbl)) {
    tbl[["Notes"]] <- 0L
  } else {
    note_text <- trimws(as.character(tbl[["Notes"]]))
    tbl[["Notes"]] <- as.integer(!is.na(note_text) & nzchar(note_text))
  }

  clicks_name <- "Echolocation clicks in frame"
  if (!clicks_name %in% names(tbl)) {
    tbl[[clicks_name]] <- rep("", nrow(tbl))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    tbl,
    file = output_file,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    na = ""
  )
  invisible(output_file)
}

standardize_raven_tables <- function(input_dir, output_dir,
                                     pattern = "\\.(txt|tsv)$",
                                     nlp_codes = c("SH", "BP", "DC", "SB")) {
  if (!dir.exists(input_dir)) {
    stop("Input directory does not exist: ", input_dir)
  }
  files <- list.files(input_dir, pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)
  if (!length(files)) stop("No .txt or .tsv files found in: ", input_dir)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- file.path(output_dir, basename(files))
  for (i in seq_along(files)) {
    standardize_raven_table(files[[i]], outputs[[i]], nlp_codes = nlp_codes)
  }
  message("Standardized ", length(files), " table(s) in: ", output_dir)
  invisible(outputs)
}

# Optional command-line interface for reproducible batch processing.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0L) {
  if (length(args) != 2L) {
    stop("Usage: Rscript standardize_raven_selection_tables.R INPUT_DIR OUTPUT_DIR")
  }
  standardize_raven_tables(args[[1]], args[[2]])
}
