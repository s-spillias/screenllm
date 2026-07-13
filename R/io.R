#' Read a corpus of records from disk
#'
#' Accepts a data frame directly, or a path to a CSV, TSV, RIS, BibTeX, or
#' Excel file. Returns a tibble with the columns `screenllm` expects:
#' `id`, `title`, `abstract`, plus any additional bibliographic columns
#' the input carries.
#'
#' @param source Either a data frame with (at minimum) columns
#'   `title` and `abstract`, or a file path.
#' @param id_column Optional name of an existing column to use as the record
#'   id. If `NULL`, an `id` column is generated as `record_<zero-padded index>`.
#' @return A tibble with columns `id`, `title`, `abstract`, plus any extras.
#' @export
read_records <- function(source, id_column = NULL) {
  df <- if (is.data.frame(source)) {
    tibble::as_tibble(source)
  } else if (is.character(source) && length(source) == 1L) {
    read_records_from_path(source)
  } else {
    cli::cli_abort("`source` must be a data frame or a single file path.")
  }

  df <- normalise_column_names(df)
  required <- c("title", "abstract")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    cli::cli_abort("Input is missing required column{?s}: {.val {missing}}")
  }

  if (is.null(id_column) || !(id_column %in% names(df))) {
    if (!("id" %in% names(df))) {
      df$id <- sprintf("record_%05d", seq_len(nrow(df)))
    }
  } else {
    df$id <- as.character(df[[id_column]])
  }

  # Reorder so id, title, abstract come first.
  df <- dplyr::relocate(df, dplyr::all_of(c("id", "title", "abstract")))
  df$title <- as.character(df$title)
  df$abstract <- ifelse(
    is.na(df$abstract), "", as.character(df$abstract)
  )
  df
}

#' @keywords internal
read_records_from_path <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  ext <- tolower(fs::path_ext(path))
  switch(
    ext,
    csv = read_csv_records(path),
    tsv = read_csv_records(path, sep = "\t"),
    xlsx = ,
    xls = {
      rlang::check_installed("readxl", "to read Excel files.")
      tibble::as_tibble(readxl::read_excel(path))
    },
    ris = {
      rlang::check_installed("bibtex", "to read RIS/BibTeX files.")
      cli::cli_abort("RIS ingest not yet implemented in v0.1; supply CSV/XLSX for now.")
    },
    bib = {
      rlang::check_installed("bibtex", "to read RIS/BibTeX files.")
      cli::cli_abort("BibTeX ingest not yet implemented in v0.1; supply CSV/XLSX for now.")
    },
    cli::cli_abort("Unsupported file extension: {.val {ext}}")
  )
}

#' @keywords internal
read_csv_records <- function(path, sep = ",") {
  if (rlang::is_installed("readr")) {
    readr::read_delim(
      path, delim = sep, show_col_types = FALSE,
      progress = FALSE, name_repair = "unique"
    )
  } else {
    tibble::as_tibble(
      utils::read.csv(path, sep = sep, stringsAsFactors = FALSE)
    )
  }
}

#' @keywords internal
normalise_column_names <- function(df) {
  aliases <- list(
    title = c("Title", "TI"),
    abstract = c("Abstract", "AB"),
    id = c("ID", "record_id", "RefID", "Reference ID", "EID"),
    year = c("Year", "PY"),
    doi = c("DOI"),
    journal = c("Journal", "Source title", "SO")
  )
  for (canon in names(aliases)) {
    hits <- intersect(names(df), aliases[[canon]])
    if (canon %in% names(df)) next
    if (length(hits) > 0L) {
      names(df)[which(names(df) == hits[1L])] <- canon
    }
  }
  df
}

#' Read completed screening decisions
#'
#' Reads a worksheet the reviewer has completed offline (an Excel file
#' produced by `export_worksheet()`) and returns a tibble of decisions.
#'
#' @param path Path to the completed worksheet.
#' @return A tibble with columns `id`, `human_decision`, `note` (if present).
#' @export
read_decisions <- function(path) {
  rlang::check_installed("readxl", "to read the decisions worksheet.")
  df <- tibble::as_tibble(readxl::read_excel(path))
  if (!("human_decision" %in% names(df))) {
    cli::cli_abort("Worksheet does not have a {.field human_decision} column.")
  }
  df$human_decision <- normalise_decisions(df$human_decision)
  df
}

#' @keywords internal
normalise_decisions <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  out[grepl("^accept", tolower(x))] <- "Accept"
  out[grepl("^reject", tolower(x))] <- "Reject"
  out[grepl("^exclud", tolower(x))] <- "Reject"
  out[grepl("^includ", tolower(x))] <- "Accept"
  out[x %in% c("Y", "y", "1", "TRUE")] <- "Accept"
  out[x %in% c("N", "n", "0", "FALSE")] <- "Reject"
  out
}
