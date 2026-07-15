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
    ris = read_ris_records(path),
    txt = read_ris_records(path),  # some exporters name RIS as .txt
    bib = {
      cli::cli_abort(
        "BibTeX ingest is not supported. Convert to CSV or RIS first."
      )
    },
    cli::cli_abort("Unsupported file extension: {.val {ext}}")
  )
}

# Minimal RIS parser. Handles the tags Zotero, EndNote, Web of Science,
# and Mendeley all emit: TI (title), T1 (title alt), AB (abstract),
# N2 (abstract alt), DO (DOI), PY / Y1 (year), JO / T2 / JF (journal),
# AU (authors), UR (URL), and ER (end of record). Silently ignores the
# rest. Multi-line values are supported by continuation of the same
# tag.
#' @keywords internal
read_ris_records <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # Records begin with TY - and end with ER -.
  records <- list()
  current <- list()
  cur_tag <- NULL
  flush <- function(cur, out) {
    if (length(cur) > 0L) {
      out[[length(out) + 1L]] <- cur
    }
    out
  }
  for (line in lines) {
    if (!nzchar(line) || grepl("^\\s*$", line)) next
    m <- regmatches(line, regexpr("^[A-Z][A-Z0-9]\\s+-\\s?", line))
    if (length(m) == 1L) {
      tag <- substr(m, 1, 2)
      value <- substr(line, nchar(m) + 1L, nchar(line))
      cur_tag <- tag
      if (identical(tag, "ER")) {
        records <- flush(current, records)
        current <- list()
        cur_tag <- NULL
        next
      }
      # Append to any existing value under the same tag (multi-line).
      old <- current[[tag]]
      current[[tag]] <- if (is.null(old)) value else paste(old, value)
    } else if (!is.null(cur_tag)) {
      # Continuation line for the previous tag.
      current[[cur_tag]] <- paste(current[[cur_tag]], trimws(line))
    }
  }
  # In case the final record is missing an ER tag.
  records <- flush(current, records)

  if (length(records) == 0L) {
    cli::cli_abort("No RIS records found in {.path {path}}.")
  }

  pick <- function(rec, tags) {
    for (t in tags) if (!is.null(rec[[t]])) return(rec[[t]])
    NA_character_
  }
  tibble::tibble(
    title    = vapply(records, pick, character(1),
                      tags = c("TI", "T1", "CT")),
    abstract = vapply(records, pick, character(1),
                      tags = c("AB", "N2")),
    year     = vapply(records, pick, character(1),
                      tags = c("PY", "Y1", "DA")),
    doi      = vapply(records, pick, character(1),
                      tags = c("DO", "DI")),
    journal  = vapply(records, pick, character(1),
                      tags = c("JO", "T2", "JF", "JA")),
    url      = vapply(records, pick, character(1),
                      tags = c("UR", "L1"))
  )
}

#' Detect and remove duplicate records
#'
#' Two records are considered duplicates if they share a non-missing DOI
#' (case-insensitive) or if their normalised titles match. Title
#' normalisation drops punctuation, lowercases, collapses whitespace, and
#' (optionally) fuzzy-matches with `stringdist` if that package is
#' available. Returns the input tibble with an added `duplicate_of`
#' column: `NA` for unique records, otherwise the id of the earlier
#' record that duplicates it.
#'
#' Multi-database searches (Scopus, Web of Science, Google Scholar) often
#' produce 10-20 percent duplicates; running this on the fresh corpus
#' before ranking avoids scoring the same abstract three or four times.
#'
#' @param records A tibble of records from `read_records()`.
#' @param fuzzy Whether to fuzzy-match titles (Jaro-Winkler similarity
#'   >= 0.95). Requires the `stringdist` package; falls back to exact
#'   normalised-title match if `stringdist` is not installed.
#' @return The input tibble with a new `duplicate_of` column.
#' @export
#' @examples
#' recs <- data.frame(
#'   id = c("a", "b", "c"),
#'   title = c("Coral reefs", "Coral Reefs.", "Deep sea"),
#'   abstract = c("x", "x", "y")
#' )
#' find_duplicates(recs)
find_duplicates <- function(records, fuzzy = TRUE) {
  stopifnot(is.data.frame(records), "title" %in% names(records))
  n <- nrow(records)
  ids <- if ("id" %in% names(records)) records$id else
    sprintf("record_%05d", seq_len(n))
  norm_title <- normalise_title(records$title)
  doi <- if ("doi" %in% names(records))
    normalise_doi(records$doi) else rep(NA_character_, n)

  duplicate_of <- rep(NA_character_, n)
  seen_titles <- new.env(parent = emptyenv())
  seen_dois <- new.env(parent = emptyenv())
  for (i in seq_len(n)) {
    tkey <- norm_title[i]
    dkey <- doi[i]
    if (!is.na(dkey) && nzchar(dkey) && exists(dkey, envir = seen_dois)) {
      duplicate_of[i] <- get(dkey, envir = seen_dois)
      next
    }
    if (nzchar(tkey) && exists(tkey, envir = seen_titles)) {
      duplicate_of[i] <- get(tkey, envir = seen_titles)
      next
    }
    if (isTRUE(fuzzy) && rlang::is_installed("stringdist") && nzchar(tkey)) {
      prior <- ls(seen_titles)
      if (length(prior) > 0L) {
        sims <- 1 - stringdist::stringdist(tkey, prior, method = "jw")
        best <- which.max(sims)
        if (sims[best] >= 0.95) {
          duplicate_of[i] <- get(prior[best], envir = seen_titles)
          next
        }
      }
    }
    if (nzchar(tkey)) assign(tkey, ids[i], envir = seen_titles)
    if (!is.na(dkey) && nzchar(dkey)) assign(dkey, ids[i], envir = seen_dois)
  }
  records$duplicate_of <- duplicate_of
  records
}

#' @keywords internal
normalise_title <- function(x) {
  s <- tolower(as.character(x))
  s <- gsub("[^a-z0-9 ]", " ", s)
  s <- gsub("\\s+", " ", s)
  trimws(s)
}

#' @keywords internal
normalise_doi <- function(x) {
  s <- tolower(trimws(as.character(x)))
  s <- sub("^https?://(dx\\.)?doi\\.org/", "", s)
  s <- sub("^doi:\\s*", "", s)
  s[is.na(s) | !nzchar(s)] <- NA_character_
  s
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

#' Coerce a decisions data.frame to the schema the rest of the
#' package expects: `id`, `human_decision`, `note`, `timestamp`.
#'
#' Any column missing from the input is added as a character NA
#' vector. Extra columns are preserved. `NULL` or a zero-row input
#' returns a properly-shaped empty tibble; the caller can `rbind` /
#' `bind_rows` new rows against it without a column-mismatch error.
#'
#' Used at load time (mod_setup rehydration) and at write time
#' (Screen tab record_decision) so a legacy 1- or 2-column decisions
#' file on disk gets sanitised on its way through the app rather
#' than propagating broken shape into downstream tabs.
#'
#' @param d A data.frame or NULL.
#' @return A data.frame with at least the four canonical columns.
#' @keywords internal
normalise_decisions_shape <- function(d) {
  required <- c("id", "human_decision", "note", "timestamp")
  if (is.null(d)) {
    return(data.frame(
      id = character(), human_decision = character(),
      note = character(), timestamp = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(d)) return(normalise_decisions_shape(NULL))
  for (col in required) {
    if (!(col %in% names(d))) {
      d[[col]] <- rep(NA_character_, nrow(d))
    } else {
      d[[col]] <- as.character(d[[col]])
    }
  }
  d
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
