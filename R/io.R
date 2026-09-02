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
  # An `id` column that came from the CSV as integer/numeric would
  # break vapply(character(1)) in rank_records aggregation. Coerce
  # unconditionally.
  df$id <- as.character(df$id)

  # Duplicate ids silently corrupt the ranking: rank_records() looks
  # up a record by `records[records$id == row$id, ][1, ]`, so both
  # rows get scored with the FIRST row's title/abstract. Catch it at
  # ingest with a message the user can act on -- merging multiple
  # database exports is the common cause.
  dupes <- df$id[duplicated(df$id)]
  if (length(dupes) > 0L) {
    show <- unique(dupes)
    example <- if (length(show) > 5L) {
      c(show[1:5], sprintf("...and %d more", length(show) - 5L))
    } else show
    cli::cli_abort(c(
      "Duplicate record ids in the input ({length(unique(dupes))} distinct value{?s}).",
      "i" = "Duplicated: {.val {example}}",
      "i" = paste0(
        "screenllm keys the cache by id, so duplicates would score ",
        "both rows with the first row's abstract. Either drop the ",
        "duplicates, deduplicate with `find_duplicates()`, or pass ",
        "`id_column = NULL` to let screenllm assign fresh ids."
      )
    ))
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
#' Read a text file, trying several encodings in turn.
#'
#' EndNote / Web of Science / Zotero RIS and CSV exports on Windows
#' are frequently Windows-1252 or Latin-1, not UTF-8. Hardcoding
#' encoding = "UTF-8" caused readLines to mark strings as UTF-8 even
#' when bytes were invalid, and downstream grepl/regexpr threw
#' "input string N is invalid in this locale". Try encodings in
#' descending likelihood and return the first that decodes cleanly.
#'
#' @param path File path.
#' @return Character vector of lines.
#' @keywords internal
read_lines_any_encoding <- function(path) {
  raw_bytes <- readBin(path, "raw", file.info(path)$size)
  # Strip UTF-8 BOM if present -- Zotero on Windows emits one, and
  # it would otherwise leave a 3-byte prefix on the first line that
  # breaks the "^TY  - JOUR" record-start match.
  if (length(raw_bytes) >= 3L &&
        identical(as.integer(raw_bytes[1:3]), c(0xEFL, 0xBBL, 0xBFL))) {
    raw_bytes <- raw_bytes[-(1:3)]
  }
  # Prefer readr::guess_encoding (statistical, fast); otherwise try
  # each candidate in descending real-world frequency.
  encodings <- if (rlang::is_installed("readr")) {
    guessed <- tryCatch(
      readr::guess_encoding(raw_bytes, n_max = 10000, threshold = 0.2),
      error = function(e) NULL
    )
    guesses <- if (!is.null(guessed) && nrow(guessed) > 0L) {
      as.character(guessed$encoding)
    } else character()
    unique(c(guesses, "UTF-8", "Windows-1252", "latin1"))
  } else {
    c("UTF-8", "Windows-1252", "latin1")
  }
  # `rawToChar` interprets bytes as a character string without decoding
  # -- fine as long as there are no embedded nulls (raw text files
  # rarely have them). iconv then decodes from the candidate encoding.
  raw_str <- suppressWarnings(rawToChar(raw_bytes))
  for (enc in encodings) {
    txt <- suppressWarnings(iconv(raw_str, from = enc, to = "UTF-8",
                                  sub = NA))
    if (!is.na(txt)) {
      return(strsplit(txt, "\r\n|\r|\n", perl = TRUE)[[1L]])
    }
  }
  # Last resort: force UTF-8 with lossy substitution so users get a
  # reduced-but-readable file rather than a hard error.
  fallback <- iconv(raw_str, from = "UTF-8", to = "UTF-8", sub = "?")
  strsplit(fallback, "\r\n|\r|\n", perl = TRUE)[[1L]]
}

#' @keywords internal
read_ris_records <- function(path) {
  lines <- read_lines_any_encoding(path)
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
#' @param fuzzy Whether to fuzzy-match titles (Jaro-Winkler
#'   similarity of at least 0.95). Requires the `stringdist`
#'   package; falls back to exact normalised-title match if
#'   `stringdist` is not installed.
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
    # Guard NA at every use: normalise_title(NA) is NA_character_,
    # nzchar(NA) is NA, and exists(NA, envir) throws "invalid first
    # argument". Any single record with a missing title used to crash
    # the whole Corpus tab dedup step.
    if (!is.na(dkey) && nzchar(dkey) && exists(dkey, envir = seen_dois)) {
      duplicate_of[i] <- get(dkey, envir = seen_dois)
      next
    }
    if (!is.na(tkey) && nzchar(tkey) && exists(tkey, envir = seen_titles)) {
      duplicate_of[i] <- get(tkey, envir = seen_titles)
      next
    }
    if (isTRUE(fuzzy) && rlang::is_installed("stringdist") &&
          !is.na(tkey) && nzchar(tkey)) {
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
    if (!is.na(tkey) && nzchar(tkey)) assign(tkey, ids[i], envir = seen_titles)
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
  # Excel-on-Windows exports CSVs as Windows-1252 (unless the user
  # explicitly picks "CSV UTF-8"), and Zotero can emit UTF-8-with-BOM.
  # Read raw first, detect encoding, then hand a properly-locale'd
  # readr call the exact path. Falls back to a scrubbed temp file for
  # utils::read.csv when readr isn't installed.
  raw_bytes <- tryCatch(
    readBin(path, "raw", file.info(path)$size),
    error = function(e) NULL
  )
  if (is.null(raw_bytes)) {
    # Something's wrong at the FS layer; let the downstream reader
    # produce its own error.
    return(read_csv_fallback(path, sep))
  }
  # Strip UTF-8 BOM if present.
  had_bom <- length(raw_bytes) >= 3L &&
    identical(as.integer(raw_bytes[1:3]),
              c(0xEFL, 0xBBL, 0xBFL))
  if (had_bom) raw_bytes <- raw_bytes[-(1:3)]

  encoding <- detect_encoding(raw_bytes)

  if (rlang::is_installed("readr")) {
    # Write scrubbed (BOM-free) bytes to a temp file so readr sees
    # them without the BOM. Cheap: one write per import.
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(raw_bytes, tmp)
    readr::read_delim(
      tmp, delim = sep, show_col_types = FALSE,
      progress = FALSE, name_repair = "unique",
      locale = readr::locale(encoding = encoding)
    )
  } else {
    read_csv_fallback(path, sep, encoding = encoding, raw_bytes = raw_bytes)
  }
}

#' @keywords internal
read_csv_fallback <- function(path, sep, encoding = "UTF-8",
                              raw_bytes = NULL) {
  # If we already scrubbed the BOM, write bytes to a temp path so
  # read.csv doesn't see it. Otherwise read directly.
  target <- path
  clean_up <- character()
  on.exit(unlink(clean_up), add = TRUE)
  if (!is.null(raw_bytes)) {
    target <- tempfile(fileext = ".csv")
    clean_up <- c(clean_up, target)
    writeBin(raw_bytes, target)
  }
  tibble::as_tibble(utils::read.csv(
    target, sep = sep, stringsAsFactors = FALSE,
    fileEncoding = encoding
  ))
}

#' @keywords internal
detect_encoding <- function(raw_bytes) {
  if (rlang::is_installed("readr")) {
    guessed <- tryCatch(
      readr::guess_encoding(raw_bytes, n_max = 10000, threshold = 0.2),
      error = function(e) NULL
    )
    if (!is.null(guessed) && nrow(guessed) > 0L) {
      # Prefer UTF-8 if it's plausible (avoids gratuitous
      # ASCII-vs-UTF-8 detection swings on small inputs).
      encs <- as.character(guessed$encoding)
      if ("UTF-8" %in% encs) return("UTF-8")
      return(encs[1L])
    }
  }
  # No guess. Assume UTF-8; the reader will produce mojibake on
  # Windows-1252 input but at least won't hard-fail.
  "UTF-8"
}

#' @keywords internal
normalise_column_names <- function(df) {
  # Aliases for the column names the app needs. Every real-world
  # export path we can plausibly hit is listed here so users don't
  # get "Input is missing required column" when their file actually
  # has the data under a slightly different label.
  aliases <- list(
    title = c("Title", "TI", "Item Title", "Manuscript Title",
              "Publication Title", "Article Title"),
    abstract = c("Abstract", "AB", "Abstract Note", "Summary",
                 "Notes"),
    id = c("ID", "record_id", "RefID", "Reference ID", "EID",
           "Key", "Item ID"),
    year = c("Year", "PY", "Publication Year", "Publication Date"),
    doi = c("DOI"),
    journal = c("Journal", "Source title", "SO",
                "Publication Title", "Journal Title", "Container Title")
  )
  for (canon in names(aliases)) {
    hits <- intersect(names(df), aliases[[canon]])
    if (length(hits) == 0L) next
    if (canon %in% names(df)) {
      # A canonical column already exists. If it's entirely
      # blank/NA, promote a populated case-variant/alias in its
      # place; otherwise leave both alone. This prevents the
      # "blank `title` beats populated `Title`" case that shows
      # up when two pipelines merge frames with the same field
      # under different capitalisations.
      canon_col <- df[[canon]]
      canon_blank <- all(is.na(canon_col) | !nzchar(as.character(canon_col)))
      if (canon_blank) {
        winner <- hits[[1L]]
        df[[canon]] <- df[[winner]]
        df[[winner]] <- NULL
      }
      next
    }
    winner <- hits[[1L]]
    names(df)[which(names(df) == winner)] <- canon
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
  low <- tolower(x)
  out <- rep(NA_character_, length(x))
  out[grepl("^accept", low)] <- "Accept"
  out[grepl("^reject", low)] <- "Reject"
  out[grepl("^exclud", low)] <- "Reject"
  out[grepl("^includ", low)] <- "Accept"
  # English + common European locale variants for TRUE/FALSE.
  # Excel writes booleans in the user's UI language, so a
  # French/German/Spanish/Italian reviewer's decisions.csv routinely
  # rides through here with "VRAI", "WAHR", "SI", or "VERO" instead
  # of "TRUE". Coerce them all. Also handle numeric decisions
  # written by Excel with a trailing ".0" (1.0 / 0.0).
  accept_lits <- tolower(c(
    "Y", "1", "1.0", "TRUE",
    "Yes", "Ja", "Oui", "Si", "Sim", "Da",   # en/de/fr/es-it/pt/ru
    "VRAI",   # fr TRUE
    "WAHR",   # de TRUE
    "VERO",   # it TRUE
    "VERDADERO", # es TRUE
    "SANT",   # sv TRUE
    "TRUE."
  ))
  reject_lits <- tolower(c(
    "N", "0", "0.0", "FALSE",
    "No", "Nein", "Non", "Nao", "Nej", "Njet",
    "FAUX",   # fr FALSE
    "FALSCH", # de FALSE
    "FALSO",  # it/es/pt FALSE
    "FALSKT", # sv FALSE
    "FALSE."
  ))
  out[low %in% accept_lits] <- "Accept"
  out[low %in% reject_lits] <- "Reject"
  out
}
