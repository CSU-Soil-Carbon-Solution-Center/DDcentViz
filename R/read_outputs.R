# R/read_outputs.R


#' Drop unnamed empty columns and make names unique
#'
#' Drops columns that are BOTH unnamed (NA or "") and entirely NA.
#' Then makes remaining names unique.
#'
#' @param df data.frame
#' @return cleaned data.frame
#' @keywords internal
dc_clean_columns <- function(df) {
  nm <- names(df)

  unnamed <- is.na(nm) | nm == ""
  all_na  <- vapply(df, function(x) all(is.na(x)), logical(1))

  drop <- unnamed & all_na
  if (any(drop)) {
    df <- df[, !drop, drop = FALSE]
    nm <- names(df)
  }

  names(df) <- make.unique(nm, sep = "_")
  df
}


#' Read a DayCent output file into a data.frame
#'
#' Supports:
#' - .csv via read.csv
#' - .out/.txt/.dat via whitespace-delimited read.table
#'
#' @param path Character. File path.
#' @param role Optional character role to tweak parsing.
#' @param header Logical. Whether to treat first row as header (for whitespace files).
#' @param stringsAsFactors Logical.
#' @param ...
#'
#' @return data.frame
#' @export
dc_read_output <- function(path,
                           role = NULL,
                           header = TRUE,
                           stringsAsFactors = FALSE,
                           ...) {

  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    df <- read.csv(path,
                   stringsAsFactors = stringsAsFactors,
                   check.names = FALSE,
                   ...)

    df <- dc_clean_columns(df)

    # Optional: drop common rownames column from write.csv(row.names=TRUE)
    if ("X" %in% names(df)) {
      xnum <- suppressWarnings(as.numeric(df$X))
      if (all(!is.na(xnum))) df$X <- NULL
    }

    return(df)
  }

  if (ext %in% c("out", "txt", "dat")) {

    df <- tryCatch(
      read.table(path,
                 header = header,
                 sep = "",
                 stringsAsFactors = stringsAsFactors,
                 check.names = FALSE,
                 ...),
      error = function(e) NULL
    )

    if (!is.null(df)) {
      df <- dc_clean_columns(df)
      return(df)
    }

    # fallback: no header
    df2 <- read.table(path,
                      header = FALSE,
                      sep = "",
                      stringsAsFactors = stringsAsFactors,
                      check.names = FALSE,
                      ...)

    names(df2) <- paste0("V", seq_len(ncol(df2)))
    df2 <- dc_clean_columns(df2)
    return(df2)
  }

  # default attempt
  df <- read.table(path,
                   header = header,
                   sep = "",
                   stringsAsFactors = stringsAsFactors,
                   check.names = FALSE,
                   ...)
  df <- dc_clean_columns(df)
  df
}

