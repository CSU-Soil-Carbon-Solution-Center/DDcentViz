# R/manifest.R

#' Infer a file "role" based on name + extension
#'
#' @param path Character. File path.
#'
#' @return Character role (one of DDCENTVIZ_ROLES).
#' @keywords internal
infer_file_role <- function(path) {
  bn <- basename(path)
  ext <- tolower(tools::file_ext(bn))
  
  for (role in names(DDCENTVIZ_ROLE_PATTERNS)) {
    spec <- DDCENTVIZ_ROLE_PATTERNS[[role]]
    
    # ext match
    if (!is.null(spec$ext) && !(ext %in% tolower(spec$ext))) next
    
    # regex match (any)
    rx <- spec$regex %||% ".*"
    hit <- FALSE
    for (p in rx) {
      if (grepl(p, bn, ignore.case = TRUE)) {
        hit <- TRUE
        break
      }
    }
    if (hit) return(role)
  }
  
  "unknown"
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Validate a manifest tibble/data.frame
#'
#' @param manifest data.frame.
#' @param strict Logical. If TRUE, stop on issues.
#'
#' @return manifest invisibly (possibly unchanged). Emits warnings/errors.
#' @export
validate_manifest <- function(manifest, strict = FALSE) {
  req_cols <- c("site", "exp", "role", "path", "ext", "mtime", "size")
  miss <- setdiff(req_cols, names(manifest))
  if (length(miss) > 0) {
    msg <- paste0("Manifest missing required columns: ", paste(miss, collapse = ", "))
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  
  if ("path" %in% names(manifest)) {
    bad <- !file.exists(manifest$path)
    if (any(bad, na.rm = TRUE)) {
      msg <- paste0("Manifest contains non-existent paths (n=", sum(bad, na.rm = TRUE), ").")
      if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    }
  }
  
  invisible(manifest)
}

#' Build a file manifest for a site (and optionally experiment)
#'
#' The manifest indexes files under:
#' - site root (inputs like .sch/.100/.wth)
#' - outputs/{exp}/ (outputs)
#'
#' @param project Character. Project root path.
#' @param site Character. Site name.
#' @param exp Character or NULL. If NULL, includes all experiments found under outputs/.
#' @param include_inputs Logical. Include input-like files found in site dir (non-recursive).
#' @param include_outputs Logical. Include output files under outputs/ (recursive = FALSE by default).
#' @param recursive_outputs Logical. Recurse under outputs/{exp}/.
#'
#' @return data.frame with columns:
#'   site, exp, role, kind, path, basename, ext, size, mtime
#' @export
dc_manifest <- function(project,
                        site,
                        exp = NULL,
                        include_inputs = TRUE,
                        include_outputs = TRUE,
                        recursive_outputs = FALSE) {
  sp <- dc_site_path(project, site, must_work = TRUE)
  
  # Determine experiments
  exps <- if (is.null(exp)) dc_list_experiments(project, site, include_eq = TRUE) else exp
  if (length(exps) < 1) exps <- character(0)
  
  out_list <- list()
  
  # Inputs: from site dir (non-recursive)
  if (isTRUE(include_inputs)) {
    files_in <- list.files(sp, full.names = TRUE, recursive = FALSE)
    files_in <- files_in[file.exists(files_in)]
    files_in <- files_in[!dir.exists(files_in)]
    
    if (length(files_in) > 0) {
      df_in <- data.frame(
        site = site,
        exp  = NA_character_,
        kind = "input",
        path = files_in,
        basename = basename(files_in),
        ext  = tolower(tools::file_ext(files_in)),
        size = as.numeric(file.info(files_in)$size),
        mtime = as.POSIXct(file.info(files_in)$mtime),
        stringsAsFactors = FALSE
      )
      df_in$role <- vapply(df_in$path, infer_file_role, character(1))
      out_list[["inputs"]] <- df_in
    }
  }
  
  # Outputs: from outputs/{exp}/
  if (isTRUE(include_outputs)) {
    base_out <- file.path(sp, "outputs")
    if (dir.exists(base_out) && length(exps) > 0) {
      for (e in exps) {
        e_dir <- file.path(base_out, e)
        if (!dir.exists(e_dir)) next
        
        files_out <- list.files(e_dir, full.names = TRUE, recursive = recursive_outputs)
        files_out <- files_out[file.exists(files_out)]
        files_out <- files_out[!dir.exists(files_out)]
        
        if (length(files_out) < 1) next
        
        df_out <- data.frame(
          site = site,
          exp  = e,
          kind = "output",
          path = files_out,
          basename = basename(files_out),
          ext  = tolower(tools::file_ext(files_out)),
          size = as.numeric(file.info(files_out)$size),
          mtime = as.POSIXct(file.info(files_out)$mtime),
          stringsAsFactors = FALSE
        )
        df_out$role <- vapply(df_out$path, infer_file_role, character(1))
        out_list[[paste0("outputs_", e)]] <- df_out
      }
    }
  }
  
  if (length(out_list) == 0) {
    mf <- data.frame(
      site = character(0),
      exp = character(0),
      role = character(0),
      kind = character(0),
      path = character(0),
      basename = character(0),
      ext = character(0),
      size = numeric(0),
      mtime = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    )
    return(mf)
  }
  
  mf <- do.call(rbind, out_list)
  rownames(mf) <- NULL
  
  validate_manifest(mf, strict = FALSE)
  mf
}
