# R/paths_project.R

#' Normalize and validate a DayCent project root
#'
#' A DayCent project root is expected to contain a "sites/" directory.
#' If \code{path} is NULL, this will use \code{getwd()}.
#'
#' @param path Character. Project root path.
#' @param sites_dir Character. Directory name under project root (default "sites").
#' @param must_work Logical. If TRUE, error if path doesn't exist.
#'
#' @return Normalized project root path.
#' @export
dc_project_root <- function(path = NULL,
                            sites_dir = DDCENTVIZ_DEFAULT_SITES_DIR,
                            must_work = TRUE) {
  if (is.null(path) || !nzchar(path)) path <- getwd()
  
  if (must_work && !dir.exists(path)) {
    stop("Project root does not exist: ", path, call. = FALSE)
  }
  
  path <- normalizePath(path, winslash = "/", mustWork = must_work)
  
  sites_path <- file.path(path, sites_dir)
  if (must_work && !dir.exists(sites_path)) {
    stop("Expected sites directory not found: ", sites_path, call. = FALSE)
  }
  
  path
}

#' Return the path to a site directory
#'
#' @param project Character. Project root path.
#' @param site Character. Site name.
#' @param sites_dir Character. Directory name under project root.
#' @param must_work Logical. If TRUE, error if site dir doesn't exist.
#'
#' @return Normalized site path.
#' @export
dc_site_path <- function(project,
                         site,
                         sites_dir = DDCENTVIZ_DEFAULT_SITES_DIR,
                         must_work = TRUE) {
  project <- dc_project_root(project, sites_dir = sites_dir, must_work = must_work)
  
  if (is.null(site) || !nzchar(site)) {
    stop("`site` must be a non-empty character string.", call. = FALSE)
  }
  
  sp <- file.path(project, sites_dir, site)
  if (must_work && !dir.exists(sp)) {
    stop("Site directory not found: ", sp, call. = FALSE)
  }
  
  normalizePath(sp, winslash = "/", mustWork = must_work)
}

#' List sites under a project
#'
#' @param project Character. Project root path.
#' @param sites_dir Character. Directory name under project root.
#'
#' @return Character vector of site folder names.
#' @export
dc_list_sites <- function(project, sites_dir = DDCENTVIZ_DEFAULT_SITES_DIR) {
  project <- dc_project_root(project, sites_dir = sites_dir, must_work = TRUE)
  sites_path <- file.path(project, sites_dir)
  
  dirs <- list.dirs(sites_path, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[nzchar(dirs)]
  sort(dirs)
}
