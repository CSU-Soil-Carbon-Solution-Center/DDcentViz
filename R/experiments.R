# R/experiments.R

#' List experiments for a site
#'
#' Experiments are inferred primarily from subdirectories under:
#' \code{project/sites/{site}/outputs/}
#'
#' @param project Character. Project root path.
#' @param site Character. Site name.
#' @param include_eq Logical. Include "eq" if present.
#'
#' @return Character vector of experiment names.
#' @export
dc_list_experiments <- function(project, site, include_eq = TRUE) {
  sp <- dc_site_path(project, site, must_work = TRUE)
  op <- file.path(sp, "outputs")
  
  if (!dir.exists(op)) return(character(0))
  
  exps <- list.dirs(op, full.names = FALSE, recursive = FALSE)
  exps <- exps[!(exps %in% DDCENTVIZ_RESERVED_EXPS)]
  exps <- exps[nzchar(exps)]
  
  if (!include_eq) exps <- setdiff(exps, "eq")
  
  sort(exps)
}
