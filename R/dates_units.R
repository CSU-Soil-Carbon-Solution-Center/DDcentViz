# R/dates_units.R

#' Add a Date column from DayCent time + day-of-year columns
#'
#' DayCent outputs commonly include:
#' - time: year (often numeric; may include decimals)
#' - dayofyr: day of year (1-366)
#'
#' This helper constructs:
#' - year = floor(time)
#' - date = as.Date(paste0(year, "-", dayofyr), format = "%Y-%j")
#'
#' @param df data.frame
#' @param time_col Character. Column name for time/year.
#' @param doy_col Character. Column name for day-of-year.
#' @param date_col Character. Output date column name.
#'
#' @return data.frame with added date column
#' @export
dc_add_date <- function(df,
                        time_col = "time",
                        doy_col = "dayofyr",
                        date_col = "date") {
  if (!is.data.frame(df)) stop("`df` must be a data.frame.", call. = FALSE)
  if (!(time_col %in% names(df))) stop("Missing column: ", time_col, call. = FALSE)
  if (!(doy_col %in% names(df))) stop("Missing column: ", doy_col, call. = FALSE)
  
  yr <- floor(df[[time_col]])
  doy <- df[[doy_col]]
  
  df[[date_col]] <- as.Date(paste0(yr, "-", doy), format = "%Y-%j")
  df
}

#' Convert SOC from g C / m2 to Mg C / ha
#'
#' g/m2 -> Mg/ha:
#' 1 g/m2 = 0.01 Mg/ha
#'
#' @param x numeric
#' @return numeric
#' @export
g_m2_to_Mg_ha <- function(x) {
  x * 0.01
}
