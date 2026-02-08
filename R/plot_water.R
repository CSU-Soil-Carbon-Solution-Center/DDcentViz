# R/plot_water.R

#' Read DayCent layered output (vswc/wfps) into wide table with Layer_*
#'
#' @param file_path Path to a DayCent layered output file (e.g., vswc.out, wfps.out).
#' @return A data.frame/data.table with columns: time, dayofyr, Layer_*, date
#' @keywords internal
dc_read_layer_data <- function(file_path) {
  dat <- data.table::fread(file_path, header = FALSE)
  names(dat) <- c("time", "dayofyr", paste0("Layer_", seq_len(ncol(dat) - 2)))
  dc_add_date(dat, time_col = "time", doy_col = "dayofyr", date_col = "date")
}

#' Plot DayCent daily water balance narratives
#'
#' Produces water-year cumulative inflows/outflows/storage panels + soil moisture diagnostics.
#'
#' Requires outputs present for `project/site/exp`:
#' - watrbal.out
#' - vswc.out
#' - wfps.out
#' and an input:
#' - soils.in (for layer thickness)
#'
#' @param project character. Project root directory.
#' @param site character. Site name.
#' @param exp character. Experiment name.
#' @param soils_in character. Optional explicit soils.in path override.
#' @param start_year,end_year numeric. Optional inclusive year bounds.
#' @param years_in integer vector. Optional explicit set of years to keep (takes precedence over start/end).
#'
#' @return Named list of ggplot objects.
#' @export
dc_plot_water <- function(project,
                          site,
                          exp,
                          soils_in = NULL,
                          start_year = NULL,
                          end_year = NULL,
                          years_in = NULL) {

  # ---- resolve file paths via manifest
  mf <- dc_manifest(project, site, exp = exp, include_inputs = TRUE, include_outputs = TRUE)

  pick_one <- function(role, kind = NULL) {
    x <- mf[mf$role == role, , drop = FALSE]
    if (!is.null(kind)) x <- x[x$kind == kind, , drop = FALSE]
    if (nrow(x) < 1) return(NA_character_)
    x <- x[order(x$mtime, decreasing = TRUE), , drop = FALSE]
    x$path[1]
  }

  watrbal_path <- pick_one("watrbal_out", kind = "output")
  vswc_path    <- pick_one("vswc_out",    kind = "output")
  wfps_path    <- pick_one("wfps_out",    kind = "output")

  if (is.null(soils_in) || !nzchar(soils_in)) {
    soils_in <- pick_one("soils_in", kind = "input")
  }

  if (!nzchar(watrbal_path) || !file.exists(watrbal_path)) stop("watrbal output not found for exp=", exp, call. = FALSE)
  if (!nzchar(vswc_path)    || !file.exists(vswc_path))    stop("vswc output not found for exp=", exp, call. = FALSE)
  if (!nzchar(wfps_path)    || !file.exists(wfps_path))    stop("wfps output not found for exp=", exp, call. = FALSE)
  if (!nzchar(soils_in)     || !file.exists(soils_in))     stop("soils.in not found for site=", site, call. = FALSE)

  # ---- helpers
  apply_year_filter <- function(df, date_col = "date") {
    if (!date_col %in% names(df)) return(df)

    if (!is.null(years_in) && length(years_in) > 0) {
      yrs <- sort(unique(as.integer(years_in)))
      return(dplyr::filter(df, lubridate::year(.data[[date_col]]) %in% yrs))
    }

    if (!is.null(start_year) || !is.null(end_year)) {
      yr <- lubridate::year(df[[date_col]])
      if (is.null(start_year)) start_year <- min(yr, na.rm = TRUE)
      if (is.null(end_year))   end_year   <- max(yr, na.rm = TRUE)
      return(dplyr::filter(df, yr >= start_year, yr <= end_year))
    }

    df
  }

  normalize_watrbal_time_col <- function(wat) {
    if ("#time" %in% names(wat)) return(wat)
    if ("time" %in% names(wat))  { names(wat)[names(wat) == "time"] <- "#time"; return(wat) }
    if ("X.time" %in% names(wat)) { names(wat)[names(wat) == "X.time"] <- "#time"; return(wat) }
    stop("watrbal file missing time column (`#time` or `time`).", call. = FALSE)
  }

  # ---- read soils.in (top/bottom depth cm)
  soildata <- data.table::fread(soils_in, header = FALSE) |>
    dplyr::transmute(
      top_depth_cm    = .data$V1,
      bottem_depth_cm = .data$V2,
      thickness_cm    = .data$V2 - .data$V1,
      layer           = paste0("Layer_", dplyr::row_number())
    )

  # ---- read outputs
  # watrbal: has a header line to skip
  wat <- data.table::fread(watrbal_path, skip = 1) |>
    dc_clean_columns() |>
    normalize_watrbal_time_col()

  if (!("dayofyr" %in% names(wat))) stop("watrbal file missing `dayofyr` column.", call. = FALSE)

  wat <- dc_add_date(wat, time_col = "#time", doy_col = "dayofyr", date_col = "date")
  wat <- apply_year_filter(wat, date_col = "date")

  vswc <- dc_read_layer_data(vswc_path) |> apply_year_filter(date_col = "date")
  wfps <- dc_read_layer_data(wfps_path) |> apply_year_filter(date_col = "date")

  # compute baseflow if possible
  if (all(c("outflow", "runoff") %in% names(wat))) {
    wat <- dplyr::mutate(wat, baseflow = .data$outflow - .data$runoff)
  }

  # ---- vswc long -> cm water per layer
  vswc_long <- vswc |>
    tidyr::pivot_longer(cols = dplyr::contains("Layer_"), names_to = "layer") |>
    dplyr::left_join(soildata, by = "layer") |>
    dplyr::mutate(
      soil_water_cm = .data$value * .data$thickness_cm,
      layer_num = as.numeric(sub("Layer_", "", .data$layer)),
      layer = factor(.data$layer, levels = unique(.data$layer[rev(order(.data$layer_num))]))
    ) |>
    dplyr::select(-.data$layer_num)

  layers_n <- unique(vswc_long$layer)
  Layer_blues <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(length(layers_n)))
  names(Layer_blues) <- layers_n

  # ---- wfps long
  wfps_long <- wfps |>
    tidyr::pivot_longer(cols = dplyr::contains("Layer_"), names_to = "layer") |>
    dplyr::left_join(soildata, by = "layer")

  # ---- watrbal long (robust)
  id_cols <- c("#time", "dayofyr", "date")
  known_components <- c(
    "ppt","melt",
    "intrcpt","evap","transp","sublim",
    "runoff","baseflow", #"outflow",
    "snow","snlq"#,
    #"accum","dsnlq","dswc","balance"
  )
  comp_cols <- intersect(known_components, names(wat))
  if (length(comp_cols) == 0) comp_cols <- setdiff(names(wat), id_cols)

  wat_long <- wat |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(comp_cols),
      names_to = "component",
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      category = dplyr::case_when(
        .data$component %in% c("ppt", "melt") ~ "Inflow",
        .data$component %in% c("intrcpt", "evap", "transp", "sublim", "baseflow", "runoff") ~ "Outflow",
        .data$component %in% c("snow","snlq") ~ "Storage", #"accum","dsnlq","dswc","balance"
        TRUE ~ "Other"
      )
    )

  # ---- cumulative per year / per water year
  wat_long2 <- wat_long |>
    dplyr::mutate(
      year = lubridate::year(.data$date),
      water_year = dplyr::if_else(lubridate::month(.data$date) >= 10,
                                  lubridate::year(.data$date) + 1L,
                                  lubridate::year(.data$date))
    ) |>
    dplyr::group_by(.data$category, .data$component, .data$year) |>
    dplyr::mutate(cum_value = cumsum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$category, .data$component, .data$water_year) |>
    dplyr::mutate(wy_cum_value = cumsum(.data$value)) |>
    dplyr::ungroup()

  # ---- colors
  color_scheme <- c(
    "ppt" = "blue", "accum" = "lightblue", "dsnlq" = "cyan", "melt" = "steelblue",
    "intrcpt" = "tan", "evap" = "lightblue", "transp" = "darkgreen", "sublim" = "purple",
    "baseflow" = "blue4", "runoff" = "blue1", "dswc" = "green", "snow" = "lightgray", "snlq" = "gray",
    "outflow" = "navy", "balance" = "black"
  )

  # ---- storage panel includes negative layer water (cm)
  wat_long3 <- wat_long2 |>
    dplyr::filter(.data$category != "Other") |>
    dplyr::rename(time = `#time`)

  wat_long3 <- dplyr::bind_rows(
    wat_long3,
    vswc_long |>
      dplyr::mutate(
        category  = "Storage",
        component = as.character(.data$layer),
        year = lubridate::year(.data$date),
        water_year = dplyr::if_else(lubridate::month(.data$date) >= 10,
                                    lubridate::year(.data$date) + 1L,
                                    lubridate::year(.data$date)),
        value = - .data$soil_water_cm,
        cum_value = - .data$soil_water_cm,
        wy_cum_value = - .data$soil_water_cm
      ) |>
      dplyr::select(dplyr::any_of(names(wat_long3)))
  ) |>
    dplyr::mutate(
      # snow/snlq are instantaneous storages; keep as daily value (not cumulative)
      cum_value = dplyr::if_else(stringr::str_detect(.data$component, "snlq|snow"), .data$value, .data$cum_value),
      wy_cum_value = dplyr::if_else(stringr::str_detect(.data$component, "snlq|snow"), .data$value, .data$wy_cum_value),
      category = factor(.data$category, levels = c("Inflow", "Storage", "Outflow"))
    )

  # ordering components (layers top->bottom)
  static_order <- c("ppt","melt","snlq","snow","baseflow","transp","evap","runoff","sublim","intrcpt","outflow")
  layer_order <- unique(wat_long3$component) |>
    grep("^Layer_", x = _, value = TRUE) |>
    data.frame(layer = _, stringsAsFactors = FALSE) |>
    dplyr::mutate(layer_num = as.numeric(sub("Layer_", "", .data$layer))) |>
    dplyr::arrange(dplyr::desc(.data$layer_num)) |>
    dplyr::pull(.data$layer)

  final_order <- c(static_order[1:3], layer_order, static_order[4:length(static_order)])
  wat_long3$component <- factor(wat_long3$component, levels = final_order)

  color_scheme_select <- c(color_scheme, Layer_blues)[final_order]

  # ---- plots
  plot_inflows_wy <- ggplot2::ggplot(
    dplyr::filter(wat_long2, .data$category == "Inflow"),
    ggplot2::aes(x = .data$date, y = .data$wy_cum_value, fill = .data$component)
  ) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(values = color_scheme, na.value = "grey70") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cumulative Inflows (Water Year)", y = "Cumulative Water (cm)", x = "Date")

  plot_outflows_wy <- ggplot2::ggplot(
    dplyr::filter(wat_long2, .data$category == "Outflow"),
    ggplot2::aes(x = .data$date, y = .data$wy_cum_value, fill = .data$component)
  ) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(values = color_scheme, na.value = "grey70") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cumulative Outflows (Water Year)", y = "Cumulative Water (cm)", x = "Date")

  plot_storage_wy <- ggplot2::ggplot(
    dplyr::filter(wat_long3, .data$category == "Storage"),
    ggplot2::aes(x = .data$date, y = .data$wy_cum_value, fill = .data$component)
  ) +
    ggplot2::geom_area(color = "grey75", linewidth = 0.01) +
    ggplot2::scale_fill_manual(values = color_scheme_select, na.value = "grey70") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Storage (Water Year)", y = "Water Storage (cm)", x = "Date")

  plot_faceted_wy <- ggplot2::ggplot(
    wat_long3,
    ggplot2::aes(x = .data$date, y = .data$wy_cum_value, fill = .data$component)
  ) +
    ggplot2::geom_area(color = "grey75", linewidth = 0.01) +
    ggplot2::scale_fill_manual(values = color_scheme_select, na.value = "grey70") +
    ggplot2::facet_grid(rows = ggplot2::vars(category), scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Water Balance Components (Water Year)", y = "Cumulative Water (cm)", x = "Date")

  inflow_outflow <- wat_long2 %>%
    dplyr::filter(.data$category %in% c("Inflow", "Outflow")) %>%
    dplyr::mutate(
      value_signed = dplyr::if_else(.data$category == "Outflow", - .data$value, .data$value)
    ) %>%
    dplyr::ungroup()%>%
    # keep date, but collapse components to a daily net
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(net_daily = sum(.data$value, na.rm = TRUE)) %>%
    # compute WY cumulative, preserving date
    dplyr::arrange(.data$date) %>%
    # dplyr::group_by(.data$water_year) %>%
    dplyr::mutate(
      wy_cum_value = cumsum(.data$net_daily),
      category = "Net Balance"
    ) %>%
    dplyr::ungroup()

  plot_combined_wy <- ggplot2::ggplot() +
    ggplot2::geom_area(
      data = dplyr::filter(wat_long2, .data$category %in% c("Inflow","Outflow")) |>
        dplyr::group_by(.data$date, .data$category) |>
        dplyr::summarize(wy_cum_value = sum(.data$wy_cum_value, na.rm = TRUE), .groups = "drop"),
      ggplot2::aes(x = .data$date, y = .data$wy_cum_value, fill = .data$category),
      position = "identity",
      alpha = 0.75
    ) +
    ggplot2::geom_line(
      data = inflow_outflow,
      ggplot2::aes(x = .data$date, y = .data$wy_cum_value, color = .data$category),
      linewidth = 1
    ) +
    ggplot2::scale_fill_manual(values = c("Inflow" = "lightblue", "Outflow" = "gold"), na.value = "grey70") +
    ggplot2::scale_color_manual(values = c("Net Balance" = "darkgreen"), na.value = "grey70") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cumulative Inflow and Outflow (WY) + Net", y = "Cumulative Water (cm)", x = "Date")

  plot_wfps <- ggplot2::ggplot(
    wfps_long,
    ggplot2::aes(x = .data$date, y = (.data$bottem_depth_cm + .data$top_depth_cm) / 2, fill = .data$value)
  ) +
    ggplot2::geom_tile(ggplot2::aes(height = .data$top_depth_cm - .data$bottem_depth_cm), width = 1) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_gradient(low = "lightyellow", high = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Date", y = "Soil Depth (cm)", fill = "Saturation", title = "Soil Water Saturation (WFPS)")

  plot_vswc <- ggplot2::ggplot(
    vswc_long,
    ggplot2::aes(x = .data$date, y = .data$soil_water_cm, fill = .data$layer)
  ) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(values = Layer_blues, na.value = "grey70") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Date", y = "Soil Water (cm)", fill = "Layer", title = "Soil Water Content by Layer")

  list(
    wy_inflows          = plot_inflows_wy,
    wy_outflows         = plot_outflows_wy,
    wy_storage          = plot_storage_wy,
    wy_inOutFacet       = plot_faceted_wy,
    wy_inflow_outflow   = plot_combined_wy,
    moisture_content_prct = plot_wfps,
    water_content_cm    = plot_vswc
  )
}
