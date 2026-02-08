# R/plot_carbon.R

#' Plot DayCent carbon pools (narrative)
#'
#' This function expects the DayCent dc_sip table as a data.frame and returns
#' a list of narrative ggplots.
#'
#' @param data data.frame. Parsed dc_sip output.
#' @param scenario character. Label for plot titles.
#' @param start_year numeric. Start year (inclusive).
#' @param end_year numeric. End year (inclusive).
#'
#' @return This function returns plots for different categories of the C pool.
#'
#' @details
#' This function was built to use outputs from a DayCent run to plot carbon pools over a specified period of time.
#' The output from this function is a list of seven plots:
#' \itemize{
#' \item Plot 1: plant carbon stocks
#' \item Plot 2: litter carbon stocks
#' \item Plot 3: soil organic carbon stocks
#' \item Plot 4: rapid turnover of soil carbon stocks
#' \item Plot 5: total system carbon stocks
#' \item Plot 6: turnover soil carbon stocks
#' \item Plot 7: relative turnover of soil carbon stocks
#' }
#'
#' @import tidyverse
plot_carbon_pools <- function(data, scenario, start_year, end_year){
  # Split columns based on their categories and the given order
  plant_cols <- c("rleavc", "fbrchc", "rlwodc", "wood1c", "wood2c", "aglivc",
                  "bglivcj", "bglivcm", "frootcj", "frootcm", "crootc")

  litter_cols <- c("stdedc", "metabc(1)", "strucc(1)", "strucc(2)", "metabc(2)")

  soc_cols <- c("som1c(1)", "som1c(2)", "som2c(1)", "som2c(2)", "som3c")

  aboveGround_cols <- c("rleavc", "fbrchc", "rlwodc", "wood1c", "wood2c", "aglivc",
                        "stdedc", "metabc(1)", "strucc(1)", "som1c(1)", "som2c(1)")

  belowGround_cols <- c("bglivcj", "bglivcm", "frootcj", "frootcm", "crootc",
                        "strucc(2)", "metabc(2)", "som1c(2)", "som2c(2)", "som3c")

  crop_grass_cols <- c("aglivc", "stdedc", "metabc(1)", "strucc(1)", "som1c(1)", "som2c(1)",
                       "bglivcj", "bglivcm", "strucc(2)", "metabc(2)", "som1c(2)", "som2c(2)", "som3c")

  tree_cols <- c("rleavc", "fbrchc", "rlwodc", "wood1c", "wood2c",
                 "frootcj", "frootcm", "crootc")

  # Check if columns exist
  required_cols <- unique(c(plant_cols, litter_cols, soc_cols, aboveGround_cols, belowGround_cols))
  missing_cols <- setdiff(required_cols, colnames(data))
  # if(length(missing_cols) > 0) {
  #   stop(paste("Missing columns:", paste(missing_cols, collapse=", ")))
  # }
  required_cols = factor(required_cols,
                         levels=c("rleavc","fbrchc","rlwodc","wood1c","wood2c","aglivc","stdedc","metabc(1)","strucc(1)","som1c(1)","som2c(1)",
                                  "bglivcj","bglivcm","crootc","frootcj","frootcm", "strucc(2)","metabc(2)","som1c(2)","som2c(2)","som3c"
                         ))
  # Pivot the data longer
  data_long <- data %>%
    pivot_longer(cols = any_of(required_cols), names_to = "Ccomponent", values_to = "Camt")

  # Create cType column
  data_long <- data_long %>%
    mutate(cType = case_when(
      Ccomponent %in% plant_cols ~ "plant",
      Ccomponent %in% litter_cols ~ "litter",
      Ccomponent %in% soc_cols ~ "soc",
      TRUE ~ NA_character_
    ))

  # Create aboveGround/belowGround column
  data_long <- data_long %>%
    mutate(groundType = case_when(
      Ccomponent %in% aboveGround_cols ~ "aboveGround",
      Ccomponent %in% belowGround_cols ~ "belowGround",
      TRUE ~ NA_character_
    ))

  # Create aboveGround/belowGround column
  data_long <- data_long %>%
    mutate(plantType = case_when(
      Ccomponent %in% crop_grass_cols ~ "crop_grass",
      Ccomponent %in% tree_cols ~ "tree",
      TRUE ~ NA_character_
    ))

  labels <- c("Leaves", "fine branches", "large wood", "dead fine branches", "dead large branches",
              "live shoots", "standing dead shoots", "abg metabolic litter", "abg structural litter", "surface active SOC", "surface slow SOC",
              "juvenile fine roots", "mature fine roots", "coarse roots", "juvenile fine root", "mature fine root",
              "blg metabolic litter", "blg structural litter", "active SOC", "slow SOC", "passive SOC")

  levels_order <- c("rleavc", "fbrchc", "rlwodc", "wood1c", "wood2c", "aglivc",
                    "stdedc", "metabc(1)", "strucc(1)", "som1c(1)", "som2c(1)",
                    "bglivcj", "bglivcm", "crootc", "frootcj", "frootcm",
                    "strucc(2)", "metabc(2)", "som1c(2)", "som2c(2)", "som3c")

  data_long =data_long%>%
    mutate(Ccomponent = Ccomponent %>%
             factor(levels=levels_order,labels = labels))

  data_long =data_long%>%
    mutate(Date = as.Date(paste0(time %>% floor(),"-", dayofyr), format = "%Y-%j"))

  # Assign hex codes for each component
  colors <- c("#228B22",   # Leaves - Forest Green
              "#8B4513",   # fine branches - Saddle Brown
              "#A0522D",   # large wood - Sienna
              "#DEB887",   # dead fine branches - Burly Wood
              "#8B4513",   # dead large branches - Saddle Brown
              "#556B2F",   # live shoots - Dark Olive Green
              "#F0E68C",   # standing dead shoots - Khaki1
              "#DAA520",   # surface metabolic litter - Goldenrod
              "#B8860B",   # surface structural litter - DarkGoldenrod
              "slateblue1",   # surface active SOC - SlateBlue1
              "#6959CD",   # surface slow SOC - SlateBlue3
              "#DEB887",   # juvenile fine roots - Burlywood3
              "#8B6508",   # mature fine roots - Burlywood4
              "#8B4513",   # coarse roots - Saddle Brown
              "#8B4513",   # juvenile fine root - Saddle Brown
              "#8B4513",   # mature fine root - Saddle Brown
              "#DAA520",   # subsurface structural litter - Golden Rod
              "#B8860B",   # subsurface metabolic litter - Dark Golden Rod
              "slateblue1",   # rapid SOC - SlateBlue1
              "slateblue3",   # slow SOC - SlateBlue3
              "#473C8B")   # passive SOC - SlateBlue4

  # Create a named vector for colors
  Ccomponent_colors <- setNames(colors, labels)


  g1 = ggplot()+
    geom_area(data = data_long %>% filter( groundType == "aboveGround",cType == "plant", plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = Camt, fill = Ccomponent))+
    geom_area(data = data_long %>% filter( groundType == "belowGround",cType == "plant",plantType == "crop_grass", time>=start_year, time <= end_year),
              aes(x = Date, y = -Camt, fill = Ccomponent))+
    theme_classic()+  scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "Plant  carbon [g/m2]", x = "Years", title = paste0(scenario," plant carbon stocks"))


  g2 = ggplot()+
    geom_area(data = data_long %>% filter( groundType == "aboveGround",cType == "litter", plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = Camt, fill = Ccomponent))+
    geom_area(data = data_long %>% filter( groundType == "belowGround",cType == "litter",plantType == "crop_grass", time>=start_year, time <= end_year),
              aes(x = Date, y = -Camt, fill = Ccomponent))+
    theme_classic()+
    scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "Litter carbon [g/m2]", x = "Years", title = paste0(scenario," litter carbon stocks"))

  g3 = ggplot()+
    geom_area(data = data_long %>% filter( groundType == "aboveGround",cType == "soc", plantType == "crop_grass", time>=start_year, time <= end_year),
              aes(x = Date, y = Camt, fill = Ccomponent))+
    geom_area(data = data_long %>% filter( groundType == "belowGround",cType == "soc",plantType == "crop_grass", time>=start_year, time <= end_year),
              aes(x = Date, y = -Camt, fill = Ccomponent))+
    theme_classic()+
    scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "Soil carbon [g/m2]", x = "Years", title = paste0(scenario," organic soil carbon stocks"))


  g4 = ggplot()+
    geom_area(data = data_long %>% filter( groundType == "aboveGround",Ccomponent != "surface slow SOC", plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = Camt, fill = Ccomponent))+
    geom_area(data = data_long %>% filter( groundType == "belowGround",Ccomponent != "passive SOC",Ccomponent != "slow SOC", plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = -Camt, fill = Ccomponent))+
    theme_classic()+
    scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "Rapid turnover soil carbon [g/m2]", x = "Years", title = paste0(scenario," rapid turnover carbon stocks"))

  g5 = ggplot()+
    geom_area(data = data_long %>% filter(plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = Camt, fill = Ccomponent))+
    theme_classic()+
    scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "total carbon [g/m2]", x = "Years", title = paste0(scenario," total system carbon stocks"))

  g6 = ggplot()+
    geom_area(data = data_long  %>% filter( groundType == "aboveGround" ,plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = Camt, fill = Ccomponent))+
    geom_area(data = data_long  %>% filter( groundType == "belowGround",plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = -Camt, fill = Ccomponent))+
    theme_classic()+
    scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "Relative turnover soil carbon [g/m2]", x = "Years", title = paste0(scenario))


  data_long_rel = data_long %>% filter(time>=start_year, time <= end_year) %>%
    group_by(Ccomponent) %>% mutate(Camt_rel = Camt- min(Camt))

  g7 = ggplot()+
    geom_area(data = data_long_rel  %>% filter( groundType == "aboveGround" ,plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = Camt_rel, fill = Ccomponent))+
    geom_area(data = data_long_rel  %>% filter( groundType == "belowGround",plantType == "crop_grass",time>=start_year, time <= end_year),
              aes(x = Date, y = -Camt_rel, fill = Ccomponent))+
    theme_classic()+
    scale_fill_manual(values = Ccomponent_colors) +
    geom_hline(yintercept = 0)+
    labs(y = "Relative turnover soil carbon [g/m2]", x = "Years", title = paste0("Delta Carbon ", scenario))


  return(list("plants" = g1,
              "litter" = g2,
              "soc" = g3,
              "rapid" = g4,
              "total" = g5,
              "SOC_abgbg" = g6,
              "year_rel"= g7))

}

#' Wrapper: build carbon narrative plots from a project/site/exp
#'
#' Finds a dc_sip CSV via \code{dc_manifest()} and returns named ggplots.
#'
#' @param project character. Project root.
#' @param site character. Site name.
#' @param exp character. Experiment name (folder under outputs).
#' @param scenario character. Plot label; default paste0(site, "_", exp).
#' @param start_year,end_year numeric. Optional; if NULL inferred from data$time.
#' @param file Optional path to override dc_sip selection.
#'
#' @return Named list of ggplot objects.
#' @export
dc_plot_carbon <- function(project,
                           site,
                           exp,
                           scenario = NULL,
                           start_year = NULL,
                           end_year = NULL,
                           file = NULL) {
  if (is.null(scenario) || !nzchar(scenario)) scenario <- paste0(site, "_", exp)

  mf <- dc_manifest(project, site, exp = exp)

  # pick file
  sip_path <- file
  if (is.null(sip_path) || !nzchar(sip_path)) {
    cand <- mf[mf$kind == "output" & mf$role == "dc_sip_csv", , drop = FALSE]
    if (nrow(cand) < 1) {
      stop("No dc_sip CSV found for site=", site, " exp=", exp,
           ". Expected role 'dc_sip_csv' in outputs/", exp, call. = FALSE)
    }
    # if multiple, choose newest by mtime
    cand <- cand[order(cand$mtime, decreasing = TRUE), ]
    sip_path <- cand$path[1]
  }

  df <- dc_read_output(sip_path)
  df <- dc_add_date(df)

  if (!("time" %in% names(df))) stop("dc_sip is missing column `time`.", call. = FALSE)
  if (!("dayofyr" %in% names(df))) stop("dc_sip is missing column `dayofyr`.", call. = FALSE)

  if (is.null(start_year) || is.null(end_year)) {
    yr_rng <- range(floor(df$time), na.rm = TRUE)
    if (is.null(start_year)) start_year <- yr_rng[1]
    if (is.null(end_year))   end_year <- yr_rng[2]
  }

  pls <- plot_carbon_pools(df, scenario = scenario, start_year = start_year, end_year = end_year)

  # Name plots consistently (your function currently returns g1..g7)
  if (is.null(names(pls)) || any(names(pls) == "")) {
    nm <- c(
      "Plant C",
      "Litter C",
      "SOC",
      "Rapid turnover C",
      "Total system C",
      "All C (turnover view)",
      "Delta C (relative)"
    )
    names(pls) <- nm[seq_along(pls)]
  }

  pls
}
