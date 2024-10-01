#' Plot Recruitment
#'
#' @param dat A data frame returned from `asar::convert_output()`.
#' @param params Print/export the parameters of the stock recruitment function?
#' @param params_only Only export the stock recruitment function or both the parameters and the plot(s)?
#' @param units If units are not available in the output file, in metric tons,
#' or are different for SB and R, then report them here starting with SB units
#' and following with R units.
#' @param spawning_biomass_units units of spawning biomass if different from biomass
#' @param recruitment_units units for recruitment
#' @param scaled T/F; indicate whether the output values for biomass and recruitment are scaled
#' @param scale_amount indicate the exact amount of scale (i.e. 1000)
#' @param show_warnings Include warnings? Default FALSE
#' @param end_year last year of assessment
#' @param return Default returns recruitment over time. Options to display recruitment time series, stock recruitment curve, or recruitment fit
#'
#' @return A series of plots are exported including recruitment over time with R0
#' reference line, stock recruitment curve, and other related figures.
#' @export
#'
plot_recruitment <- function(dat,
                             params = FALSE,
                             params_only = FALSE,
                             units = c(sb = "metric tons", recruitment = "metric tons"),
                             recruitment_units = "metric tons",
                             spawning_biomass_units = "metric tons",
                             scaled = FALSE,
                             scale_amount = NULL,
                             show_warnings = FALSE,
                             end_year = NULL,
                             return = "recruitment"){
  # check units
  # biomass
  if(!is.null(recruitment_units)){
    ru <- recruitment_units
  } else {
    ru <- "metric tons"
  }
  # spawning biomass
  if(!is.null(spawning_biomass_units)){
    sbu <- spawning_biomass_units
  } else {
    sbu <- "metric tons"
  }

  output <- dat
  if (scaled) {
    rec <- output |>
      dplyr::filter(label == "recruitment",
                    module_name == "TIME_SERIES" | module_name == "t.series",
                    !is.na(year),
                    is.na(fleet) | length(unique(fleet)) <= 1,
                    is.na(sex) | length(unique(sex)) <= 1,
                    is.na(area) | length(unique(area)) <= 1,
                    is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                    year != "S/Rcurve" | year != "Init" | year != "Virg"
      ) |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year)) |>
      dplyr::rename(recruitment = estimate) |>
      dplyr::select(-c(module_name, label))
    sb <- output |>
      dplyr::filter(label == "spawning_biomass",
                    module_name == "TIME_SERIES" | module_name == "t.series",
                    !is.na(year),
                    is.na(fleet) | length(unique(fleet)) <= 1,
                    is.na(sex) | length(unique(sex)) <= 1,
                    is.na(area) | length(unique(area)) <= 1,
                    is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                    year != "S/Rcurve" | year != "Init" | year != "Virg"
      ) |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year)) |>
      dplyr::rename(spawning_biomass = estimate) |>
      dplyr::select(-c(module_name, label))
  } else {
    rec <- output |>
      dplyr::filter(label == "recruitment",
                    module_name == "TIME_SERIES" | module_name == "t.series",
                    !is.na(year),
                    is.na(fleet) | length(unique(fleet)) <= 1,
                    is.na(sex) | length(unique(sex)) <= 1,
                    is.na(area) | length(unique(area)) <= 1,
                    is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                    year != "S/Rcurve" | year != "Init" | year != "Virg"
      ) |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year)) |>
      dplyr::rename(recruitment = estimate) |>
      dplyr::select(-c(module_name, label))
    sb <- output |>
      dplyr::filter(label == "spawning_biomass",
                    module_name == "TIME_SERIES" | module_name == "t.series",
                    !is.na(year),
                    is.na(fleet) | length(unique(fleet)) <= 1,
                    is.na(sex) | length(unique(sex)) <= 1,
                    is.na(area) | length(unique(area)) <= 1,
                    is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                    year != "S/Rcurve" | year != "Init" | year != "Virg"
      ) |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year)) |>
      dplyr::rename(spawning_biomass = estimate) |>
      dplyr::select(-c(module_name, label))
  }

  rec_devs <- output |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  year != "S/Rcurve" | year != "Init" | year != "Virg"
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::rename(recruitment_deviations = estimate) |>
    dplyr::select(-c(module_name, label))
  if(return == "recruitment_deviations" & nrow(rec_devs)==0){
    stop("No recruitment deviations found in data.")
  }

  if (is.null(end_year)){
    endyr <- max(rec$year)
  } else {
    endyr <- end_year
  }
  stryr <- min(rec$year)

  # merge DF
  sr <- dplyr::full_join(sb, rec)

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(subset(sr, year<=endyr)$year)/10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(subset(sr, year<=endyr)$year)/5)
  }

  if (return == "stock_recruitment"){
    plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_line(ggplot2::aes(x = spawning_biomass, y = recruitment/1000), linewidth = 1) +
      ggplot2::labs(x = paste("Spawning Biomass (", sbu, ")", sep = ""),
                    y = paste("Recruitment (", ru, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")
    # sr_plt <- add_theme(sr_plt)
  } else if (return == "recruitment") {
    plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = recruitment)) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = recruitment), linewidth = 1) +
      ggplot2::labs(x = "Year",
                    y = paste("Recruitment (", ru, ")", sep = "")) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                  guide = ggplot2::guide_axis(minor.ticks = TRUE))
  } else if (return == "recruitment_deviations") {
    plt <- ggplot2::ggplot(data = rec_devs) +
      # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
      ggplot2::geom_pointrange(ggplot2::aes(x = year, y = estimate, ymax = Value, ymin = 0),  fatten = 1, size = 2, shape = 1) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(x = "Year",
                    y = "Recruitment Deviations")
  }
  plt_fin <- add_theme(plt)
  return(plt_fin)
}
