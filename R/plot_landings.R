#' Plot observed landings by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of landings as to label the axis
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time by fleet.Includes options to plot by fleet, total observed landings
#' with and without predicted landings. Indicate if fleet should be faceted or on one plot (default). Warning: i
#' @export
#'
plot_landings <- function(dat,
                          unit_label = "metric tons"){
  # Units
  # TODO: fix unit label is scaling
  land_label <- glue::glue("Landings ({unit_label})")

  # read standard data file and extract target quantity
  land <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
      ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year),
      fleet = as.character(fleet)
      ) |>
    suppressWarnings() |>
    dplyr::filter(
      !is.na(year)
    )

  #Check number of areas and season - if any are >1 then need to use alternative plot (or summarize)
  narea <- length(unique(land$area))
  nseas <- length(unique(land$season))

  if(narea > 1) {
    factors <- TRUE
    # will need facet if TRUE
  } else {
    factors <- FALSE
  }
  # Check for nseas > 1 - mean of landings through the year
  if (nseas > 1) {
    land <- land |>
      dplyr::group_by(year, fleet, sex, area, growth_pattern) |>
      dplyr::summarize(estimate = mean(estimate)) |>
      dplyr::mutate(fleet = as.character(fleet))
  }

  # Make generic plot
  plt <- ggplot2::ggplot(data = land) +
    ggplot2::geom_area(
      ggplot2::aes(
        x = year,
        y = estimate,
        fill = fleet)) #+
    # ggplot2::facet_wrap(~label)
  # Apply std NOAA theme
  plt_fin <- add_theme(plt)
  return(plt_fin)
}

