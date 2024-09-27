#' Plot observed landings by fleet
#'
#' @inheritParams plot_recruitment
#' @param units indicate the name of the units of landings as to label the axis
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time by fleet.Includes options to plot by fleet, total observed landings
#' with and without predicted landings. Indicate if fleet should be faceted or on one plot (default). Warning: i
#' @export
#'
plot_landings <- function(dat,
                          model = "standard",
                          units = NULL){
  # check to make sure file works with fxn
  if (grepl(".sso|.rdat", dat)) {
    stop("File type not compatible with function. Please use standard csv format of output files. An example can be found at https://github.com/nmfs-ost/satf")
  }
  # read standard data file and extract target quantity
  land <- utils::read.csv(dat) |>
    dplyr::filter(module_name == "t.series" | module_name == "CATCH", # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
                  grepl("landings", label) | label == "obs") |>
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year))

  land_ts <- land |>
    dplyr::filter(!is.na(year),
                  !is.na(fleet),
                  label == "landings_observed" | label == "obs")

  #Check number of areas and season - if any are >1 then need to use alternative plot (or summarize)
  narea <- length(unique(land_ts$area))
  nseas <- length(unique(land_ts$season))

  if(narea > 1) {
    factors <- TRUE
    # will need facet if TRUE
  }
  # Check for nseas > 1 - mean of landings through the year
  if (nseas > 1) {
    land_ts <- land_ts |>
      dplyr::group_by(year, fleet, sex, area, growth_pattern) |>
      dplyr::summarize(estimate = mean(estimate)) |>
      dplyr::mutate(fleet = as.character(fleet))
  }

  # Make generic plot
  plt <- ggplot2::ggplot(data = land_ts) +
    ggplot2::geom_area(ggplot2::aes(x = year, y = estimate, fill = fleet))
  # Apply std NOAA theme
  add_theme(plt)
}

