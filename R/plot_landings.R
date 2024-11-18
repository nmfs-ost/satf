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
                          units = NULL,
                          make_rda = FALSE,
                          rda_dir = getwd()
                          ){

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "landings"
  fig_or_table <- "figure"

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table)

  # read standard data file and extract target quantity
  land <- dat |>
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

  plt_fin <- suppressWarnings(add_theme(plt))

  # add alt text and caption
  plt_fin <- plt_fin +
    ggplot2::labs(caption = caps_alttext[[1]],
                  alt = caps_alttext[[2]]
    )

  # export figure to rda if argument = T
  if (make_rda == TRUE){

    export_rda(plt_fin = plt_fin,
               caps_alttext = caps_alttext,
               rda_dir = rda_dir,
               topic_label = topic_label,
               fig_or_table = fig_or_table)
  }


  return(plt_fin)
}

