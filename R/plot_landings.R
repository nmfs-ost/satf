#' Plot observed landings by fleet
#'
#' @template dat
#' @template model
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
  land <- utils::read.csv(dat) |>
    dplyr::filter(module_name == "t.series" | module_name == "CATCH",
                  grepl("landings", label) | label == "obs") |>
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year))

  land_ts <- land |>
    dplyr::filter(!is.na(year),
                  !is.na(fleet),
                  label == "landings_observed" | label == "obs")

  narea <- length(unique(land_ts$area))
  nseas <- length(unique(land_ts$season))

  if(narea > 1) {
    factors <- TRUE
  }
  if (nseas > 1) {
    land_ts2 <- land_ts |>
      dplyr::group_by(year, fleet, sex, area, growth_pattern) |>
      dplyr::summarize(estimate = mean(estimate)) |>
      dplyr::mutate(fleet = as.character(fleet))
  }


  plt <- ggplot2::ggplot(data = land_ts2) +
    ggplot2::geom_area(ggplot2::aes(x = year, y = estimate, fill = fleet))

  add_theme(plt)


}

# Std framework examples
# SS3
dat <- "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/test_ASAR/billfish_output.csv"
# BAM
#dat <- utils::read.csv("C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/test_ASAR/blk_sea_bass_output.csv")[,-1]
dat <- "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/test_ASAR/black_sea_bass_output.csv"

