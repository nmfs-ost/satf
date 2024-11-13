#' Plot Recruitment Deviations
#'
#'  @inheritParams plot_recruitment
#'
#' @returnPlot Plot recruitment deviations relative to one over time from an assessment model output file
#' translated to a standardized output. There are options to return a {ggplot2}
#' object or export an .rda object containing associated caption and alternative
#' text for the figure.
#' @export
#'
#' @examples
plot_recruitment_deviations <- function(
    dat = NULL,
    end_year = NULL,
    n_projected_years = 10,

) {
  if (is.null(end_year)) {
    end_year <- max(as.numeric(dat$year), na.rm = TRUE) - n_projected_years
  } else {
    end_year <- end_year
  }
  stryr <- min(as.numeric(dat$year), na.rm = TRUE)

  rec_devs <- dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  year != "S/Rcurve" | year != "Init" | year != "Virg",
                  year <= end_year
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) # |>
    # dplyr::rename(recruitment_deviations = estimate) |>
    # dplyr::select(-c(module_name, label))
  if(nrow(rec_devs) == 0) {
    stop("No recruitment deviations found in data.")
  }

  # Plot
  plt <- ggplot2::ggplot(data = rec_devs) +
    ggplot2::geom_point(ggplot2::aes(x = year, y = estimate), shape = 1, size = 2.5) +
    # ggplot2::geom_pointrange(
    #   ggplot2::aes(
    #     x = year,
    #     y = estimate,
    #     ymax = Value,
    #     ymin = 0),
    #   fatten = 1,
    #   size = 2,
    #   shape = 1
    #   ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
      ) +
    ggplot2::labs(
      x = "Year",
      y = "Recruitment Deviations"
      )
  suppressWarnings(add_theme(plt))
}
