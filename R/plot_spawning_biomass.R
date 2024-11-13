#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @inheritParams plot_recruitment
#' @param ref_line A string specifying the type of reference you want to
#'   compare spawning biomass to. The default is `"target"`, which looks for
#'   `"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`.
#' @param relative A logical value specifying if the resulting figures should
#'   be relative spawning biomass. The default is `FALSE`. `ref_line` indicates
#'   which reference point to use.
#' @param unit_label units for spawning biomass that will be added as a label to
#' the y-axis in the plot. Default units are metric tons.
#'
#' @return
#' Plot spawning biomass from the results of an assessment model translated to
#' the standard output. The {ggplot2} object is returned for further
#' modifications if needed.
#' @export
#'
plot_spawning_biomass <- function(
  dat,
  unit_label = "metric ton",
  scale_amount = 1,
  ref_line = c("target", "unfished"),
  end_year = NULL,
  relative = FALSE
) {
  ref_line <- match.arg(ref_line)
  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  spawning_biomass_label <- ifelse(
    relative,
    yes = "Relative spawning biomass",
    no = glue::glue("Spawning biomass ({unit_label})")
  )

  output <- dat
  # Determine the end year
  all_years <- output[["year"]][grepl("^[0-9\\.]+$", output[["year"]])]
  if (is.null(end_year)) {
    end_year <- as.numeric(max(all_years, na.rm = TRUE))
  }
  stopifnot(any(end_year >= all_years))

  # Select value for reference line and label
  ref_line_val <- as.numeric(output[
    grep(
      pattern = glue::glue("^spawning_biomass.*{tolower(ref_line)}"),
      x = output[["label"]]
    ),
    "estimate"
  ])
  if (length(ref_line_val) == 0) {
    stop(glue::glue(
      "The resulting reference value of `spawning_biomass_{ref_line}` was
      not found in `dat[[\"label\"]]`."
    ))
  }
  sb <- output |>
    dplyr::filter(
      label == "spawning_biomass",
      module_name %in% c("DERIVED_QUANTITIES", "t.series"),
      year <= end_year
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year),
      estimate_y = estimate / ifelse(relative, ref_line_val, scale_amount),
      # TODO: Determine what unit uncertainty is in, following is for normal sd
      # TODO: This fails for Bayesian estimation
      estimate_lower = (estimate - 1.96 * uncertainty) /
        ifelse(relative, ref_line_val, scale_amount),
      estimate_upper = (estimate + 1.96 * uncertainty) /
        ifelse(relative, ref_line_val, scale_amount)
    )

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(sb[["year"]]) / 10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(sb[["year"]]) / 5)
  }

  plt <- ggplot2::ggplot(data = sb) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = year,
        y = estimate_y
      ),
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = ref_line_val / ifelse(relative, ref_line_val, scale_amount),
      linetype = 2
    ) +
    # Only add confidence intervals for the non NA estimates
    # which allows for no warnings if uncertainty = NA
    ggplot2::geom_ribbon(
      data = sb |> dplyr::filter(!is.na(estimate_lower)),
      ggplot2::aes(
        x = year,
        ymin = estimate_lower,
        ymax = estimate_upper
      ),
      colour = "grey",
      alpha = 0.3,
    ) +
    ggplot2::labs(
      x = "Year",
      y = spawning_biomass_label
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks,
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::annotate(
      geom = "text",
      x = end_year + 0.05,
      y = ref_line_val / ifelse(relative, ref_line_val, scale_amount),
      label = list(bquote(SB[.(ref_line)])),
      parse = TRUE
    )

  plt_fin <- suppressWarnings(add_theme(plt))
  return(plt_fin)
}
