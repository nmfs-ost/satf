#' Plot Spawn-Recruit Curve
#'
#'  @inheritParams plot_recruitment
#'  @param spawning_biomass_label Units for spawning biomass
#'  @param recruitment_label units for recruitment
#'
#' @return
#' @export
#'
#' @examples
plot_spawn_recruitment <- function(
    dat = NULL,
    spawning_biomass_label = "metric tons",
    recruitment_label = "metric tons",
    end_year = NULL
) {
  if (is.null(end_year)){
    endyr <- max(rec$year)
  } else {
    endyr <- end_year
  }
  stryr <- min(rec$year)

  # Extract spawning biomass
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

  # Extract recruitment
  rec <- dat |>
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

  # merge DF
  sr <- dplyr::full_join(sb, rec)

  # Plot
  plt <- ggplot2::ggplot(data = sr) +
    ggplot2::geom_line(ggplot2::aes(x = spawning_biomass, y = recruitment/1000), linewidth = 1) +
    ggplot2::labs(x = glue::glue("Spawning Biomass ({spawning_biomass_label})"),
                  y = glue::glue("Recruitment ({recruitment_label})")) +
    ggplot2::theme(legend.position = "none")
  # sr_plt <- add_theme(sr_plt)
}
