#' Plot Spawn-Recruit Curve
#'
#'  @inheritParams plot_recruitment
#'  @param spawning_biomass_label Units for spawning biomass
#'  @param recruitment_label units for recruitment
#'
#' @return Plot spawning recruitment relationship from a standardized output file originating from asar::convert_output()
#' @export
#'
plot_spawn_recruitment <- function(
    dat = NULL,
    spawning_biomass_label = "metric tons",
    recruitment_label = "metric tons",
    end_year = NULL,
    make_rda = FALSE,
    rda_dir = getwd()
) {
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

  if (is.null(end_year)){
    endyr <- max(rec$year)
  } else {
    endyr <- end_year
  }
  stryr <- min(rec$year)

  # Extract spawning biomass
  sb <- dat |>
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

  # merge DF
  sr <- dplyr::full_join(sb, rec)

  # Plot
  plt <- ggplot2::ggplot(data = sr) +
    ggplot2::geom_point(ggplot2::aes(x = spawning_biomass, y = recruitment/1000)) +
    ggplot2::labs(x = glue::glue("Spawning Biomass ({spawning_biomass_label})"),
                  y = glue::glue("Recruitment ({recruitment_label})")) +
    ggplot2::theme(legend.position = "none")

  plt_fin <- suppressWarnings(add_theme(plt))

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "est_stock_recruitment"

  # identify output
  fig_or_table <- "figure"

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv"))
  ) {
    satf::write_captions(dat = dat,
                         dir = getwd(),
                         year = end_year)
  }

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table)

  # export figure to rda if argument = T
  if (make_rda == TRUE){

    export_rda(plt_fin = plt_fin,
               caps_alttext = caps_alttext,
               rda_dir = rda_dir,
               topic_label = topic_label,
               fig_or_table = fig_or_table)

  }
}
