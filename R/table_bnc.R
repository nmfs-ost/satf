table_bnc <- function(
    dat,
    unit_label,
    alt_label
    ) {

  biomass <- dat |>
    dplyr::filter(
      # SS3 params
      label == "biomass",
      !is.na(year),
      module_name %in% c("TIME_SERIES","t.series"),
      is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
      module_name != "DERIVED_QUANTITIES"
    ) |>
    dplyr::rename(biomass = estimate) |>
    dplyr::select(year, biomass)

  if (length(unique(biomass$year)) != nrow(biomass)){
    stop("Duplicate years found in biomass df.")
  }

  catch <- dat |>
    dplyr::filter(
      # SS3 params
      grepl("catch", label),
      !is.na(year),
      # module_name %in% c("TIME_SERIES","t.series"),
      # is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
      module_name != "DERIVED_QUANTITIES"
    )
  # Check if df is by age and summarize to time series
  if (any(grepl("at_age", tolower(catch$module_name)))) {
    catch <- catch |>
      dplyr::group_by(year) |>
      dplyr::summarise(total_catch = sum(estimate))
  }

  abundance <- dat |>
    dplyr::filter(
      # SS3 params
      grepl("mature_abundance", label), # | statement for BAM label
      !is.na(year),
      module_name %in% c("TIME_SERIES","t.series"),
      # is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
      module_name != "DERIVED_QUANTITIES"
    ) |>
    dplyr::rename(abundance = estimate) |>
    dplyr::select(year, abundance)

  # Bring together quantities for table
  bnc <- biomass |>
    dplyr::left_join(abundance, by = "year") |>
    dplyr::left_join(catch, by = "year")

  # identify output
  fig_or_table <- "table"

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv"))
  ) {
    satf::write_captions(dat = dat,
                         dir = getwd(),
                         year = NULL)
  }

  # REMINDERS: add in code that
  # -adds make_rda and rda_dir as arguments
  # -defines topic_label, caps_alttext; and
  # -makes an rda if make_rda = TRUE
  # (see table_indices.R for reference)
  # for the rda-related fxns to work, the final table has to be called tab
}
