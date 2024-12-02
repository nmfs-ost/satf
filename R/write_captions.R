#' Write captions and alternative text
#'
#' Function to create captions and alternative text that contain
#' key quantities from the model results file.
#'
#' @inheritParams plot_spawning_biomass
#' @param dir Directory where the output captions and alt text file should be saved
#' @param year the last year of the data or the current year this function is being performed
#'
#' @return Exports .csv with captions and alt text for figures and tables
#' that contain key quantities (e.g., an assessment's start year) that
#' are automatically extracted from the converted model results file.
#'
#' @export

write_captions <- function(dat, # converted model output object
                           dir = NULL,
                           year = NULL){

  # import pre-written captions and alt text that include placeholders
  # for key quantities (e.g., 'start_year' is the placeholder for the
  # assessment's start year)
  caps_alttext <- utils::read.csv(
    system.file("resources", "captions_alt_text_template.csv", package = "satf")
  )

  # extract key quantities (these are examples and are not accurate)
  # REMINDER: the variable names must exactly match those in the captions/alt text csv.
  #
  # This start_year was written previously. Is it usable for any plots, below?
  # start_year <- min(as.numeric(dat$year[dat$year!="S/Rcurve" | dat$year!="Virg" | dat$year!="Init"]), na.rm = TRUE)

  # FIGURES-----

  ## kobe plot
  # B_BMSY_min <- # minimum value of B/B(MSY)
  # B_BMSY_max <- # maximum value of B/B(MSY)
  # F_FMSY_min <- # minimum value of F/F(MSY)
  # F_FMSY_max <- # maximum value of F/F(MSY)
  # B_BMSY_end_yr <- # value of B/B(MSY) at the end year
  # F_FMSY_end_yr <- # value of F/F(MSY) at the end year
  # overfished_status_is_isnot <- # object that should be "is" or "is not" and answers the question, "the stock overfishing status ___ overfished"
  # overfishing_status_is_isnot <- # object that should be "is" or "is not" and answers the question, "the stock ___ experiencing overfishing"
  # kobe_start_year <- # start year of kobe plot
  # kobe_end_year <- # end year of kobe plot

  ## Biomass plot
  # B_ref_pt <- # biomass reference point
  # B_ref_pt_unit <- # biomass reference point unit

  # start year of biomass plot
  B_start_year <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of biomass plot
  # B_end_year <- # this will take some thought, since end_year is
    # either entered by the user or calculated in plot_biomass.R

  # units of B (plural)
  # B_units <- # this will take some thought, since units is
    # entered by the user in plot_biomass.R

  # minimum B
  B_min <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # maximum B
  B_max <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # Bend <-
  # Btarg <-
  # Bmsy <-

  ## mortality (F) plot
  # F_ref_pt <- # F reference point
  # F_ref_pt_unit <- # F reference point unit
  # F_start_year_ <- # start year of F plot
  # F_end_year <- # start year of F plot
  # F_units <- # units of F (plural)
  # F_min <- # minimum F
  # F_max <- # maximum F
  # NOTE: SB added the [1] so one number would be assigned to Fend; otherwise,
  # this breaks because Fend = hundreds of numbers
  Fend_df <- dat |>
    dplyr::filter(label == "fishing_mortality" & year == year | label == "F_terminal")
  Fend <- as.numeric(Fend_df$estimate)[1]
  # Ftarg <-
  # F_Ftarg <-

  ## landings plot

  # start year of landings plot
  landings_start_year <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      year = as.numeric(year)
    ) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of landings plot
  landings_end_year <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      year = as.numeric(year)
    ) |>
    dplyr::slice(which.max(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # units of landings (plural)
  # landings_units <- # this will take some thought, since units is
  # entered by the user in plot_landings.R

  # minimum landings
  landings_min <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # maximum landings
  landings_max <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  ## natural mortality (M)
  # M_age_min <- # minimum age of M
  # M_age_max <- # maximum age of M
  # M_units <- # units of M (plural)
  # M_rate_min <- # minimum M rate
  # M_rate_max <- # maximum M rate

  ## vonB LAA (von Bertalanffy growth function + length at age)
  # vonb_age_units <- # vonB age units (plural)
  # vonb_age_min <- # minimum vonB age
  # vonb_age_max <- # maximum vonB age
  # vonb_length_units <- # vonB length units (plural)
  # vonb_length_min <- # minimum vonB length
  # vonb_length_max <- # minimum vonB length

  ## length-type conversion plot
  # total_length_units <- # total length units (plural)
  # total_length_min <- # minimum total length
  # total_length_max <- # maximum total length
  # fork_length_units <- # fork length units (plural)
  # fork_length_min <- # minimum fork length
  # fork_length_max <- # maximum fork length

  ## weight-length conversion plot
  # wl_length_units <- # length units (plural)
  # wl_length_min <- # minimum length
  # wl_length_max <- # maximum length
  # wl_weight_units <- # weight units (plural)
  # wl_weight_min <- # minimum weight
  # wl_weight_max <- # maximum weight

  ## maturity schedule (proportion mature)
  # prop_mat_length_units <- # length units (plural)
  # prop_mat_length_min <- # minimum length
  # prop_mat_length_max <- # maximum length

  ## fecundity at length
  # fecundity_length_units <- # length units (plural)
  # fecundity_length_min <- # minimum length
  # fecundity_length_max <- # maximum length
  # fecundity_units <- # fecundity units (plural)
  # fecundity_min <- # minimum fecundity
  # fecundity_max <- # maximum fecundity

  ## CAA (catch at age)
  # fleet_or_survey_name <- # fleet or survey name (SHARED with CAL, below)
  # caa_age_min <- # minimum age group
  # caa_age_max <- # maximum age group

  ## CAL (catch at length)
  # cal_length_min <- # minimum length group
  # cal_length_max <- # maximum length group

  ## CPUE indices plot
  # cpue_start_year <- # start year of CPUE indices plot
  # cpue_end_year <- # end year of CPUE indices plot
  # cpue_units <- # CPUE units (plural) (SHARED with mod_fit_abun, below)
  # cpue_min <- # minimum CPUE (SHARED with mod_fit_abun, below)
  # cpue_max <- # maximum CPUE (SHARED with mod_fit_abun, below)

  ## NAA (numbers at age)
  # bubble_start_year_min <- # start year of NAA plot
  # bubble_end_year_max <- # end year of NAA plot
  # bubble_age_units <- # age units (plural)
  # bubble_age_min <- # minimum age
  # bubble_age_max <- # maximum age

  ## mod_fit_catch (model fit to catch ts)
  # mod_fit_catch_start_year <- # start year of model fit to catch ts plot
  # mod_fit_catch_end_year <- # end year of model fit to catch ts plot
  # mod_fit_catch_units <- # catch units (plural)
  # mod_fit_catch_min <- # minimum catch
  # mod_fit_catch_max <- # maximum catch

  ## mod_fit_abun (model fit to abundance indices plot)
  # mod_fit_abun_start_year <- # start year of model fit to abundance indices plot
  # mod_fit_abun_end_year <- # end year of model fit to abundance indices plot

  ## mod_fit_discards
  # mod_fit_discards_start_year <- # start year of model fit to discards plot
  # mod_fit_discards_end_year <- # end year of model fit to discards plot
  # mod_fit_discards_units <- # discards units (plural)
  # mod_fit_discards_min <- # minimum discards
  # mod_fit_discards_max <- # maximum discards

  ## selectivity
  # selectivity_start_year <- # start year of selectivity plot
  # selectivity_end_year <- # end year of selectivity plot
  # selectivity_length_units <- # length units (plural)
  # selectivity_length_min <- # minimum length
  # selectivity_length_max <- # maximum length


  ## estimated stock recruitment (aka spawning stock biomass)
  # youngest-age recruited fish (instead of age-0)
  # sr_age_min <- # unsure how to extract this

  # ssb units (plural)
  # sr_ssb_units <- # this will take some thought, since
  # spawning_biomass_label is entered by the user in plot_spawn_recruitment.R

  # minimum ssb
  sr_ssb_min <- dat |>
  dplyr::filter(label == "spawning_biomass",
                module_name == "TIME_SERIES" | module_name == "t.series",
                !is.na(year),
                is.na(fleet) | length(unique(fleet)) <= 1,
                is.na(sex) | length(unique(sex)) <= 1,
                is.na(area) | length(unique(area)) <= 1,
                is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                !year %in% c('S/Rcurve', 'Init', 'Virg')
  ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # maximum ssb
  sr_ssb_max <- dat |>
    dplyr::filter(label == "spawning_biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% c('S/Rcurve', 'Init', 'Virg')
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # recruitment units (plural)
  # sr_units <- # this will take some thought, since recruitment_label
  # is entered by the user in plot_spawn_recruitment.R

  # minimum recruitment
  sr_min <- dat |>
  dplyr::filter(label == "recruitment",
                module_name == "TIME_SERIES" | module_name == "t.series",
                !is.na(year),
                is.na(fleet) | length(unique(fleet)) <= 1,
                is.na(sex) | length(unique(sex)) <= 1,
                is.na(area) | length(unique(area)) <= 1,
                is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                !year %in% c('S/Rcurve', 'Init', 'Virg')
  ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # maximum recruitment
  sr_max <- dat |>
    dplyr::filter(label == "recruitment",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% c('S/Rcurve', 'Init', 'Virg')
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()


  ## recruitment ts
  # recruitment units (plural) - numbers of fish, in thousands
  # recruitment_units <- # this will take some thought, since unit_label is
    # entered by the user in plot_recruitment.R

  # start year of recruitment ts plot
  recruitment_start_year <- dat |>
    dplyr::filter(label == "recruitment",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% c('S/Rcurve', 'Init', 'Virg')
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of recruitment ts plot
  # recruitment_end_year <- # this will take some thought, since end_year is
    # either entered by the user or calculated in plot_recruitment.R

  # minimum recruitment
  recruitment_min <- sr_min

  # maximum recruitment
  recruitment_max <- sr_max


  ## recruitment deviations
  # start year of recruitment deviations plot
  recruit_dev_start_year <-  dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% c('S/Rcurve', 'Init', 'Virg')
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of recruitment deviations plot
  # recruit_dev_end_year <- # this will take some thought, since end_year is
  # either entered by the user or calculated in plot_recruitment_deviations.R

  # minimum recruitment deviation
  recruit_dev_min <- dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% c('S/Rcurve', 'Init', 'Virg')
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # maximum recruitment deviation
  recruit_dev_max <- dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% c('S/Rcurve', 'Init', 'Virg')
    ) |> # SS3 and BAM target module names
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  ## tot_b (total biomass)
  # biomass_start_year <- # start year of biomass plot
  # biomass_end_year <- # end year of biomass plot
  # biomass_units <- # biomass units (plural)
  # biomass_min <- # minimum biomass
  # biomass_max <- # maximum biomass
  # biomass_ref_pt <- # biomass reference point
  # biomass_ref_pt_units <- # biomass reference point units

  ## spawning_biomass (ssb)
  # start year of ssb plot
  ssb_start_year <- dat |>
  dplyr::filter(
    label == "spawning_biomass",
    module_name %in% c("DERIVED_QUANTITIES", "t.series")
  ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of ssb plot
  # ssb_end_year <- # this will take some thought, since end_year is
  # either entered by the user or calculated in plot_spawning_biomass.R

  # ssb units (plural)
  # ssb_units <- # this will take some thought, since unit_label is
  # entered by the user in plot_spawning_biomass.R

  # minimum ssb
  ssb_min <- dat |>
    dplyr::filter(
      label == "spawning_biomass",
      module_name %in% c("DERIVED_QUANTITIES", "t.series")
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # maximum ssb
  ssb_max <- dat |>
    dplyr::filter(
      label == "spawning_biomass",
      module_name %in% c("DERIVED_QUANTITIES", "t.series")
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric()

  # ssb reference point
  # ssb_ref_pt <- # this will take some thought, since ref_line_val is
  # calculated in plot_spawning_biomass.R

  # ssb reference point units
  # ssb_ref_pt_units <- ssb_units


  ## spr (spawning potential ratio)
  # spr_start_year <- # start year of spr plot
  # spr_end_year <- # end year of spr plot
  # spr_min <- # minimum spr
  # spr_max  <- # maximum spr
  # spr_ref_pt <- # spr reference point
  # spr_ref_pt_units <- # spr reference point units

  ## pop_naa_baa (population numbers at age and population biomass at age)
  # pop_naa_baa_start_year <- # start year of spr plot
  # pop_naa_baa_end_year <- # end year of spr plot
  # pop_naa_baa_fish_min <- # minimum number of fish
  # pop_naa_baa_fish_max <- # maximum number of fish

  ## proj_catch (projected catch)
  # proj_catch_units <- # projected catch units (plural)
  # proj_catch_start_year <- # start year of projected catch plot
  # proj_catch_end_year <- # end year of projected catch plot
  # proj_catch_min <- # minimum projected catch
  # proj_catch_max <- # maximum projected catch

  ## proj_biomass (projected biomass)
  # proj_biomass_units <- # projected biomass units (plural)
  # proj_biomass_start_year <- # start year of projected biomass plot
  # proj_biomass_end_year <- # end year of projected biomass plot
  # proj_biomass_min <- # minimum projected biomass
  # proj_biomass_max <- # maximum projected biomass
  # proj_biomass_ref_pt <- # projected biomass reference point
  # proj_biomass_ref_pt_units <- # projected biomass reference point units

  ## Other
  # tot_catch <-
  # M <-
  # steep <-
  # R0 <-
  # SBmsy <-
  # fSB <-
  # sbtarg <-

  # TABLES-----

  ## catch
  # catch_fleet <- # fleet

  ## landings
  # landings_tbl_units <- # landings units; remove if units already in table

  ## discards
  # discards_tbl_units <- # discards units

  ## catchability
  # catchability_fleet <- # fleet


  # add in more quantities here, and update the quantities above

  # substitute quantity placeholders in the captions/alt text with
  # the real values, extracted above
  caps_alttext_subbed <- caps_alttext |>
    dplyr::mutate_if(is.character,
                   stringr::str_replace_all,
                   pattern = c("Fend"),
                   replacement = c(as.character(Fend))) |>
    dplyr::mutate_if(is.character,
                     stringr::str_replace_all,
                     pattern = c("B_min"),
                     replacement = c(as.character(B_min))) |>
    dplyr::mutate_if(is.character,
                     stringr::str_replace_all,
                     pattern = c("B_max"),
                     replacement = c(as.character(B_max)))


  # export df with substituted captions and alt text to csv
 utils::write.csv(x = caps_alttext_subbed,
            file = file.path(dir,
                             "captions_alt_text.csv"),
            row.names=FALSE)

}
