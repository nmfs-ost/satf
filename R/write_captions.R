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
  # B_div_BMSY_min <- # minimum value of B/B(MSY)
  # B_div_BMSY_max <- # maximum value of B/B(MSY)
  # F_div_FMSY_min <- # minimum value of F/F(MSY)
  # F_div_FMSY_max <- # maximum value of F/F(MSY)
  # B_div_BMSY_end_yr <- # value of B/B(MSY) at the end year
  # F_div_FMSY_end_yr <- # value of F/F(MSY) at the end year
  # overfished_status_is_isnot <- # object that should be "is" or "is not" and answers the question, "the stock overfishing status ___ overfished"
  # overfishing_status_is_isnot <- # object that should be "is" or "is not" and answers the question, "the stock ___ experiencing overfishing"
  # kobe_start_year <- # start year of kobe plot
  # kobe_end_year <- # end year of kobe plot

  ## Biomass plot
  # B_ref_pt <- # biomass reference point
  # B_ref_pt_unit <- # biomass reference point unit
  # B_start_year_ <- # start year of biomass plot
  # B_end_year <- # start year of biomass plot
  # B_units <- # units of B (plural)
  # B_min <- # minimum B
  # B_max <- # maximum B
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
  Fend_df <- dat |>
    dplyr::filter(label == "fishing_mortality" & year == year | label == "F_terminal")

  # NOTE: SB added the [1] so one number would be assigned to Fend; otherwise,
  # this breaks because Fend = hundreds of numbers
  Fend <- as.numeric(Fend_df$estimate)[1]
  # Ftarg <-
  # F_Ftarg <-

  ## landings plot
  # landings_start_year <- # start year of landings plot
  # landings_end_year <- # end year of landings plot
  # landings_units <- # units of landings (plural)
  # landings_min <- # minimum landings
  # landings_max <- # maximum landings

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

  ## estimated stock recruitment
  # est_stock_recruitment_age_min <- # youngest-age recruited fish (instead of age-0)
  # est_stock_recruitment_ssb_units <- # ssb units (plural)
  # est_stock_recruitment_ssb_min <- # minimum ssb
  # est_stock_recruitment_ssb_max <- # maximum ssb
  # est_stock_recruitment_units <- # recruitment units (plural)
  # est_stock_recruitment_min <- # minimum recruitment
  # est_stock_recruitment_max <- # maximum recruitment

  ## recruitment ts
  # recruitment_units <- # recruitment units (plural) - numbers of fish, in thousands
  # recruitment_start_year <- # start year of recruitment ts plot
  # recruitment_end_year <- # end year of recruitment ts plot
  # recruitment_min <- # minimum recruitment
  # recruitment_max <- # maximum recruitment

  ## recruitment deviations
  # recruit_dev_start_year <- # start year of recruitment deviations plot
  # recruit_dev_end_year <- # end year of recruitment deviations plot
  # recruit_dev_min <- # minimum recruitment deviation
  # recruit_dev_max <- # maximum recruitment deviation

  ## tot_b (total biomass)
  # biomass_start_year <- # start year of biomass plot
  # biomass_end_year <- # end year of biomass plot
  # biomass_units <- # biomass units (plural)
  # biomass_min <- # minimum biomass
  # biomass_max <- # maximum biomass
  # biomass_ref_pt <- # biomass reference point
  # biomass_ref_pt_units <- # biomass reference point units

  ## spawning_biomass (ssb)
  # ssb_start_year <- # start year of ssb plot
  # ssb_end_year <- # end year of ssb plot
  # ssb_units <- # ssb units (plural)
  # ssb_min <- # minimum ssb
  # ssb_max <- # maximum ssb
  # ssb_ref_pt <- # ssb reference point
  # ssb_ref_pt_units <- # ssb reference point units

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


  ## catchability
  #

  # add in more quantities here, and update the quantities above

  # substitute quantity placeholders in the captions/alt text with
  # the real values, extracted above
  caps_alttext_subbed <- caps_alttext |>
    dplyr::mutate_if(is.character,
                   stringr::str_replace_all,
                   pattern = c("Fend"),
                   replacement = c(as.character(Fend)))#|>
    # dplyr::mutate_if(is.character,
    #                  stringr::str_replace_all,
    #                  pattern = c("start_year"),
    #                  replacement = c(as.character(start_year)))


  # export df with substituted captions and alt text to csv
  utils::write.csv(x = caps_alttext_subbed,
            file = file.path(dir,
                             "captions_alt_text.csv"),
            row.names=FALSE)

}
