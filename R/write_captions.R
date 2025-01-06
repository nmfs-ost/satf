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

  # extract key quantities
  # REMINDERS:
  # -the variable names must exactly match those in the captions/alt text csv.

  # suppress warnings
  options(warn=-1)

  # FIGURES-----

  ## kobe plot
  # B.BMSY.min <- # minimum value of B/B(MSY)
  # B.BMSY.max <- # maximum value of B/B(MSY)
  # F.FMSY.min <- # minimum value of F/F(MSY)
  # F.FMSY.max <- # maximum value of F/F(MSY)
  # B.BMSY.end.yr <- # value of B/B(MSY) at the end year
  # F.FMSY.end.yr <- # value of F/F(MSY) at the end year
  # overfished.status.is.isnot <- # object that should be "is" or "is not" and answers the question, "the stock overfishing status ... overfished"
  # overfishing.status.is.isnot <- # object that should be "is" or "is not" and answers the question, "the stock ... experiencing overfishing"
  # kobe.start.year <- # start year of kobe plot
  # kobe.end.year <- # end year of kobe plot

  ## Biomass plot
  # B.ref.pt <- # biomass reference point- SHARED with kobe plot, above
  # B.ref.pt.units <- # biomass reference point unit

  # start year of biomass plot
  B.start.year <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of biomass plot
  # B.end.year <- # this will take some thought, since end_year is
    # either entered by the user or calculated in plot_biomass.R

  # units of B (plural)
  # B.units <- # this will take some thought, since units is
    # entered by the user in plot_biomass.R

  # minimum B
  B.min <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # maximum B
  B.max <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # R0
  R0 <- dat |>
    dplyr::filter(grepl('R0', label) | grepl('recruitment_virgin', label)) |>
    dplyr::pull(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # Bend <-

  # Target biomass
  Btarg <- dat |>
    dplyr::filter(c(grepl('biomass', label) & grepl('target', label) & estimate >1) | label == 'biomass_msy') |>
    dplyr::pull(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # Bmsy <-


  ## relative B
  # relative B min
  rel.B.min <- (B.min / Btarg) |>
    round(digits = 2)

  # relative B max
  rel.B.max <- (B.max / Btarg) |>
    round(digits = 2)


  ## mortality (F) plot
  # F.ref.pt <- # F reference point
  # F.ref.pt.units <- # F reference point unit
  # F.start.year. <- # start year of F plot
  # F.end.year <- # start year of F plot
  # F.units <- # units of F (plural)
  # F.min <- # minimum F
  # F.max <- # maximum F
  # Ftarg <-
  # F.Ftarg <-

  ## landings plot

  # start year of landings plot
  landings.start.year <- dat |>
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
  landings.end.year <- dat |>
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
  # landings.units <- # this will take some thought, since units is
  # entered by the user in plot_landings.R

  # minimum landings
  landings.min <- dat |>
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
    as.numeric() |>
    round(digits = 2)

  # maximum landings
  landings.max <- dat |>
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
    as.numeric() |>
    round(digits = 2)

  ## natural mortality (M)
  # M.age.min <- # minimum age of M
  # M.age.max <- # maximum age of M
  # M.units <- # units of M (plural)
  # M.rate.min <- # minimum M rate
  # M.rate.max <- # maximum M rate

  ## vonB LAA (von Bertalanffy growth function + length at age)
  # vonb.age.units <- # vonB age units (plural)
  # vonb.age.min <- # minimum vonB age
  # vonb.age.max <- # maximum vonB age
  # vonb.length.units <- # vonB length units (plural)
  # vonb.length.min <- # minimum vonB length
  # vonb.length.max <- # minimum vonB length

  ## length-type conversion plot
  # total.length.units <- # total length units (plural)
  # total.length.min <- # minimum total length
  # total.length.max <- # maximum total length
  # fork.length.units <- # fork length units (plural)
  # fork.length.min <- # minimum fork length
  # fork.length.max <- # maximum fork length

  ## weight-length conversion plot
  # wl.length.units <- # length units (plural)
  # wl.length.min <- # minimum length
  # wl.length.max <- # maximum length
  # wl.weight.units <- # weight units (plural)
  # wl.weight.min <- # minimum weight
  # wl.weight.max <- # maximum weight

  ## maturity schedule (proportion mature)
  # prop.mat.length.units <- # length units (plural)
  # prop.mat.length.min <- # minimum length
  # prop.mat.length.max <- # maximum length

  ## fecundity at length
  # fecundity.length.units <- # length units (plural)
  # fecundity.length.min <- # minimum length
  # fecundity.length.max <- # maximum length
  # fecundity.units <- # fecundity units (plural)
  # fecundity.min <- # minimum fecundity
  # fecundity.max <- # maximum fecundity

  ## CAA (catch at age)
  # fleet.or.survey.name <- # fleet or survey name (SHARED with CAL, below)
  # caa.age.min <- # minimum age group
  # caa.age.max <- # maximum age group

  ## CAL (catch at length)
  # cal.length.min <- # minimum length group
  # cal.length.max <- # maximum length group

  ## CPUE indices plot
  # cpue.start.year <- # start year of CPUE indices plot
  # cpue.end.year <- # end year of CPUE indices plot
  # cpue.units <- # CPUE units (plural) (SHARED with mod.fit.abun, below)
  # cpue.min <- # minimum CPUE (SHARED with mod_fit_abun, below)
  # cpue.max <- # maximum CPUE (SHARED with mod_fit_abun, below)

  ## NAA (numbers at age)
  # bubble.start.year.min <- # start year of NAA plot
  # bubble.end.year.max <- # end year of NAA plot
  # bubble.age.units <- # age units (plural)
  # bubble.age.min <- # minimum age
  # bubble.age.max <- # maximum age

  ## mod_fit_catch (model fit to catch ts)
  # mod.fit.catch.start.year <- # start year of model fit to catch ts plot
  # mod.fit.catch.end.year <- # end year of model fit to catch ts plot
  # mod.fit.catch.units <- # catch units (plural)
  # mod.fit.catch.min <- # minimum catch
  # mod.fit.catch.max <- # maximum catch

  ## mod_fit_abun (model fit to abundance indices plot)
  # mod.fit.abun.start.year <- # start year of model fit to abundance indices plot
  # mod.fit.abun.end.year <- # end year of model fit to abundance indices plot

  ## mod_fit_discards
  # mod.fit.discards.start.year <- # start year of model fit to discards plot
  # mod.fit.discards.end.year <- # end year of model fit to discards plot
  # mod.fit.discards.units <- # discards units (plural)
  # mod.fit.discards.min <- # minimum discards
  # mod.fit.discards.max <- # maximum discards

  ## selectivity
  # selectivity.start.year <- # start year of selectivity plot
  # selectivity.end.year <- # end year of selectivity plot
  # selectivity.length.units <- # length units (plural)
  # selectivity.length.min <- # minimum length
  # selectivity.length.max <- # maximum length


  ## estimated stock recruitment (aka spawning stock biomass)
  # youngest-age recruited fish (instead of age-0)
  # sr.age.min <- # unsure how to extract this

  # ssb units (plural)
  # sr.ssb.units <- # this will take some thought, since
  # spawning_biomass_label is entered by the user in plot_spawn_recruitment.R

  # minimum ssb
  sr.ssb.min <- dat |>
  dplyr::filter(label == "spawning_biomass",
                module_name == "TIME_SERIES" | module_name == "t.series",
                !is.na(year),
                is.na(fleet) | length(unique(fleet)) <= 1,
                is.na(sex) | length(unique(sex)) <= 1,
                is.na(area) | length(unique(area)) <= 1,
                is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                !year %in% year_exclusions
  ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # maximum ssb
  sr.ssb.max <- dat |>
    dplyr::filter(label == "spawning_biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% year_exclusions
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # recruitment units (plural)
  # sr.units <- # this will take some thought, since recruitment_label
  # is entered by the user in plot_spawn_recruitment.R

  # minimum recruitment
  sr.min <- dat |>
  dplyr::filter(label == "recruitment",
                module_name == "TIME_SERIES" | module_name == "t.series",
                !is.na(year),
                is.na(fleet) | length(unique(fleet)) <= 1,
                is.na(sex) | length(unique(sex)) <= 1,
                is.na(area) | length(unique(area)) <= 1,
                is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                !year %in% year_exclusions
  ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # maximum recruitment
  sr.max <- dat |>
    dplyr::filter(label == "recruitment",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% year_exclusions
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)


  ## recruitment ts
  # recruitment units (plural) - numbers of fish, in thousands
  # recruitment.units <- # this will take some thought, since unit_label is
    # entered by the user in plot_recruitment.R

  # start year of recruitment ts plot
  recruitment.start.year <- dat |>
    dplyr::filter(label == "recruitment",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% year_exclusions
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of recruitment ts plot
  # recruitment.end.year <- # this will take some thought, since end_year is
    # either entered by the user or calculated in plot_recruitment.R

  # minimum recruitment
  recruitment.min <- sr.min

  # maximum recruitment
  recruitment.max <- sr.max


  ## relative recruitment
  # minimum relative recruitment
  rel.recruitment.min <- (recruitment.min / R0) |>
    round(digits = 2)

  # maximum relative recruitment
  rel.recruitment.max <- (recruitment.max / R0) |>
    round(digits = 2)


  ## recruitment deviations
  # start year of recruitment deviations plot
  recruit.dev.start.year <-  dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% year_exclusions
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()

  # end year of recruitment deviations plot
  # recruit.dev.end.year <- # this will take some thought, since end_year is
  # either entered by the user or calculated in plot_recruitment_deviations.R

  # minimum recruitment deviation
  recruit.dev.min <- dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% year_exclusions
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # maximum recruitment deviation
  recruit.dev.max <- dat |>
    dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                  module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  !year %in% year_exclusions
    ) |> # SS3 and BAM target module names
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  ## tot_b (total biomass)
  # biomass.start.year <- # start year of biomass plot
  # biomass.end.year <- # end year of biomass plot
  # biomass.units <- # biomass units (plural)
  # biomass.min <- # minimum biomass
  # biomass.max <- # maximum biomass
  # biomass.ref.pt <- # biomass reference point
  # biomass.ref.pt.units <- # biomass reference point units

  ## spawning_biomass (ssb)
  # start year of ssb plot
  ssb.start.year <- dat |>
  dplyr::filter(
    label == "spawning_biomass",
    module_name %in% c("DERIVED_QUANTITIES", "t.series")
  ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric() |>
    round(digits = 2)

  # end year of ssb plot
  # ssb.end.year <- # this will take some thought, since end_year is
  # either entered by the user or calculated in plot_spawning_biomass.R

  # ssb units (plural)
  # ssb.units <- # this will take some thought, since unit_label is
  # entered by the user in plot_spawning_biomass.R

  # minimum ssb
  ssb.min <- dat |>
    dplyr::filter(
      label == "spawning_biomass",
      module_name %in% c("DERIVED_QUANTITIES", "t.series")
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)) |>
    dplyr::slice(which.min(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # maximum ssb
  ssb.max <- dat |>
    dplyr::filter(
      label == "spawning_biomass",
      module_name %in% c("DERIVED_QUANTITIES", "t.series")
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)) |>
    dplyr::slice(which.max(estimate)) |>
    dplyr::select(estimate) |>
    as.numeric() |>
    round(digits = 2)

  # ssb reference point
  # ssb.ref.pt <- # this will take some thought, since ref_line_val is
  # calculated in plot_spawning_biomass.R

  # ssb reference point units
  # ssb.ref.pt.units <- ssb_units

  ssbtarg <- dat |>
    dplyr::filter(c(grepl('spawning_biomass', label) & grepl('msy$', label) & estimate >1) | label == 'spawning_biomass_msy$') |>
    dplyr::pull(estimate) |>
    as.numeric() |>
    round(digits = 2)

  ## relative ssb
  # relative ssb min
  rel.ssb.min <- (ssb.min / ssbtarg) |>
    round(digits = 2)

  # relative ssb max
  rel.ssb.max <- (ssb.max / ssbtarg) |>
    round(digits = 2)


  ## spr (spawning potential ratio)
  # spr.start.year <- # start year of spr plot
  # spr.end.year <- # end year of spr plot
  # spr.min <- # minimum spr
  # spr.max  <- # maximum spr
  # spr.ref.pt <- # spr reference point
  # spr.ref.pt.units <- # spr reference point units

  ## pop_naa_baa (population numbers at age and population biomass at age)
  # pop.naa.baa.start.year <- # start year of spr plot
  # pop.naa.baa.end.year <- # end year of spr plot
  # pop.naa.baa.fish.min <- # minimum number of fish
  # pop.naa.baa.fish.max <- # maximum number of fish

  ## proj_catch (projected catch)
  # proj.catch.units <- # projected catch units (plural)
  # proj.catch.start.year <- # start year of projected catch plot
  # proj.catch.end.year <- # end year of projected catch plot
  # proj.catch.min <- # minimum projected catch
  # proj.catch.max <- # maximum projected catch

  ## proj_biomass (projected biomass)
  # proj.biomass.units <- # projected biomass units (plural)
  # proj.biomass.start.year <- # start year of projected biomass plot
  # proj.biomass.end.year <- # end year of projected biomass plot
  # proj.biomass.min <- # minimum projected biomass
  # proj.biomass.max <- # maximum projected biomass
  # proj.biomass.ref.pt <- # projected biomass reference point
  # proj.biomass.ref.pt.units <- # projected biomass reference point units

  ## Other
  # tot.catch <-
  # M <-
  # steep <-
  # SBmsy <-
  # fSB <-

  # TABLES-----

  ## catch
  # catch.fleet <- # fleet

  ## landings
  # landings.tbl.units <- # landings units; remove if units already in table

  ## discards
  # discards.tbl.units <- # discards units

  ## catchability
  # catchability.fleet <- # fleet


  # add in more quantities here, and update the quantities above

  # substitute quantity placeholders in the captions/alt text with
  # the real values, extracted above


  # make list with all placeholders
  # uncomment placeholders once uncommented, above
  patterns_replacements <- c(
    # FIGURES-----

   ## kobe plot
   # 'B.BMSY.min' = as.character(B.BMSY.min),
   # 'B.BMSY.max' = as.character(B.BMSY.max),
   # 'F.FMSY.min' = as.character(F.FMSY.min),
   # 'F.FMSY.max' = as.character(F.FMSY.max),
   # 'B.BMSY.end.yr' = as.character(B.BMSY.end.yr),
   # 'F.FMSY.end.yr' = as.character(F.FMSY.end.yr),
   # 'overfished.status.is.isnot' = as.character(overfished.status.is.isnot),
   # 'overfishing.status.is.isnot' = as.character(overfishing.status.is.isnot),
   # 'kobe.start.year' = as.character(kobe.start.year),
   # 'kobe.end.year' = as.character(kobe.end.year),

   ## Relative biomass plot
   # NOTE: moving this above biomass so rel.B.min isn't changed to "rel." + B.min (etc.)
   'rel.B.min' = as.character(rel.B.min),
   'rel.B.max' = as.character(rel.B.max),

   ## Biomass plot
   # 'B.ref.pt' = as.character(B.ref.pt),
   # 'B.ref.pt.units' = as.character(B.ref.pt.units),
   'B.start.year' = as.character(B.start.year),
   # 'B.end.year' = as.character(B.end.year),
   # 'B.units' = as.character(B.units),
   'B.min' = as.character(B.min),
   'B.max' = as.character(B.max),
   'R0' = as.character(R0),
   # 'Bend' = as.character(Bend),
   'Btarg' = as.character(Btarg),
   # 'Bmsy' = as.character(Bmsy),

    ## mortality (F) plot
   # 'F.ref.pt' = as.character(F.ref.pt),
   # 'F.ref.pt.units' = as.character(F.ref.pt.units),
   # 'F.start.year' = as.character(F.start.year),
   # 'F.end.year' = as.character(F.end.year),
   # 'F.units' = as.character(F.units),
   # 'F.min' = as.character(F.min),
   # 'F.max' = as.character(F.max),
   # 'Ftarg' = as.character(Ftarg),
   # 'F.Ftarg' = as.character(F.Ftarg),

    ## landings plot
   'landings.start.year' = as.character(landings.start.year),
   'landings.end.year' = as.character(landings.end.year),
   # 'landings.units' = as.character(landings.units),
   'landings.min' = as.character(landings.min),
   'landings.max' = as.character(landings.max),

   ## natural mortality (M)
   # 'M.age.min' = as.character(M.age.min),
   # 'M.age.max' = as.character(M.age.max),
   # 'M.units' = as.character(M.units),
   # 'M.rate.min' = as.character(M.rate.min),
   # 'M.rate.max' = as.character(M.rate.max),

   ## vonB LAA (von Bertalanffy growth function + length at age)
   # 'vonb.age.units' = as.character(vonb.age.units),
   # 'vonb.age.min' = as.character(vonb.age.min),
   # 'vonb.age.max' = as.character(vonb.age.max),
   # 'vonb.length.units' = as.character(vonb.length.units),
   # 'vonb.length.min' = as.character(vonb.length.min),
   # 'vonb.length.max' = as.character(vonb.length.max),

    ## length-type conversion plot
   # 'total.length.units' = as.character(total.length.units),
   # 'total.length.min' = as.character(total.length.min),
   # 'total.length.max' = as.character(total.length.max),
   # 'fork.length.units' = as.character(fork.length.units),
   # 'fork.length.min' = as.character(fork.length.min),
   # 'fork.length.max' = as.character(fork.length.max),

   ## weight-length conversion plot
   # 'wl.length.units' = as.character(wl.length.units),
   # 'wl.length.min' = as.character(wl.length.min),
   # 'wl.length.max' = as.character(wl.length.max),
   # 'wl.weight.units' = as.character(wl.weight.units),
   # 'wl.weight.min' = as.character(wl.weight.min),
   # 'wl.weight.max' = as.character(wl.weight.max),

   ## maturity schedule (proportion mature)
   # 'prop.mat.length.units' = as.character(prop.mat.length.units),
   # 'prop.mat.length.min' = as.character(prop.mat.length.min),
   # 'prop.mat.length.max' = as.character(prop.mat.length.max),

   ## fecundity at length
   # 'fecundity.length.units' = as.character(fecundity.length.units),
   # 'fecundity.length.min' = as.character(fecundity.length.min),
   # 'fecundity.length.max' = as.character(fecundity.length.max),
   # 'fecundity.units' = as.character(fecundity.units),
   # 'fecundity.min' = as.character(fecundity.min),
   # 'fecundity.max' = as.character(fecundity.max),

   ## CAA (catch at age)
   # 'fleet.or.survey.name' = as.character(fleet.or.survey.name),
   # 'caa.age.min' = as.character(caa.age.min),
   # 'caa.age.max' = as.character(caa.age.max),

   ## CAL (catch at length)
   # 'cal.length.min' = as.character(cal.length.min),
   # 'cal.length.max' = as.character(cal.length.max),

   ## CPUE indices plot
   # 'cpue.start.year' = as.character(cpue.start.year),
   # 'cpue.end.year' = as.character(cpue.end.year),
   # 'cpue.units' = as.character(cpue.units),
   # 'cpue.min' = as.character(cpue.min),
   # 'cpue.max' = as.character(cpue.max),

   ## NAA (numbers at age)
   # 'bubble.start.year.min' = as.character(bubble.start.year.min),
   # 'bubble.end.year.max' = as.character(bubble.end.year.max),
   # 'bubble.age.units' = as.character(bubble.age.units),
   # 'bubble.age.min' = as.character(bubble.age.min),
   # 'bubble.age.max' = as.character(bubble.age.max),

   ## mod_fit_catch (model fit to catch ts)
   # 'mod.fit.catch.start.year' = as.character(mod.fit.catch.start.year),
   # 'mod.fit.catch.end.year' = as.character(mod.fit.catch.end.year),
   # 'mod.fit.catch.units' = as.character(mod.fit.catch.units),
   # 'mod.fit.catch.min' = as.character(mod.fit.catch.min),
   # 'mod.fit.catch.max' = as.character(mod.fit.catch.max),

   ## mod_fit_abun (model fit to abundance indices plot)
   # 'mod.fit.abun.start.year' = as.character(mod.fit.abun.start.year),
   # 'mod.fit.abun.end.year' = as.character(mod.fit.abun.end.year),

   ## mod_fit_discards
   # 'mod.fit.discards.start.year' = as.character(mod.fit.discards.start.year),
   # 'mod.fit.discards.end.year' = as.character(mod.fit.discards.end.year),
   # 'mod.fit.discards.units' = as.character(mod.fit.discards.units),
   # 'mod.fit.discards.min' = as.character(mod.fit.discards.min),
   # 'mod.fit.discards.max' = as.character(mod.fit.discards.max),

   ## selectivity
   # 'selectivity.start.year' = as.character(selectivity.start.year),
   # 'selectivity.end.year' = as.character(selectivity.end.year),
   # 'selectivity.length.units' = as.character(selectivity.length.units),
   # 'selectivity.length.min' = as.character(selectivity.length.min),
   # 'selectivity.length.max' = as.character(selectivity.length.max),

   ## estimated stock recruitment (aka spawning stock biomass)
   # 'sr.age.min' = as.character(sr.age.min),
   # 'sr.ssb.units' = as.character(sr.ssb.units),
   'sr.ssb.min' = as.character(sr.ssb.min),
   'sr.ssb.max' = as.character(sr.ssb.max),
   # 'sr.units' = as.character(sr.units),
   'sr.min' = as.character(sr.min),
   'sr.max' = as.character(sr.max),

   # relative recruitment ts
   # NOTE: moving this above recruitment so rel.recruitment.min isn't changed
   # to "rel." + recruitment.min (etc.)
   'rel.recruitment.min' = as.character(rel.recruitment.min),
   'rel.recruitment.max' = as.character(rel.recruitment.max),

   ## recruitment ts
   # 'recruitment.units' = as.character(recruitment.units),
   'recruitment.start.year' = as.character(recruitment.start.year),
   # 'recruitment.end.year' = as.character(recruitment.end.year),
   'recruitment.min' = as.character(recruitment.min),
   'recruitment.max' = as.character(recruitment.max),

   ## recruitment deviations
   'recruit.dev.start.year' = as.character(recruit.dev.start.year),
   # 'recruit.dev.end.year' = as.character(recruit.dev.end.year),
   'recruit.dev.min' = as.character(recruit.dev.min),
   'recruit.dev.max' = as.character(recruit.dev.max),

   ## tot_b (total biomass)
   # 'biomass.start.year' = as.character(biomass.start.year),
   # 'biomass.end.year' = as.character(biomass.end.year),
   # 'biomass.units' = as.character(biomass.units),
   # 'biomass.min' = as.character(biomass.min),
   # 'biomass.max' = as.character(biomass.max),
   # 'biomass.ref.pt' = as.character(biomass.ref.pt),
   # 'biomass.ref.pt.units' = as.character(biomass.ref.pt.units),

   # relative ssb
   # NOTE: moving this above recruitment so rel.ssb.min isn't changed to
   # "rel." + ssb.min), etc.
   'rel.ssb.min' = as.character(rel.ssb.min),
   'rel.ssb.max' = as.character(rel.ssb.max),

   ## spawning.biomass (ssb)
   'ssb.start.year' = as.character(ssb.start.year),
   # 'ssb.end.year' = as.character(ssb.end.year),
   # 'ssb.units' = as.character(ssb.units),
   'ssb.min' = as.character(ssb.min),
   'ssb.max' = as.character(ssb.max)#,
   # 'ssb.ref.pt' = as.character(ssb.ref.pt),
   # 'ssb.ref.pt.units' = as.character(ssb.ref.pt.units),
   # 'ssbtarg' = as.character(ssbtarg),


   # ## spr (spawning potential ratio)
   # 'spr.start.year' = as.character(spr.start.year),
   # 'spr.end.year' = as.character(spr.end.year),
   # 'spr.min' = as.character(spr.min),
   # 'spr.max' = as.character(spr.max),
   # 'spr.ref.pt' = as.character(spr.ref.pt),
   # 'spr.ref.pt.units' = as.character(spr.ref.pt.units),
   #
   # ## pop_naa_baa (population numbers at age and population biomass at age)
   # 'pop.naa.baa.start.year' = as.character(pop.naa.baa.start.year),
   # 'pop.naa.baa.end.year' = as.character(pop.naa.baa.end.year),
   # 'pop.naa.baa.fish.min' = as.character(pop.naa.baa.fish.min),
   # 'pop.naa.baa.fish.max' = as.character(pop.naa.baa.fish.max),
   #
   # ## proj_catch (projected catch)
   # 'proj.catch.units' = as.character(proj.catch.units),
   # 'proj.catch.start.year' = as.character(proj.catch.start.year),
   # 'proj.catch.end.year' = as.character(proj.catch.end.year),
   # 'proj.catch.min' = as.character(proj.catch.min),
   # 'proj.catch.max' = as.character(proj.catch.max),
   #
   # ## proj_biomass (projected biomass)
   # 'proj.biomass.units' = as.character(proj.biomass.units),
   # 'proj.biomass.start.year' = as.character(proj.biomass.start.year),
   # 'proj.biomass.end.year' = as.character(proj.biomass.end.year),
   # 'proj.biomass.min' = as.character(proj.biomass.min),
   # 'proj.biomass.max' = as.character(proj.biomass.max),
   # 'proj.biomass.ref.pt' = as.character(proj.biomass.ref.pt),
   # 'proj.biomass.ref.pt.units' = as.character(proj.biomass.ref.pt.units),
   #
   # ## Other
   # 'tot.catch' = as.character(tot.catch),
   # 'M' = as.character(M),
   # 'steep' = as.character(steep),
   # 'SBmsy' = as.character(SBmsy),
   # 'fSB' = as.character(fSB),
   # 'sbtarg' = as.character(sbtarg),
   #
   # # TABLES-----
   #
   # ## catch
   # 'catch.fleet' = as.character(catch.fleet),
   #
   # ## landings
   # 'landings.tbl.units' = as.character(landings.tbl.units),
   #
   # ## discards
   # 'discards.tbl.units' = as.character(discards.tbl.units),
   #
   # ## catchability
   # 'catchability.fleet' = as.character(catchability.fleet)
  )

  # take the values associated with the quantities and replace the df's
  # placeholders with them. For example, if ssb_min = 10, this will replace
  # "the minimum ssb = ssb_min" with "the minimum ssb = 10".

  # replace values in caption column
  caps_alttext$caption <- stringr::str_replace_all(
    caps_alttext$caption,
    patterns_replacements
  )

  # replace values in alt text column
  caps_alttext$alt_text <- stringr::str_replace_all(
    caps_alttext$alt_text,
    patterns_replacements
  )

  # export df with updated captions and alt text to csv
 utils::write.csv(x = caps_alttext,
            file = fs::path(dir,
                             "captions_alt_text.csv"),
            row.names=FALSE)



 # enable warnings again
 options(warn=0)

}

