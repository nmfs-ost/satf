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
  patterns_replacements <- list(
    # FIGURES-----

   ## kobe plot
   # 'B.BMSY.min' = B.BMSY.min,
   # 'B.BMSY.max' = B.BMSY.max,
   # 'F.FMSY.min' = F.FMSY.min,
   # 'F.FMSY.max' = F.FMSY.max,
   # 'B.BMSY.end.yr' = B.BMSY.end.yr,
   # 'F.FMSY.end.yr' = F.FMSY.end.yr,
   # 'overfished.status.is.isnot' = overfished.status.is.isnot,
   # 'overfishing.status.is.isnot' = overfishing.status.is.isnot,
   # 'kobe.start.year' = kobe.start.year,
   # 'kobe.end.year' = kobe.end.year,

   ## Relative biomass plot
   # NOTE: moving this above biomass so rel.B.min isn't changed to "rel." + B.min (etc.)
   'rel.B.min' = rel.B.min,
   'rel.B.max' = rel.B.max,

   ## Biomass plot
   # 'B.ref.pt' = B.ref.pt,
   # 'B.ref.pt.units' = B.ref.pt.units,
   'B.start.year' = B.start.year,
   # 'B.end.year' = B.end.year,
   # 'B.units' = B.units,
   'B.min' = B.min,
   'B.max' = B.max,
   'R0' = R0,
   # 'Bend' = Bend,
   'Btarg' = Btarg,
   # 'Bmsy' = Bmsy,

    ## mortality (F) plot
   # 'F.ref.pt' = F.ref.pt,
   # 'F.ref.pt.units' = F.ref.pt.units,
   # 'F.start.year' = F.start.year,
   # 'F.end.year' = F.end.year,
   # 'F.units' = F.units,
   # 'F.min' = F.min,
   # 'F.max' = F.max,
   # 'Ftarg' = Ftarg,
   # 'F.Ftarg' = F.Ftarg,

    ## landings plot
   'landings.start.year' = landings.start.year,
   'landings.end.year' = landings.end.year,
   # 'landings.units' = landings.units,
   'landings.min' = landings.min,
   'landings.max' = landings.max,

   ## natural mortality (M)
   # 'M.age.min' = M.age.min,
   # 'M.age.max' = M.age.max,
   # 'M.units' = M.units,
   # 'M.rate.min' = M.rate.min,
   # 'M.rate.max' = M.rate.max,

   ## vonB LAA (von Bertalanffy growth function + length at age)
   # 'vonb.age.units' = vonb.age.units,
   # 'vonb.age.min' = vonb.age.min,
   # 'vonb.age.max' = vonb.age.max,
   # 'vonb.length.units' = vonb.length.units,
   # 'vonb.length.min' = vonb.length.min,
   # 'vonb.length.max' = vonb.length.max,

    ## length-type conversion plot
   # 'total.length.units' = total.length.units,
   # 'total.length.min' = total.length.min,
   # 'total.length.max' = total.length.max,
   # 'fork.length.units' = fork.length.units,
   # 'fork.length.min' = fork.length.min,
   # 'fork.length.max' = fork.length.max,

   ## weight-length conversion plot
   # 'wl.length.units' = wl.length.units,
   # 'wl.length.min' = wl.length.min,
   # 'wl.length.max' = wl.length.max,
   # 'wl.weight.units' = wl.weight.units,
   # 'wl.weight.min' = wl.weight.min,
   # 'wl.weight.max' = wl.weight.max,

   ## maturity schedule (proportion mature)
   # 'prop.mat.length.units' = prop.mat.length.units,
   # 'prop.mat.length.min' = prop.mat.length.min,
   # 'prop.mat.length.max' = prop.mat.length.max,

   ## fecundity at length
   # 'fecundity.length.units' = fecundity.length.units,
   # 'fecundity.length.min' = fecundity.length.min,
   # 'fecundity.length.max' = fecundity.length.max,
   # 'fecundity.units' = fecundity.units,
   # 'fecundity.min' = fecundity.min,
   # 'fecundity.max' = fecundity.max,

   ## CAA (catch at age)
   # 'fleet.or.survey.name' = fleet.or.survey.name,
   # 'caa.age.min' = caa.age.min,
   # 'caa.age.max' = caa.age.max,

   ## CAL (catch at length)
   # 'cal.length.min' = cal.length.min,
   # 'cal.length.max' = cal.length.max,

   ## CPUE indices plot
   # 'cpue.start.year' = cpue.start.year,
   # 'cpue.end.year' = cpue.end.year,
   # 'cpue.units' = cpue.units,
   # 'cpue.min' = cpue.min,
   # 'cpue.max' = cpue.max,

   ## NAA (numbers at age)
   # 'bubble.start.year.min' = bubble.start.year.min,
   # 'bubble.end.year.max' = bubble.end.year.max,
   # 'bubble.age.units' = bubble.age.units,
   # 'bubble.age.min' = bubble.age.min,
   # 'bubble.age.max' = bubble.age.max,

   ## mod_fit_catch (model fit to catch ts)
   # 'mod.fit.catch.start.year' = mod.fit.catch.start.year,
   # 'mod.fit.catch.end.year' = mod.fit.catch.end.year,
   # 'mod.fit.catch.units' = mod.fit.catch.units,
   # 'mod.fit.catch.min' = mod.fit.catch.min,
   # 'mod.fit.catch.max' = mod.fit.catch.max,

   ## mod_fit_abun (model fit to abundance indices plot)
   # 'mod.fit.abun.start.year' = mod.fit.abun.start.year,
   # 'mod.fit.abun.end.year' = mod.fit.abun.end.year,

   ## mod_fit_discards
   # 'mod.fit.discards.start.year' = mod.fit.discards.start.year,
   # 'mod.fit.discards.end.year' = mod.fit.discards.end.year,
   # 'mod.fit.discards.units' = mod.fit.discards.units,
   # 'mod.fit.discards.min' = mod.fit.discards.min,
   # 'mod.fit.discards.max' = mod.fit.discards.max,

   ## selectivity
   # 'selectivity.start.year' = selectivity.start.year,
   # 'selectivity.end.year' = selectivity.end.year,
   # 'selectivity.length.units' = selectivity.length.units,
   # 'selectivity.length.min' = selectivity.length.min,
   # 'selectivity.length.max' = selectivity.length.max,

   ## estimated stock recruitment (aka spawning stock biomass)
   # 'sr.age.min' = sr.age.min,
   # 'sr.ssb.units' = sr.ssb.units,
   'sr.ssb.min' = sr.ssb.min,
   'sr.ssb.max' = sr.ssb.max,
   # 'sr.units' = sr.units,
   'sr.min' = sr.min,
   'sr.max' = sr.max,

   # relative recruitment ts
   # NOTE: moving this above recruitment so rel.recruitment.min isn't changed
   # to "rel." + recruitment.min (etc.)
   'rel.recruitment.min' = rel.recruitment.min,
   'rel.recruitment.max' = rel.recruitment.max,

   ## recruitment ts
   # 'recruitment.units' = recruitment.units,
   'recruitment.start.year' = recruitment.start.year,
   # 'recruitment.end.year' = recruitment.end.year,
   'recruitment.min' = recruitment.min,
   'recruitment.max' = recruitment.max,

   ## recruitment deviations
   'recruit.dev.start.year' = recruit.dev.start.year,
   # 'recruit.dev.end.year' = recruit.dev.end.year,
   'recruit.dev.min' = recruit.dev.min,
   'recruit.dev.max' = recruit.dev.max,

   ## tot_b (total biomass)
   # 'biomass.start.year' = biomass.start.year,
   # 'biomass.end.year' = biomass.end.year,
   # 'biomass.units' = biomass.units,
   # 'biomass.min' = biomass.min,
   # 'biomass.max' = biomass.max,
   # 'biomass.ref.pt' = biomass.ref.pt,
   # 'biomass.ref.pt.units' = biomass.ref.pt.units,

   # relative ssb
   # NOTE: moving this above recruitment so rel.ssb.min isn't changed to
   # "rel." + ssb.min, etc.
   'rel.ssb.min' = rel.ssb.min,
   'rel.ssb.max' = rel.ssb.max,

   ## spawning.biomass (ssb)
   'ssb.start.year' = ssb.start.year,
   # 'ssb.end.year' = ssb.end.year,
   # 'ssb.units' = ssb.units,
   'ssb.min' = ssb.min,
   'ssb.max' = ssb.max#,
   # 'ssb.ref.pt' = ssb.ref.pt,
   # 'ssb.ref.pt.units' = ssb.ref.pt.units,
   # 'ssbtarg' = ssbtarg,


   # ## spr (spawning potential ratio)
   # 'spr.start.year' = spr.start.year,
   # 'spr.end.year' = spr.end.year,
   # 'spr.min' = spr.min,
   # 'spr.max' = spr.max,
   # 'spr.ref.pt' = spr.ref.pt,
   # 'spr.ref.pt.units' = spr.ref.pt.units,
   #
   # ## pop_naa_baa (population numbers at age and population biomass at age)
   # 'pop.naa.baa.start.year' = pop.naa.baa.start.year,
   # 'pop.naa.baa.end.year' = pop.naa.baa.end.year,
   # 'pop.naa.baa.fish.min' = pop.naa.baa.fish.min,
   # 'pop.naa.baa.fish.max' = pop.naa.baa.fish.max,
   #
   # ## proj_catch (projected catch)
   # 'proj.catch.units' = proj.catch.units,
   # 'proj.catch.start.year' = proj.catch.start.year,
   # 'proj.catch.end.year' = proj.catch.end.year,
   # 'proj.catch.min' = proj.catch.min,
   # 'proj.catch.max' = proj.catch.max,
   #
   # ## proj_biomass (projected biomass)
   # 'proj.biomass.units' = proj.biomass.units,
   # 'proj.biomass.start.year' = proj.biomass.start.year,
   # 'proj.biomass.end.year' = proj.biomass.end.year,
   # 'proj.biomass.min' = proj.biomass.min,
   # 'proj.biomass.max' = proj.biomass.max,
   # 'proj.biomass.ref.pt' = proj.biomass.ref.pt,
   # 'proj.biomass.ref.pt.units' = proj.biomass.ref.pt.units,
   #
   # ## Other
   # 'tot.catch' = tot.catch,
   # 'M' = M,
   # 'steep' = steep,
   # 'SBmsy' = SBmsy,
   # 'fSB' = fSB,
   # 'sbtarg' = sbtarg,
   #
   # # TABLES-----
   #
   # ## catch
   # 'catch.fleet' = catch.fleet,
   #
   # ## landings
   # 'landings.tbl.units' = landings.tbl.units,
   #
   # ## discards
   # 'discards.tbl.units' = discards.tbl.units,
   #
   # ## catchability
   # 'catchability.fleet' = catchability.fleet
  )

  # take the values associated with the quantities and replace the df's
  # placeholders with them. For example, if ssb_min = 10, this will replace
  # "the minimum ssb = ssb_min" with "the minimum ssb = 10".
  caps_alttext_subbed <- caps_alttext |>
    dplyr::mutate(across(where(is.character), ~{
      for (pattern in names(patterns_replacements)){
        replacement_value <- patterns_replacements[[pattern]]
        . <- stringr::str_replace_all(., pattern, as.character(replacement_value))
      }
      .
    }))

  # export df with updated captions and alt text to csv
 utils::write.csv(x = caps_alttext_subbed,
            file = fs::path(dir,
                             "captions_alt_text.csv"),
            row.names=FALSE)

}

