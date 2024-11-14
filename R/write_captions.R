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
    system.file("resources", "captions_alttext.csv", package = "satf")
  )

  # extract key quantities (these are examples and are not accurate)
  # REMINDER: the variable names must exactly match those in the captions/alt text csv.
  ## quantities applicable to multiple plots
  start_year <- min(as.numeric(dat$year[dat$year!="S/Rcurve" | dat$year!="Virg" | dat$year!="Init"]), na.rm = TRUE)

  # end_year


  ## quantities for individual plots
  ### kobe plot
  # B_div_BMSY_min <- # (= minimum value of B/B(MSY))
  # B_div_BMSY_max <- # (= maximum value of B/B(MSY))
  # F_div_FMSY_min <- # (= minimum value of F/F(MSY))
  # F_div_FMSY_max <- # (= maximum value of F/F(MSY))
  # B_div_BMSY_end_yr <- # (= value of B/B(MSY) at the end year)
  # F_div_FMSY_end_yr <- # (= value of F/F(MSY) at the end year)
  # overfished_status_is_isnot <- # object that should be "is" or "is not" and answers the question, "the stock overfishing status ___ overfished"
  # overfishing_status_is_isnot <- # object that should be "is" or "is not" and answers the question, "the stock ___ experiencing overfishing"

  ### F
  Fend_df <- dat |>
    dplyr::filter(label == "fishing_mortality" & year == year | label == "F_terminal")
  Fend <- as.numeric(Fend_df$estimate)

  # Ftarg <-
  # F_Ftarg <-

  ### B
  # Bend <-
  # Btarg <-
  # Bmsy <-


  ### SB
  # SBmsy <-
  # fSB <-
  # sbtarg <-

  ### Other
  # tot_catch <-
  # M <-
  # steep <-
  # R0 <-




  # add in more quantities here, and update the quantities above

  # substitute quantity placeholders in the captions/alt text with
  # the real values, extracted above
  caps_alttext_subbed <- caps_alttext |>
    dplyr::mutate_if(is.character,
                   stringr::str_replace_all,
                   pattern = c("Fend"),
                   replacement = c(as.character(Fend)))|>
    dplyr::mutate_if(is.character,
                     stringr::str_replace_all,
                     pattern = c("start_year"),
                     replacement = c(as.character(start_year)))


  # export df with substituted captions and alt text to csv
  write.csv(x = caps_alttext_subbed,
            file = file.path(dir,
                             "captions_alt_text.csv"),
            row.names=FALSE)

}
