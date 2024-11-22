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
  start_year <- min(as.numeric(dat$year[dat$year!="S/Rcurve" | dat$year!="Virg" | dat$year!="Init"]), na.rm = TRUE)
                # as.numeric(dat[3,3])
  Fend_df <- dat |>
    dplyr::filter(label == "fishing_mortality" & year == year | label == "F_terminal")
  Fend <- as.numeric(Fend_df$estimate)
          # as.numeric(dat$estimate[2])
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
 utils:: write.csv(x = caps_alttext_subbed,
            file = file.path(dir,
                             "captions_alt_text.csv"),
            row.names=FALSE)

}
