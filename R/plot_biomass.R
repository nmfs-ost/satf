plot_biomass <- function(dat,
                         model,
                         refpts = FALSE) {

  bio_info <- SS3_extract_df(dat, "DERIVED_QUANTITIES")[-c(1:4),]
  colnames(bio_info) <- bio_info[1,]
  bio_info <- bio_info[-1,] |>
    tidyr::separate_wider_delim(cols = LABEL, delim = "_", names = c("label", "year"))

}
