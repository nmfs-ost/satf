#' Create Indices of Abundance Table
#'
#' @inheritParams plot_recruitment
#' @return Create table of observed annual indices of abundance plus error
#' stratified by fleet.
#' @export
#'
table_indices <- function(dat,
                          make_rda = FALSE,
                          rda_dir = getwd()) {
  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "indices_abun"


  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv"))
  ) {
    satf::write_captions(dat = dat,
                         dir = getwd(),
                         year = NULL)
  }

  # identify output
  fig_or_table <- "table"

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table)

  # Load data
  output <- dat |>
    dplyr::filter(module_name == "INDEX_2" | module_name == "t.series")
  # Check for U
  if (any(unique(output$module_name=="INDEX_2"))) {
    output2 <- output |>
      dplyr::filter(grepl("input_indices", label))
  } else if (any(unique(output$module_name=="t.series"))) {
    output <- output |>
      dplyr::filter(grepl("cpue", label))
  }
  # Extract fleet names
  fleet_names <- unique(output$fleet)
  factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo","growth_pattern", "gp")
  # re-structure df for table
  indices <- output |>
    tidyr::pivot_wider(
      id_cols = unique(output$label),
      names_from = fleet,
      values_from = estimate
    )
    # dplyr::rename(!!unique(output$label) := estimate,
    #               !!unique(output$uncertainty_label) := uncertainty) |>
    tidyr::pivot_wider(
      id_cols = -intersect(colnames(output), factors),
      names_from = fleet,
      values_from = c(unique(output$label), unique(output$uncertainty_label))
    ) # stated internal error for tidyr and asks to report - try again monday

  return(tab)
}
