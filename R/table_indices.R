#' Create Indices of Abundance Table
#'
#' @inheritParams plot_recruitment
#' @return Create table of observed annual indices of abundance plus error
#' stratified by fleet.
#' @export
#'

table_indices <- function(dat) {
  output <- dat
  output <- output |>
    dplyr::filter(module_name == "INDEX_2" | module_name == "t.series")
  if (any(unique(output$module_name=="INDEX_2"))) {
    output <- output |>
      dplyr::filter(grepl("obs", label))
  } else if (any(unique(output$module_name=="t.series"))) {
    output <- output |>
      dplyr::filter(grepl("cpue", label))
  }
  fleet_names <- unique(output$fleet)
  factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo","growth_pattern", "gp")
  # re-structure df for table
  indices <- output |>
    dplyr::rename(!!unique(output$label) := estimate,
                  !!unique(output$uncertainty_label) := uncertainty) |>
    tidyr::pivot_wider(
      id_cols = -intersect(colnames(output), factors),
      names_from = fleet,
      values_from = c(unique(output$label), unique(output$uncertainty_label))
    ) # stated internal error for tidyr and asks to report - try again monday

  return(tab)
}
