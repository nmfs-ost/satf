#' #' Export all figures and tables
#' #'
#' #' Export all figures in tables within one function.
#' #'
#' #' @inheritParams plot_recruitment
#' #' @inheritParams plot_total_biomass
#' #' @inheritParams plot_landings
#' #' @inheritParams plot_spawning_biomass
#' #' @inheritParams table_bnc
#' #' @inheritParams table_indices
#' #' @inheritParams table_harvest_projection
#' #' @inheritParams table_afsc_tier
#' #'
#' #' @return
#' #' Rda files for each figure/table.
#' #' @export
#' #'
#' exp_all_figs_tables <- function(
#'     # arguments within plot_recruitment
#'     dat,
#'     params = FALSE,
#'     params_only = FALSE,
#'     units = c(sb = "metric tons", recruitment = "metric tons"), # ALSO IN landings
#'     recruitment_units = "metric tons",
#'     spawning_biomass_units = "metric tons",
#'     scaled = FALSE,
#'     scale_amount = NULL,
#'     show_warnings = FALSE,
#'     end_year = NULL,
#'     return = "recruitment",
#'     make_rda = FALSE,
#'     rda_dir = getwd(),
#'     # arguments specific to plot_biomass
#'     show_warnings = FALSE,
#'     units = NULL,
#'     scaled = FALSE,
#'     scale_amount = 1000,
#'     ref_line = c("target", "MSY", "msy", "unfished"), # ALSO IN SB
#'     end_year = NULL,
#'     relative = FALSE # ALSO IN SB
#' ) {
#'
#'
#' }
#'
