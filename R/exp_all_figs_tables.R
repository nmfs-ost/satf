#' Export all figures and tables
#'
#' Export all figures in tables within one function.
#'
#' @inheritParams plot_recruitment
#' @param ref_line @inheritParams plot_biomass
#' @param spawning_biomass_label @inheritParams plot_spawn_recruitment
#' @param recruitment_label @inheritParams plot_spawn_recruitment
#' @param ref_line_sb Identical definition as `ref_line`, but this argument is
#' applied to plot_spawning_biomass.
#'
#' @return
#' Rda files for each figure/table.
#'
#' @export
#'
exp_all_figs_tables <- function(

  # imported from plot_recruitment
  dat,
  unit_label = "metric tons",
  scale_amount = 1,
  end_year = NULL,
  n_projected_years = 10,
  relative = FALSE,
  make_rda = FALSE,
  rda_dir = getwd(),

  # imported from plot_biomass
  ref_line = c("target", "MSY", "msy", "unfished"),

  # imported from plot_landings- zero unique arguments

  # imported from plot_recruitment_deviations- zero unique arguments

  # imported from plot_spawn_recruitment
  spawning_biomass_label = "metric tons",
  recruitment_label = "metric tons",

  # imported from plot_spawning_biomass
  # changed ref_line to ref_line_sb to differentiate from plot_biomass arg
  ref_line_sb = c("target", "MSY", "msy", "unfished")

) {


  satf::plot_recruitment(dat,
                         unit_label,
                         scale_amount,
                         end_year,
                         n_projected_years,
                         relative,
                         make_rda,
                         rda_dir)

  satf::plot_biomass(dat,
                     unit_label,
                     scale_amount,
                     ref_line,
                     end_year,
                     relative,
                     make_rda,
                     rda_dir)

  satf::plot_landings(dat,
                      unit_label,
                      make_rda,
                      rda_dir)

  satf::plot_recruitment_deviations(dat,
                                    end_year,
                                    n_projected_years,
                                    make_rda,
                                    rda_dir)

  satf::plot_spawn_recruitment(dat,
                         spawning_biomass_label,
                         recruitment_label,
                         end_year,
                         make_rda,
                         rda_dir)

  satf::plot_spawning_biomass(dat,
                              unit_label,
                              scale_amount,
                              ref_line_sb,
                              end_year,
                              relative,
                              n_projected_years,
                              make_rda,
                              rda_dir)

}
