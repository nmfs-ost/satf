#' Export all figures and tables
#'
#' Export all figures and tables to Rda files within one function.
#'
#' @inheritParams plot_recruitment
#' @param recruitment_unit_label Units for recruitment
#' @param ref_line @inheritParams plot_biomass
#' @param landings_unit_label Units for landings
#' @param biomass_unit_label Units for biomass
#' @param spawning_biomass_label @inheritParams plot_spawn_recruitment
#' @param ref_line_sb Identical definition as `ref_line`, but this argument is
#' applied to plot_spawning_biomass.
#' @param indices_unit_label Units for index of abundance/CPUE
#' @param catch_unit_label @inheritParams table_bnc
#' @param biomass_unit_label @inheritParams table_bnc
#'
#' @return Rda files for each figure/table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' exp_all_figs_tables(dat, ref_line = "unfished",
#' ref_line_sb = "target", make_rda = TRUE, indices_unit_label = "CPUE")
#' }

exp_all_figs_tables <- function(

  # imported from plot_recruitment
  dat,
  recruitment_unit_label = "mt", # changed from unit_label to recruitment_unit_label for specificity
  scale_amount = 1,
  end_year = NULL,
  n_projected_years = 10,
  relative = FALSE,
  rda_dir = getwd(),

  # imported from plot_biomass
  ref_line = c("target", "MSY", "msy", "unfished"),

  # imported from plot_landings
  landings_unit_label = "mt",

  # imported from plot_recruitment_deviations- zero unique arguments

  # imported from plot_spawn_recruitment
  spawning_biomass_label = "mt",

  # imported from plot_spawning_biomass
  ref_line_sb = c("target", "MSY", "msy", "unfished"),

  # imported from plot_indices
  indices_unit_label = NULL,

  # imported from table_afsc_tier- add potential unique arguments after dev

  # imported from table_bnc
  biomass_unit_label = "mt",
  catch_unit_label = "mt"

  # imported from table_harvest_projection- add potential unique arguments after dev

  # imported from table_indices- zero unique arguments

) {

  make_rda <- TRUE

  # figures

  satf::plot_recruitment(dat,
                         unit_label = recruitment_unit_label,
                         scale_amount,
                         end_year,
                         n_projected_years,
                         relative,
                         make_rda,
                         rda_dir) |> suppressWarnings() |> invisible()

  message("Exported plot_recruitment")

  satf::plot_biomass(dat,
                     unit_label = biomass_unit_label,
                     scale_amount,
                     ref_line,
                     end_year,
                     relative,
                     make_rda,
                     rda_dir) |> suppressWarnings() |> invisible()

  message("Exported plot_biomass")

  satf::plot_landings(dat,
                      unit_label = landings_unit_label,
                      make_rda,
                      rda_dir) |> suppressWarnings() |> invisible()

  message("Exported plot_landings")

  satf::plot_recruitment_deviations(dat,
                                    end_year,
                                    n_projected_years,
                                    make_rda,
                                    rda_dir) |> suppressWarnings() |> invisible()

  message("Exported plot_recruitment_deviations")

  # satf::plot_spawn_recruitment(dat,
  #                        spawning_biomass_label,
  #                        recruitment_label = recruitment_unit_label,
  #                        end_year,
  #                        make_rda,
  #                        rda_dir) |> suppressWarnings() |> invisible()
  #
  # message("Exported plot_spawn_recruitment")

  satf::plot_spawning_biomass(dat,
                              unit_label = spawning_biomass_label,
                              scale_amount,
                              ref_line_sb,
                              end_year,
                              relative,
                              n_projected_years,
                              make_rda,
                              rda_dir) |>
 #   suppressWarnings() |>
    invisible()

  message("Exported plot_spawning_biomass")

  # uncomment when this is working properly
  # satf::plot_indices(dat,
  #                    unit_label = indices_unit_label,
  #                    make_rda,
  #                    rda_dir) |> suppressWarnings() |> invisible()
  #
  # message("Exported plot_indices")

  # tables
  satf::table_bnc(dat,
            end_year,
            biomass_unit_label,
            catch_unit_label,
            make_rda,
            rda_dir) |> suppressWarnings() |> invisible()

  message("Exported table_bnc")

  satf::table_indices(dat,
                make_rda,
                rda_dir) |> suppressWarnings() |> invisible()

  message("Exported table_indices")

  # uncomment when finished
  # satf::table_landings(dat) |> suppressWarnings() |> invisible()
  #
  # message("Exported table_landings")
  #
  # undeveloped tables - add arguments after more development
  # table_afsc_tier() |> suppressWarnings() |> invisible()
  # table_harvest_projection() |> suppressWarnings() |> invisible()

}
