#' Export figure and alternative text
#'
#' Function to export a specified plot object, and its alternative
#' text, from the R environment.
#'
#' @param figure Plot object in the R environment to export.
#' @param alt_text Alternative text for the figure.
#' @param path Directory in which "exported" folder should be saved.
#' @param width Plot width, in units (see argument below). Default
#' is 5.
#' @param height Plot height, in units (see argument below). Default
#' is 5.
#' @param units Plot size units (options: "in", "cm", "mm", "px").
#' Default is "cm".

#' @return A folder containing exported plots and associated
#'  alternative text as .png and .csv objects, respectively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model_data <- utils::read.csv(here::here("data/Petrale_sole_std_res_2023.csv"))
#'
#' fig1 <- satf::plot_spawning_biomass(model_data)
#'
#' exp_fig_accessible(figure = fig1,
#'                    alt_text = "my alt text",
#'                    path = here::here(),
#'                    width = 5,
#'                    height = 5,
#'                    units = "cm")
#'
#' }

exp_fig_accessible <- function(figure,
                               alt_text,
                               path,
                               width = 5,
                               height = 5,
                               units = "cm"){

  # turn figure name into a character string so it can be
  # pasted into filepath
  fig_name_deparsed <- sprintf(deparse(substitute(figure)))

  # create new "exported" folder
  new_path <- paste0(path, "/", "exported")
  dir.create(path = new_path)

  # export figure to png
  ggplot2::ggsave(filename = paste0(fig_name_deparsed, ".png"),
                  plot = figure,
                  path = new_path,
                  width = width,
                  height = height,
                  units = units
                  )

  # make template csv to store alt text
  alt_text_df <- data.frame("Figure" = as.character(fig_name_deparsed),
                            "Alt_text" = as.character(alt_text))

  # export alt text csv
  utils::write.csv(x = alt_text_df,
            file = paste0(new_path, "/", fig_name_deparsed, "_alt_text.csv"),
            row.names=FALSE)

}
