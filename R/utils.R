#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# extract captions and alt text from csv in wd
extract_caps_alttext <- function(topic_label,
                                 fig_or_table){

  # import csv with captions and alt text
  captions_alttext_df <- read.csv(
    fs::path(getwd(), "captions_alt_text.csv")
  )

  # extract plot or table's caption and alt text
  cap <- captions_alttext_df |>
    dplyr::filter(label == topic_label,
                  type == fig_or_table) |>
    dplyr::select(caption) |>
    as.character()

  if (fig_or_table == "figure"){
    alt_text <- captions_alttext_df |>
      dplyr::filter(label == topic_label,
                    type == "figure") |>
      dplyr::select(alt_text) |>
      as.character()

    caps_alttext_list <- list(cap,
                              alt_text)
  } else {
    caps_alttext_list <- list(cap)
  }

  return(caps_alttext_list)
}

# export ggplot, alt text, and caption to rda if indicated in arguments
export_rda <- function(plt_fin = plt_fin,
                       caps_alttext = caps_alttext,
                       rda_folder = rda_folder,
                       topic_label = topic_label){

  # make rda for figures
  if (fig_or_table == "figure") {
    rda <- list("figure" = plt_fin,
                "cap" = caps_alttext[[1]],
                "alt_text" = caps_alttext[[2]])
  # make rda for tables
  } else if (fig_or_table == "table"){
    rda <- list("table" = tab,
                "cap" = caps_alttext[[1]])
  }

  # check if an rda_files folder already exists; if not, make one
  if (!dir.exists(file.path(rda_folder, "rda_files"))) {
    dir.create(file.path(rda_folder, "rda_files"))
  }

  save(rda,
       file = file.path(rda_folder,
                        "rda_files",
                        paste0(topic_label, "_rda.rda")))
}
