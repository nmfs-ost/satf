#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# extract captions and alt text from csv in wd
extract_caps_alttext <- function(topic_label){

  # import csv with captions and alt text
  captions_alttext_df <- read.csv(
    fs::path(getwd(), "captions_alt_text.csv")
  )

  # extract plot's caption and alt text
  cap <- captions_alttext_df |>
    dplyr::filter(label == topic_label) |>
    dplyr::filter(type == "figure") |>
    dplyr::select(caption) |>
    as.character()

  alt_text <- captions_alttext_df |>
    dplyr::filter(label == topic_label) |>
    dplyr::filter(type == "figure") |>
    dplyr::select(alt_text) |>
    as.character()

  caps_alttext_list <- list(cap,
                            alt_text)

  return(caps_alttext_list)
}

# export ggplot, alt text, and caption to rda if indicated in arguments
export_rda <- function(plt_fin = plt_fin,
                       caps_alttext = caps_alttext,
                       rda_folder = rda_folder,
                       topic_label = topic_label){

   rda <- list("figure" = plt_fin,
              "cap" = caps_alttext[[1]],
              "_alttext" = caps_alttext[[2]])

  # check if an rda_files folder already exists; if not, make one
  if (!dir.exists(file.path(rda_folder, "rda_files"))) {
    dir.create(file.path(rda_folder, "rda_files"))
  }

  save(rda,
       file = file.path(rda_folder,
                        "rda_files",
                        paste0(topic_label, "_rda.rda")))
}
