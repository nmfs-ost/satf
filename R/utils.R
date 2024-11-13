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


