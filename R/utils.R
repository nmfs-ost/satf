#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# extract captions and alt text from csv in wd
extract_caps_alttext <- function(topic_label,
                                 fig_or_table){

  # import csv with captions and alt text
  # Note: make sure to check this later in the workflow and its use -
  # I could see it having issues
  captions_alttext_df <- utils::read.csv(
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
export_rda <- function(final = final,
                       caps_alttext = caps_alttext,
                       rda_dir = rda_dir,
                       topic_label = topic_label,
                       fig_or_table = fig_or_table){

  # make rda for figures
  if (fig_or_table == "figure") {
    rda <- list("figure" = final,
                "cap" = caps_alttext[[1]],
                "alt_text" = caps_alttext[[2]])
    # make rda for tables
  } else if (fig_or_table == "table"){
    rda <- list("table" = final,
                "cap" = caps_alttext[[1]])
  }

  # check if an rda_files folder already exists; if not, make one
  if (!dir.exists(fs::path(rda_dir, "rda_files"))) {
    dir.create(fs::path(rda_dir, "rda_files"))
  }

  save(rda,
       file = fs::path(rda_dir,
                        "rda_files",
                        paste0(topic_label, "_", fig_or_table, ".rda")))
}
