#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# extract captions and alt text from csv in wd
extract_caps_alttext <- function(topic_label,
                                 fig_or_table,
                                 dir = getwd()){

  # import csv with captions and alt text
  # Note: make sure to check this later in the workflow and its use -
  # I could see it having issues
  captions_alttext_df <- utils::read.csv(
    fs::path(dir, "captions_alt_text.csv")
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

# substitute in more key quantities (units, end_years, reference points) to captions/alt text
add_more_key_quants <- function(
    topic = topic_label,
    fig_or_table = fig_or_table,
    end_year = NULL,
    units = NULL){

  # import csv
  caps_alt_df <- read.csv(fs::path(getwd(), "captions_alt_text.csv"))

  # make year character if not null
  if (!is.null(end_year)){
    end_year <- as.character(end_year)
  }

  # select specific fig/table's caption/alt text
  topic_cap_alt <- caps_alt_df |>
    dplyr::filter(label == topic,
                  type == fig_or_table)

  # select caption
  # topic_cap <- topic_cap_alt |>
  #   dplyr::select(caption)

  # select alt text
  topic_alt <- topic_cap_alt |>
    dplyr::select(alt_text)

  # replace placeholders in alt text
  ## end_year
  # if "end.year" is found in topic_alt, replace it with end_year
  topic_alt <- sub("\\S*end\\.year\\S*",
                   as.character(end_year),
                   topic_alt)

  ## units
  topic_alt <- sub("\\S*units\\S*",
                   as.character(units),
                   topic_alt)

  ## reference points
  ## TODO: ADD HERE

  # replace old alt text with new alt text
 test <- caps_alt_df[caps_alt_df$type == fig_or_table,
                    # caps_alt_df$label == topic
                     ]


  }

