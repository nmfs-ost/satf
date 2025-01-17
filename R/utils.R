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
    dir = NULL,
    end_year = NULL,
    units = NULL,
    sr_ssb_units = NULL,
    sr_recruitment_units = NULL,
    ref_pt = NULL){

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


  # calculate key quantities that rely on end_year for calculation
  ## terminal fishing mortality
  if (topic_cap_alt$label == "fishing.mortality") {

    F.end.year <- dat |>
      dplyr::filter(
        c(label == 'fishing_mortality' &
            year == end_year) |
          c(label == 'terminal_fishing_mortality' & is.na(year))
      ) |>
      dplyr::pull(estimate) |>
      as.numeric() |>
      round(digits = 2)

    # COMMENTING OUT THESE LINES because the current alt text/captions csv
    # doesn't include Ftarg or F.Ftarg. If we alter them to include them,
    # then uncomment these lines and add code that would substitute the key
    # quantities into the df, like at the bottom of write_captions.
    #
    # # recalculate Ftarg for F.Ftarg, below
    # Ftarg <- dat |>
    #   dplyr::filter(grepl('f_target', label) |
    #                   grepl('f_msy', label) |
    #                   c(grepl('fishing_mortality_msy', label) &
    #                       is.na(year))) |>
    #   dplyr::pull(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)
    #
    # # Terminal year F respective to F target
    # F.Ftarg <- F.end.year / Ftarg

    if (!is.null(F.end.year)){
      end_year <- as.character(F.end.year)
    }
  }


  # replace placeholders (e.g., if "end.year" is found in topic_alt, replace it with end_year)
  ## end_year-----
  if(!is.null(end_year)){
  ### alt text
  ### this regex preserves the comma after the end year
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(alt_text = stringr::str_replace_all(alt_text,
                                                      stringr::regex("(\\S*end\\.year\\S*)(?=\\s?,)"),
                                                      end_year))

  ### this regex removes a potential trailing space after the end year
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(alt_text = stringr::str_replace_all(alt_text,
                                                      stringr::regex("\\S*end\\.year\\S*\\s*"),
                                                      end_year))
  }
  ## units-----
  if(!is.null(units)){
  ### caption
  ### this regex preserves the closing ) after the units
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(caption = stringr::str_replace_all(caption,
                                                      stringr::regex("(\\S*units\\S*)(?=\\s?\\))"),
                                                      as.character(units)))

  ### this regex preserves the period after the units
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(caption = stringr::str_replace_all(caption,
                                                     stringr::regex("(\\S*units\\S*)(?=\\s?.)"),
                                                     as.character(units)))

  ### this regex replaces the units if it's not found with the previous two commands
  ### (i.e., there's no parenthesis or period adjacent to the units variable)
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(caption = stringr::str_replace_all(caption,
                                                     stringr::regex("\\S*units\\S*"),
                                                     as.character(units)))
  }

  if(!is.null(sr_ssb_units)){
  ### this is for plot_spawn_recruitment, since there are two units
  #### replace sr.ssb.units with sr_ssb_units
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(alt_text = stringr::str_replace_all(alt_text,
                                                      "sr.ssb.units",
                                                      as.character(sr_ssb_units)))
  }

  if(!is.null(sr_recruitment_units)){
  ### this is for plot_spawn_recruitment, since there are two units
  #### replace sr.units with sr_recruitment_units
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(alt_text = stringr::str_replace_all(alt_text,
                                                      "sr.units",
                                                      as.character(sr_recruitment_units)))
  }

  if(!is.null(units)){
  ### alt text
  ### this regex preserves the comma after the units
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(alt_text = stringr::str_replace_all(alt_text,
                                                      stringr::regex("(\\S*units\\S*)(?=\\s?,)"),
                                                      as.character(units)))

  ### this regex replaces the units if it's not found with the previous command
  ### (i.e., there's no comma adjacent to the units variable)
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(alt_text = stringr::str_replace_all(alt_text,
                                                     stringr::regex("\\S*units\\S*"),
                                                     as.character(units)))

  }
  ## reference points-----
  if(!is.null(ref_pt)){
  ### caption
  ### this regex preserves the opening ( before the ref pt
  topic_cap_alt <- topic_cap_alt |>
    dplyr::mutate(caption = stringr::str_replace_all(caption,
                                                     stringr::regex("\\(\\S*ref\\.pt*\\S*"),
                                                     paste0("(", as.character(ref_pt))))
  }

  # remove row with old caption/alt text, then add new row
  replaced_df <- dplyr::anti_join(caps_alt_df,
                                  topic_cap_alt,
                                  by = c("label", "type")) |>
    dplyr::full_join(topic_cap_alt)

  # export df with updated captions and alt text to csv
  utils::write.csv(x = replaced_df,
                   file = fs::path(dir,
                                   "captions_alt_text.csv"),
                   row.names=FALSE)

  }
