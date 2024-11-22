table_bnc <- function(dat,
                      model) {
    if (model == "standard"){

    } else if (tolower(model) == "ss3") {

    } else if (tolower(model) == "bam") {

    }

  # identify whether function generates a figure or table
  # extract name of function housing id_fxn_output
  fxn_name <- as.character(match.call()[[1]])

  # if housing fxn's name starts with "plot", return "figure"; else, return "table"
  fig_or_table <- ifelse(startsWith(fxn_name, "plot"),
                         "figure",
                         "table")

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv"))
  ) {
    satf::write_captions(dat = dat,
                         dir = getwd(),
                         year = NULL)
  }

  # REMINDERS: add in code that
  # -adds make_rda and rda_dir as arguments
  # -defines topic_label, caps_alttext; and
  # -makes an rda if make_rda = TRUE
  # (see table_indices.R for reference)
  # for the rda-related fxns to work, the final table has to be called tab
}
