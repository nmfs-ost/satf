table_bnc <- function(dat,
                      model) {
    if (model == "standard"){

    } else if (tolower(model) == "ss3") {

    } else if (tolower(model) == "bam") {

    }

  # identify output
  fig_or_table <- "table"

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
  # -defines topic_label, fig_or_table; and
  # -makes an rda if make_rda = TRUE
  # (see table_indices.R for reference)
  # for the rda-related fxns to work, the final table has to be called tab
}
