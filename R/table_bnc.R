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

  # REMINDERS: add in code that
  # -adds make_rda and rda_dir as arguments
  # -defines topic_label, fig_or_table, caps_alttext; and
  # -makes an rda if make_rda = TRUE
  # (see table_indices.R for reference)
  # for the rda-related fxns to work, the final table has to be called tab
}
