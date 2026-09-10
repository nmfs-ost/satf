## code to prepare `DATASET` dataset goes here
script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_file) == 1) {
  sub("^--file=", "", script_file)
} else {
  sourced_paths <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else frame$ofile
    },
    character(1)
  )
  sourced_paths <- sourced_paths[!is.na(sourced_paths)]

  if (length(sourced_paths) == 0) {
    stop("Unable to determine the DATASET.R path. Run with Rscript or source the file directly.")
  }

  sourced_paths[[length(sourced_paths)]]
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
old_wd <- setwd(repo_root)
on.exit(setwd(old_wd), add = TRUE)

# Path to SS3 output file
EX_REPORT_PATH <- file.path("inst", "extdata", "Report.sso")
# Convert output
example_data <- stockplotr::convert_output(EX_REPORT_PATH)
# Save object as rda into raw_data
usethis::use_data(example_data, overwrite = TRUE)
