## code to prepare `DATASET` dataset goes here
args <- commandArgs(trailingOnly = FALSE)
script_file <- grep("^--file=", args, value = TRUE)
file_index <- match("--file", args)
script_path <- if (length(script_file) == 1) {
  sub("^--file=", "", script_file)
} else if (!is.na(file_index) && length(args) > file_index) {
  args[[file_index + 1]]
} else {
  stop("Run DATASET.R with Rscript so the repository root can be resolved.")
}
script_path <- normalizePath(script_path)
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
old_wd <- setwd(repo_root)
on.exit(setwd(old_wd), add = TRUE)

# Path to SS3 output file
EX_REPORT_PATH <- file.path("inst", "extdata", "Report.sso")
# Convert output
example_data <- stockplotr::convert_output(EX_REPORT_PATH)
# Save object as rda into raw_data
usethis::use_data(example_data, overwrite = TRUE)
