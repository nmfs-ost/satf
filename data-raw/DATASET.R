## code to prepare `DATASET` dataset goes here
args <- commandArgs(trailingOnly = FALSE)
script_file <- grep("^--file=", args, value = TRUE)
file_index <- match("--file", args)
repo_root <- if (length(script_file) == 1) {
  script_path <- normalizePath(sub("^--file=", "", script_file))
  normalizePath(file.path(dirname(script_path), ".."))
} else if (!is.na(file_index) && length(args) > file_index) {
  script_path <- normalizePath(args[[file_index + 1]])
  normalizePath(file.path(dirname(script_path), ".."))
} else if (interactive() && file.exists("DESCRIPTION")) {
  normalizePath(".")
} else {
  stop("Run DATASET.R with Rscript, or source it after setting the working directory to the repository root.")
}
old_wd <- setwd(repo_root)
on.exit(setwd(old_wd), add = TRUE)

# Path to SS3 output file
EX_REPORT_PATH <- file.path("inst", "extdata", "Report.sso")
# Convert output
example_data <- stockplotr::convert_output(EX_REPORT_PATH)
# Save object as rda into raw_data
usethis::use_data(example_data, overwrite = TRUE)
