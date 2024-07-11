SS3_extract_df <- function(output, label){
  # Locate the row containing the specified value from the df
  value_row <- which(apply(output, 1, function(row) any(row == label)))[2]

  # If the parameter value is not found, return NA
  if(length(value_row) == 0){
    message("Label not found in data frame.")
    return(NA)
  }
  # Search for the next blank row after the value
  next_blank <- which(apply(output, 1, function(row) all(is.na(row) | row == "" | row == "-" | row == "#")) & (seq_len(nrow(output)) > value_row))[1]

  # Combine the rows surrounding the selected metric from the output table
  rows <- c(value_row, next_blank)

  # Extract the metric using the rows from above as a guide and clean up empty columns
  clean_df <- output[rows[1]:(rows[2]-1),] |>
    naniar::replace_with_na_all(condition = ~.x == "")
  clean_df <- Filter(function(x)!all(is.na(x)), clean_df)

  return(clean_df)
}


SS3_extract_bio_info <- function(
    dat,
    parameter = c("SPB", "Recr", "SPRratio", "F", "Bratio", "SSB", "TotBio", "SmryBio", "SPR", "Fstd", "TotYield", "RetYield", "ForeCatch", "OFLCatch", "ForeCatchret", "Bzero"),
    reference.points = TRUE) {
  parameter <- match.arg(parameter, several.ok = FALSE)

  bio_info <- SS3_extract_df(dat, "DERIVED_QUANTITIES")[-c(1:4),]
  colnames(bio_info) <- bio_info[1,]
  bio_info <- bio_info[-1,] |>
    tidyr::separate_wider_delim(cols = LABEL, delim = "_", names = c("label", "year")) |>
    dplyr::filter(label == parameter)
  bio_info <- Filter(function(x)!all(is.na(x)), bio_info)

  if (isTRUE(reference.points)) {

  }

  return(bio_info)

}
