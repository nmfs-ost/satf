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
