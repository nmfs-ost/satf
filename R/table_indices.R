table_indices <- function(dat,
                          model){
  if(model == "SS3"){
    # Read report file
    if(grepl("Report.sso", dat)){
      get_ncol <- function(file, skip = 0) {
        nummax <- max(utils::count.fields(file,
                                          skip = skip, quote = "",
                                          comment.char = ""
        )) + 1
        return(nummax)
      }

      output <- utils::read.table(
        file = dat, col.names = 1:get_ncol(dat), fill = TRUE, quote = "",
        colClasses = "character", nrows = -1, comment.char = "",
        blank.lines.skip = FALSE
      )
    } else {
      output <- dat
    }



  } # close SS3 if statement

  if(model == "BAM"){
    output <- dget(dat)

  } # close BAM if statement
}
