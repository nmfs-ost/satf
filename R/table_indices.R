#' Create Indices of Abundance Table
#'
#' @template dat
#' @template model
#'
#' @return Create table of observed annual indices of abundance plus error
#' stratified by fleet.
#' @export
#'
#' @examples table_indices(dat = dat, model = "BAM")
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

    # Extract Fleet Names
    fleet_info <- SS3_extract_df(output, "DEFINITIONS")
    fleet_names <- as.character(fleet_info[fleet_info$X1=="fleet_names:",][,-1])
    fleet_names <- gsub("_", " ", fleet_names)

    # Index 1
    index1 <- SS3_extract_df(output, "INDEX_1")
    colnames(index1) <- index1[2,]
    index1 <- index1[-c(1:2),]

    # Index 2
    index2 <- SS3_extract_df(output, "INDEX_2")
    colnames(index2) <- index2[2,]
    index2 <- index2[-c(1:2),] |>
      dplyr::select(Fleet, Yr, Seas, Obs, SE) |>
      dplyr::mutate(Obs = round(as.numeric(Obs), digits = 3),
                    SE = round(as.numeric(SE), digits = 3))
    index2_obs <- index2 |>
      tidyr::pivot_wider(id_cols = Yr,names_from = Fleet, values_from = Obs) |>
      dplyr::rename_at(dplyr::vars(-1), ~ paste(., "obs", sep = "_"))
    index2_se <- index2 |>
      tidyr::pivot_wider(id_cols = Yr,names_from = Fleet, values_from = SE) |>
      dplyr::rename_at(dplyr::vars(-1), ~ paste(., "se", sep = "_"))

    ind_fleets <- unique(index2$Fleet) |>
      strsplit(split = "_")

    # Recombine fleet names - header name ready
    select_one <- function(x){
      a <- lapply(x, tail, -1)
      sapply(a, paste, collapse = " ")
    }

    ind_fleets <- select_one(ind_fleets)

    indices <- merge(index2_obs, index2_se) |>
      dplyr::rename(year = Yr)
    indices <- indices[, sort(names(indices))] |>
      dplyr::select("year", everything())
    indices[is.na(indices)] <- "-"

    tab <- indices |>
      flextable::flextable() |>
      flextable::set_header_labels(values = c(
        "Year", rep(c("Obs.", "SE"),
                    length(ind_fleets))
      )) |>
      flextable::add_header_row(top = TRUE, values = c(
        "", rep(intersect(ind_fleets,fleet_names), each = 2))
      )
    tab <- add_theme(tab)


  } # close SS3 if statement

  if(model == "BAM"){
    output <- dget(dat)
    indices <- output$t.series |>
      dplyr::select(year, contains("U.") & contains(".ob") | contains("cv.U"))

    # Create function to reorder column names so ordered by fleet
    fleet_names <- function(x){
      extract_names <- colnames(x)[-1] |>
        stringr::str_replace("U.", "") |>
        stringr::str_replace("cv.", "") |>
        stringr::str_replace(".ob", "") |>
        unique()
      return(extract_names)
    }
    col_order <- function(x){
      extract_names <- fleet_names(x)
      out_df <- x |> dplyr::select(year)
      for(i in 1:length(extract_names)){
        cols <- grep(extract_names[i], names(x), value = TRUE)
        cols_extract <- x[, cols]
        out_df <- cbind(out_df, cols_extract)
      }
      return(out_df)
    }

    indices <- col_order(indices)
    indices[is.na(indices)] <- "-"
    ind_fleets <- fleet_names(indices)

    tab <- indices |>
      dplyr::mutate(year = as.factor(year)) |>
      flextable::flextable() |>
      flextable::set_header_labels(values = c(
        "Year", rep(c("Obs.", "CV"),
                    length(ind_fleets))
      )) |>
      flextable::add_header_row(top = TRUE, values = c(
        "", rep(ind_fleets, each = 2))
      )
    tab <- add_theme(tab)

  } # close BAM if statement

  return(tab)
}
