#' Create Indices of Abundance Table
#'
#' @inheritParams plot_recruitment
#'
#' @return Create table of observed annual indices of abundance plus error
#' stratified by fleet.
#' @export
#'

table_indices <- function(dat,
                          model = "standard"){
  if (model == "standard"){
    output <- utils::read.csv(dat)
    output <- output |>
      dplyr::filter(module_name == "INDEX_2" | module_name == "t.series")
    if (any(unique(output$module_name=="INDEX_2"))) {
      output <- output |>
        dplyr::filter(grepl("obs", label))
    } else if (any(unique(output$module_name=="t.series"))) {
      output <- output |>
        dplyr::filter(grepl("cpue", label))
    }
    fleet_names <- unique(output$fleet)
    factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo","growth_pattern", "gp")
    # re-structure df for table
    indices <- output |>
      dplyr::rename(!!unique(output$label) := estimate,
                    !!unique(output$uncertainty_label) := uncertainty) |>
      tidyr::pivot_wider(
        id_cols = -intersect(colnames(output), factors),
        names_from = fleet,
        values_from = c(unique(output$label), unique(output$uncertainty_label))
      ) # stated internal error for tidyr and asks to report - try again monday

  } else if (model == "SS3"){
    # Read report file
    if(grepl(".sso|.txt", dat)){
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
    }
    # Extract Fleet Names
    fleet_info <- SS3_extract_df(output, "Fleet")[,1]
    colnames(fleet_info) <- fleet_info[1,1]
    fleet_info <- fleet_info[-1,]
    fleet_names <- unlist(as.vector(fleet_info))
    fleet_names <- paste("Fleet_", fleet_names, sep ="")
    # fleet_names <- as.character(fleet_info[fleet_info$X1=="fleet_names:",][,-1])
    # fleet_names <- gsub("_", " ", fleet_names)

    # Index 1
    index1 <- SS3_extract_df(output, "INDEX_1")
    colnames(index1) <- index1[2,]
    index1 <- index1[-c(1:2),]

    # Index 2
    index2 <- SS3_extract_df(output, "INDEX_2")
    colnames(index2) <- index2[2,]
    index2 <- index2[-c(1:2),] |>
      dplyr::select(Fleet, Yr, Seas, Area, Subseas, Month, Obs, SE, Exp) |>
      dplyr::mutate(Obs = round(as.numeric(Obs), digits = 3),
                    SE = round(as.numeric(SE), digits = 3),
                    Exp = round(as.numeric(Exp), digits = 3))
    index2_obs <- index2 |>
      tidyr::pivot_wider(id_cols = Yr,
                         names_from = Fleet,
                         values_from = Obs) |>
      dplyr::rename_at(dplyr::vars(-1), ~ paste(., "obs", sep = "_"))
    index2_se <- index2 |>
      tidyr::pivot_wider(id_cols = Yr,names_from = Fleet, values_from = SE) |>
      dplyr::rename_at(dplyr::vars(-1), ~ paste(., "se", sep = "_"))

    # ind_fleets <- unique(index2$Fleet) |>
    #   strsplit(split = "_")
    ind_fleets <- paste("Fleet_", unique(index2$Fleet), sep = "")

    # Recombine fleet names - header name ready
    # select_one <- function(x){
    #   a <- lapply(x, tail, -1)
    #   sapply(a, paste, collapse = " ")
    # }
    #
    # ind_fleets <- select_one(ind_fleets)

    indices <- merge(index2_obs, index2_se) |>
      dplyr::rename(year = Yr)
    indices <- indices[, sort(names(indices))] |>
      dplyr::select("year", dplyr::everything())
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
      dplyr::select(year, dplyr::contains("U.") & contains(".ob") | contains("cv.U"))

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
