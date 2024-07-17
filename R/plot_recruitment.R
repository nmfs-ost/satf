#' Plot Recruitment
#'
#' @template dat
#' @template model
#' @param params Print/export the parameters of the stock recruitment function?
#' @param params_only Only export the stock recruitment function or both the parameters and the plot(s)?
#' @param units If units are not available in the output file, in metric tons,
#' or are different for SB and R, then report them here starting with SB units
#' and following with R units.
#' @param include_fxn Include the stock recruitment function on the SR curve plot?
#' @param show_warnings Include warnings? Default FALSE
#'
#' @return A series of plots are exported including recruitment over time with R0
#' reference line, stock recruitment curve, and other related figures.
#' @export
#'
#' @examples
plot_recruitment <- function(dat,
                             model,
                             params = FALSE,
                             params_only = FALSE,
                             units = NULL,
                             include_fxn = FALSE,
                             show_warnings = FALSE){
  if(model == "SS3"){
    # Read rep file or rename
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
        nrows = -1, comment.char = "", blank.lines.skip = FALSE
      )
    } else {
      output <- dat
    }
    # extract recruitment
    sr_info <- SS3_extract_df(output, "SPAWN_RECRUIT")
    sr <- sr_info[-c(1:13),]
    colnames(sr) <- sr_info[12,]
    sr <- sr  |>
      dplyr::mutate(spawn_bio = as.numeric(spawn_bio),
                    exp_recr = as.numeric(exp_recr),
                    with_env = as.numeric(with_env),
                    adjusted = as.numeric(adjusted),
                    pred_recr = as.numeric(pred_recr),
                    # dev = as.numeric(dev),
                    biasadj = as.numeric(biasadj))

    # Store virgin and initial recruitment for reference in plotting
    time_series <- SS3_extract_df(output, "TIME_SERIES")
    colnames(time_series) <- time_series[2,]
    time_series <- time_series[-c(1:2),]
    time_series[, 5:ncol(time_series)] <- lapply(5:ncol(time_series), function(x) suppressWarnings(as.numeric(time_series[[x]])))

    # Adapted from r4ss
    B0 <- sum(time_series[["SpawnBio"]][time_series[["Era"]] == "VIRG"], na.rm = TRUE)
    B1 <- sum(time_series[["SpawnBio"]][time_series[["Era"]] == "INIT"], na.rm = TRUE)
    R0 <- sum(time_series[["Recruit_0"]][time_series[["Era"]] == "VIRG"], na.rm = TRUE)
    R1 <- sum(time_series[["Recruit_0"]][time_series[["Era"]] == "INIT"], na.rm = TRUE)

    # extract sr_err
    sr_err <- sr_info[10:11,]
    colnames(sr_err) <- sr_info[9,]
    sr_err <- Filter(function(x)!all(is.na(x)), sr_err)

    # sr fxn variables
    sr_vars <- sr_info[2:6,1:2]
    colnames(sr_vars) <- c("value", "variable")
    sr_fxn <- sr_info[1,3][[1]]

    if (isTRUE(params) & isTRUE(params_only)){
      return(sr_vars)
      stop("Only stock recruitment model parameters were exported and plots were not created.")
    }

    lnr0 <- sr_vars$value[sr_vars$variable=="Ln(R0)"]
    steep <- sr_vars$value[sr_vars$variable=="steep"]
    sigr <- sr_vars$value[sr_vars$variable=="sigmaR"]
    envlink <- sr_vars$value[sr_vars$variable=="env_link_"]
    ini_eq <- sr_vars$value[sr_vars$variable=="init_eq"]

    # Export message
    message(cat("Stock Recrutiment Fxn: ", sr_fxn, "\n", # need to add conversion to what this number means to analyst
                "    ", "    ", "   ", "ln(R0): ", lnr0, "\n",
                "     ", "      ", "     ", "h: ", steep, "\n",
                "    ", "    ", "   ", "sigmaR: ", sigr, "\n",
                "    ", "    ", " ", "env_link: ", envlink, "\n",
                "    ", "    ", "  ", "init_eq: ", ini_eq, "\n"
                )
            )

    # remove inital and virg recruitment
    sr2 <- sr |> dplyr::filter(year!="Init" & year!="Virg")

    # need to rescale to 1000s rather than xe..
    sr_plt <- ggplot2::ggplot(data = sr2) +
      ggplot2::geom_line(ggplot2::aes(x = spawn_bio/1000, y = exp_recr/1000), linewidth = 1) + # exp. R
      # add exp R after bias adjustment (dotted line)
      ggplot2::geom_point(ggplot2::aes(x = spawn_bio/1000, y = pred_recr/1000, color = year)) + # change colors
      # ggplot2::geom_text() +
      ggplot2::labs(x = "Spawning Biomass (metric tons)",
           y = "Recruitment (metric tons)") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none")

    sr_plt <- add_theme(sr_plt)

    rts <- ggplot2::ggplot()
  } # close SS3 if statement

  if(model == "BAM"){

  } # close BAM if statement
  return(sr_plt)
}

