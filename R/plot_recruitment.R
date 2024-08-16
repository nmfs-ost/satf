#' Plot Recruitment
#'
#' @template dat
#' @template model
#' @param params Print/export the parameters of the stock recruitment function?
#' @param params_only Only export the stock recruitment function or both the parameters and the plot(s)?
#' @param units If units are not available in the output file, in metric tons,
#' or are different for SB and R, then report them here starting with SB units
#' and following with R units.
#' @param show_warnings Include warnings? Default FALSE
#' @param return Default returns recruitment over time. Options to display stock recruitment curve/recruitment fit
#'
#' @return A series of plots are exported including recruitment over time with R0
#' reference line, stock recruitment curve, and other related figures.
#' @export
#'

plot_recruitment <- function(dat,
                             model,
                             params = FALSE,
                             params_only = FALSE,
                             units = c(sb = "metric tons", recruitment = "metric tons"),
                             show_warnings = FALSE,
                             return = "recruitment"){
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

    # Units - change this statement to fit new convention
    if(!is.null(units)){
      if(length(units)>1){
        sb_units <- units["sb"]
        rec_units <- units["recruitment"]
        message("Please check the units on your axes are correct. If they are flipped, change the order of names in the units argument.")
      } else {
        if(grepl("eggs", units)){
          sb_units <- "1e10 eggs"
          rec_units <- "metric tons"
        } else if(grepl("number", units)){
          rec_units <- "number of fish"
          sb_units <- "metric tons"
        } else {
          warning("Unit type is not defined for this function. Please leave an issue at https://github.com/nmfs-ost/satf/issues")
        }
      }
      message("Default units for both SB and R are metric tons.")
    }

    # remove inital and virg recruitment
    sr <- sr |>
      dplyr::filter(year!="Init" & year!="Virg" & era != "Forecast") |>
      dplyr::mutate(year = as.numeric(year))

    # need to rescale to 1000s rather than xe..
    sr_plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_line(ggplot2::aes(x = spawn_bio/1000, y = exp_recr/1000), linewidth = 1) + # exp. R
      # add exp R after bias adjustment (dotted line)
      ggplot2::geom_point(ggplot2::aes(x = spawn_bio/1000, y = pred_recr/1000, color = year)) + # change colors
      # ggplot2::geom_text() +
      ggplot2::labs(x = paste("Spawning Biomass (", sb_units, ")", sep = ""),
           y = paste("Recruitment (", rec_units, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")
    # sr_plt <- add_theme(sr_plt)

    r_plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = pred_recr)) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = pred_recr), linewidth = 1) +
      ggplot2::labs(x = "Year",
                    y = paste("Recruitment (", rec_units, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")
    # r_plt <- add_theme(r_plt)

    # recruitment deviations
    params <- SS3_extract_df(output, "PARAMETERS")
    colnames(params) <- params[2,]
    params2 <- params[-c(1:2),] |>
      dplyr::filter(grepl('RecrDev', Label)) |>
      dplyr::select(Label, Value) |>
      tidyr::separate_wider_delim(Label, delim = "_", names = c("Era", "Recr", "Year"))
    params_proj <-  params[-c(1:2),] |>
      dplyr::filter(grepl('ForeRecr', Label)) |>
      dplyr::select(Label, Value) |>
      tidyr::separate_wider_delim(Label, delim = "_", names = c("Recr", "Year")) |>
      dplyr::mutate(Era = "Fore")

    params_fin <- rbind(params2, params_proj)

    rdev_plt <- ggplot2::ggplot(data = params) +
      # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
      ggplot2::geom_pointrange(ggplot2::aes(x = Year, y = Value, ymax = log_rec_dev, ymin = 0),  fatten = 1, size = 2, shape = 1) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(x = "Year",
                    y = "logR Deviations")

    # params_sub = params[grep("Recr",params$Label),]
    # Years = t(data.frame(strsplit(params_sub$Label,"_")))
    # Years = as.data.frame(t(data.frame(strsplit(params_sub$Label,"_"))))

  } # close SS3 if statement

  if(model == "BAM"){
    # read in output
    output <- dget(dat)

    R_virg <- output$parms$R.virgin.bc
    R0 <- output$parms$R0
    SSBmsy <- output$parms$SSBmsy
    SSB0 <- output$parms$SSB0

    # lnr0 <- "Ln(R0)"
    # steep <- "steep"
    sigr <- output$parms$R.sigma.par
    sigr_dev <- output$parms$R.sigma.logdevs
    # envlink <- "env_link_" # is there an equivalent in BAM?
    # ini_eq <- "init_eq" # what is this SR param?

    # Export message
    # message(cat("Stock Recrutiment Fxn: ", sr_fxn, "\n", # need to add conversion to what this number means to analyst
    #             "    ", "    ", "   ", "ln(R0): ", lnr0, "\n",
    #             "     ", "      ", "     ", "h: ", steep, "\n",
    #             "    ", "    ", "   ", "sigmaR: ", sigr, "\n",
    #             "    ", "    ", " ", "env_link: ", envlink, "\n",
    #             "    ", "    ", "  ", "init_eq: ", ini_eq, "\n"
    #   )
    # )

    # SSB time series
    sr <- data.frame(year = output$t.series$year,
                     spawn_bio = output$t.series$SSB,
                     pred_recr = output$t.series$recruits,
                     exp_recr = output$t.series,
                     log_rec_dev = output$t.series$logR.dev)

    # Check if units were declared
    if(!is.null(units)){
      if(length(units)>1){
        sb_units <- units[1]
        rec_units <- units[2]
        message("Please check the units on your axes are correct. If they are flipped, change the order of names in the units argument.")
      } else {
        if(grepl("eggs", units)){
          sb_units <- "1e10 eggs"
          rec_units <- "metric tons"
        } else if(grepl("number", units)){
          rec_units <- "number of fish"
          sb_units <- "metric tons"
        } else {
          warning("Unit type is not defined for this function. Please leave an issue at https://github.com/nmfs-ost/satf/issues")
        }
      }
    } else {
      sb_units <- "metric tons"
      rec_units <- "metric tons"
      message("Default units for both SB and R are metric tons.")
    }
    # plot stock recruitment in 1000
    sr_plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_line(ggplot2::aes(x = spawn_bio, y = exp_recr), linewidth = 1) + # exp. R
      # add exp R after bias adjustment (dotted line)
      ggplot2::geom_point(ggplot2::aes(x = spawn_bio, y = pred_recr, color = year)) + # change colors
      # ggplot2::geom_text() +
      ggplot2::labs(x = paste("Spawning Biomass (", sb_units, ")", sep = ""),
                    y = paste("Recruitment (", rec_units, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")

    # sr_plt <- add_theme(sr_plt)

    r_plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = pred_recr), linewidth = 1) + # exp. R
      ggplot2::geom_point(ggplot2::aes(x = year, y = pred_recr)) + # change colors
      # ggplot2::geom_text() +
      ggplot2::labs(x = "Year",
                    y = paste("Recruitment (", rec_units, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")

    # r_plt <- add_theme(r_plt)

    rdev_plt <- ggplot2::ggplot(data = sr) +
      # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
      ggplot2::geom_pointrange(ggplot2::aes(x = year, y = log_rec_dev, ymax = log_rec_dev, ymin = 0),  fatten = 1, size = 2, shape = 1) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(x = "Year",
                    y = "logR Deviations")
    # rdev_plt <- add_theme(rdev_plt)

  } # close BAM if statement
  if(return == "recruitment"){
    return(add_theme(r_plt))
  } else if (return == "stock recruitment") {
    return(add_theme(sr_plt))
  } else if (return == "recruitment deviations"){
    return(add_theme(rdev_plt))
  }

  if(model == "base"){
    out <- readRDS(dat)
    # Stock Recruitment
    sr_plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_line(ggplot2::aes(x = spawn_bio/1000, y = exp_recr/1000), linewidth = 1) + # exp. R
      # add exp R after bias adjustment (dotted line)
      ggplot2::geom_point(ggplot2::aes(x = spawn_bio/1000, y = pred_recr/1000, color = year)) + # change colors
      # ggplot2::geom_text() +
      ggplot2::labs(x = paste("Spawning Biomass (", sb_units, ")", sep = ""),
                    y = paste("Recruitment (", rec_units, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")

    # Recruitment time series
    r_plt <- ggplot2::ggplot(data = sr) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = pred_recr)) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = pred_recr), linewidth = 1) +
      ggplot2::labs(x = "Year",
                    y = paste("Recruitment (", rec_units, ")", sep = "")) +
      ggplot2::theme(legend.position = "none")
    # Recruitment deviations
    rdev_plt <- ggplot2::ggplot(data = params) +
      # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
      ggplot2::geom_pointrange(ggplot2::aes(x = Year, y = Value, ymax = log_rec_dev, ymin = 0),  fatten = 1, size = 2, shape = 1) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(x = "Year",
                    y = "logR Deviations")
  }

}

