#' Plot Recruitment
#'
#' @param dat .csv file and path where the data is located of standard stock assessment output
#' @param model the model in which the output came from (standard, SS3, BAM, ect)
#' @param params Print/export the parameters of the stock recruitment function?
#' @param params_only Only export the stock recruitment function or both the parameters and the plot(s)?
#' @param units If units are not available in the output file, in metric tons,
#' or are different for SB and R, then report them here starting with SB units
#' and following with R units.
#' @param spawning_biomass_units units of spawning biomass if different from biomass
#' @param recruitment_units units for recruitment
#' @param scaled T/F; indicate whether the output values for biomass and recruitment are scaled
#' @param scale_amount indicate the exact amount of scale (i.e. 1000)
#' @param show_warnings Include warnings? Default FALSE
#' @param end_year last year of assessment
#' @param return Default returns recruitment over time. Options to display recruitment time series, stock recruitment curve, or recruitment fit
#'
#' @return A series of plots are exported including recruitment over time with R0
#' reference line, stock recruitment curve, and other related figures.
#' @export
#'

plot_recruitment <- function(dat,
                             model = "standard",
                             params = FALSE,
                             params_only = FALSE,
                             units = c(sb = "metric tons", recruitment = "metric tons"),
                             recruitment_units = "metric tons",
                             spawning_biomass_units = "metric tons",
                             scaled = FALSE,
                             scale_amount = NULL,
                             show_warnings = FALSE,
                             end_year = NULL,
                             return = "recruitment"){
  # check units
  # biomass
  if(!is.null(recruitment_units)){
    ru <- recruitment_units
  } else {
    ru <- "metric tons"
  }
  # spawning biomass
  if(!is.null(spawning_biomass_units)){
    sbu <- spawning_biomass_units
  } else {
    sbu <- "metric tons"
  }

  if (model == "standard"){
    output <- utils::read.csv(dat)
    if (scaled) {
      rec <- output |>
        dplyr::filter(label == "recruitment",
                      module_name == "TIME_SERIES" | module_name == "t.series",
                      !is.na(year),
                      is.na(fleet) | length(unique(fleet)) <= 1,
                      is.na(sex) | length(unique(sex)) <= 1,
                      is.na(area) | length(unique(area)) <= 1,
                      is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                      year != "S/Rcurve" | year != "Init" | year != "Virg"
        ) |> # SS3 and BAM target module names
        dplyr::mutate(estimate = as.numeric(estimate),
                      year = as.numeric(year)) |>
        dplyr::rename(recruitment = estimate) |>
        dplyr::select(-c(module_name, label))
      sb <- output |>
        dplyr::filter(label == "spawning_biomass",
                      module_name == "TIME_SERIES" | module_name == "t.series",
                      !is.na(year),
                      is.na(fleet) | length(unique(fleet)) <= 1,
                      is.na(sex) | length(unique(sex)) <= 1,
                      is.na(area) | length(unique(area)) <= 1,
                      is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                      year != "S/Rcurve" | year != "Init" | year != "Virg"
        ) |> # SS3 and BAM target module names
        dplyr::mutate(estimate = as.numeric(estimate),
                      year = as.numeric(year)) |>
        dplyr::rename(spawning_biomass = estimate) |>
        dplyr::select(-c(module_name, label))
    } else {
      rec <- output |>
        dplyr::filter(label == "recruitment",
                      module_name == "TIME_SERIES" | module_name == "t.series",
                      !is.na(year),
                      is.na(fleet) | length(unique(fleet)) <= 1,
                      is.na(sex) | length(unique(sex)) <= 1,
                      is.na(area) | length(unique(area)) <= 1,
                      is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                      year != "S/Rcurve" | year != "Init" | year != "Virg"
        ) |> # SS3 and BAM target module names
        dplyr::mutate(estimate = as.numeric(estimate),
                      year = as.numeric(year)) |>
        dplyr::rename(recruitment = estimate) |>
        dplyr::select(-c(module_name, label))
      sb <- output |>
        dplyr::filter(label == "spawning_biomass",
                      module_name == "TIME_SERIES" | module_name == "t.series",
                      !is.na(year),
                      is.na(fleet) | length(unique(fleet)) <= 1,
                      is.na(sex) | length(unique(sex)) <= 1,
                      is.na(area) | length(unique(area)) <= 1,
                      is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                      year != "S/Rcurve" | year != "Init" | year != "Virg"
        ) |> # SS3 and BAM target module names
        dplyr::mutate(estimate = as.numeric(estimate),
                      year = as.numeric(year)) |>
        dplyr::rename(spawning_biomass = estimate) |>
        dplyr::select(-c(module_name, label))
    }

    rec_devs <- output |>
      dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                    module_name == "TIME_SERIES" | module_name == "t.series",
                    !is.na(year),
                    is.na(fleet) | length(unique(fleet)) <= 1,
                    is.na(sex) | length(unique(sex)) <= 1,
                    is.na(area) | length(unique(area)) <= 1,
                    is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                    year != "S/Rcurve" | year != "Init" | year != "Virg"
      ) |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year)) |>
      dplyr::rename(recruitment_deviations = estimate) |>
      dplyr::select(-c(module_name, label))
    if(return == "recruitment_deviations" & nrow(rec_devs)==0){
      stop("No recruitment deviations found in data.")
    }

    if (is.null(end_year)){
      endyr <- max(rec$year)
    } else {
      endyr <- end_year
    }
    stryr <- min(rec$year)

    # merge DF
    sr <- dplyr::full_join(sb, rec)

    # Choose number of breaks for x-axis
    x_n_breaks <- round(length(subset(sr, year<=endyr)$year)/10)
    if (x_n_breaks <= 5) {
      x_n_breaks <- round(length(subset(sr, year<=endyr)$year)/5)
    }

    if (return == "stock_recruitment"){
      plt <- ggplot2::ggplot(data = sr) +
        ggplot2::geom_line(ggplot2::aes(x = spawning_biomass, y = recruitment/1000), linewidth = 1) +
        ggplot2::labs(x = paste("Spawning Biomass (", sbu, ")", sep = ""),
                      y = paste("Recruitment (", ru, ")", sep = "")) +
        ggplot2::theme(legend.position = "none")
      # sr_plt <- add_theme(sr_plt)
    } else if (return == "recruitment") {
      plt <- ggplot2::ggplot(data = sr) +
        ggplot2::geom_point(ggplot2::aes(x = year, y = recruitment)) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = recruitment), linewidth = 1) +
        ggplot2::labs(x = "Year",
                      y = paste("Recruitment (", ru, ")", sep = "")) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                    guide = ggplot2::guide_axis(minor.ticks = TRUE))
    } else if (return == "recruitment_deviations") {
      plt <- ggplot2::ggplot(data = rec_devs) +
        # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
        ggplot2::geom_pointrange(ggplot2::aes(x = year, y = estimate, ymax = Value, ymin = 0),  fatten = 1, size = 2, shape = 1) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(x = "Year",
                      y = "Recruitment Deviations")
    }
    plt_fin <- add_theme(plt)
  } else if(model == "SS3") {
    # Read rep file or rename
    if(grepl(".sso", dat)){
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
    sr <- sr_info[-c(1:14),]
    colnames(sr) <- tolower(sr_info[13,])
    sr <- sr  |>
      dplyr::mutate(year = yr,
                    spawnbio = as.numeric(spawnbio),
                    exp_recr = as.numeric(exp_recr),
                    with_regime = as.numeric(with_regime),
                    bias_adjusted = as.numeric(bias_adjusted),
                    pred_recr = as.numeric(pred_recr),
                    # dev = as.numeric(dev),
                    # biasadjuster = as.numeric(biasadjuster)
                    )

    # Store virgin and initial recruitment for reference in plotting
    time_series <- SS3_extract_df(output, "TIME_SERIES")
    colnames(time_series) <- tolower(time_series[2,])
    time_series <- time_series[-c(1:2),]
    time_series[, 5:ncol(time_series)] <- lapply(5:ncol(time_series), function(x) suppressWarnings(as.numeric(time_series[[x]])))

    # Adapted from r4ss
    B0 <- sum(time_series[["spawnbio"]][time_series[["era"]] == "VIRG"], na.rm = TRUE)
    B1 <- sum(time_series[["spawnbio"]][time_series[["era"]] == "INIT"], na.rm = TRUE)
    R0 <- sum(time_series[["recruit_0"]][time_series[["era"]] == "VIRG"], na.rm = TRUE)
    R1 <- sum(time_series[["recruit_0"]][time_series[["era"]] == "INIT"], na.rm = TRUE)

    # Units - change this statement to fit new convention
    sb_units = spawning_biomass_units
    rec_units = recruitment_units
    message("Default units for both SB and R are metric tons.")

    # remove inital and virg recruitment
    sr <- sr |>
      dplyr::filter(year!="Init" & year!="Virg" & era != "Forecast") |>
      dplyr::mutate(year = as.numeric(year))

    if (return == "stock_recruitment") {
      # need to rescale to 1000s rather than xe.
      plt <- ggplot2::ggplot(data = sr) +
        ggplot2::geom_line(ggplot2::aes(x = spawnbio/1000, y = exp_recr/1000), linewidth = 1) + # exp. R
        # add exp R after bias adjustment (dotted line)
        ggplot2::geom_point(ggplot2::aes(x = spawnbio/1000, y = pred_recr/1000, color = year)) + # change colors
        # ggplot2::geom_text() +
        ggplot2::labs(x = paste("Spawning Biomass (", sb_units, ")", sep = ""),
             y = paste("Recruitment (", rec_units, ")", sep = "")) +
        ggplot2::theme(legend.position = "none")
    } else if (return == "recruitment") {
      plt <- ggplot2::ggplot(data = sr) +
        ggplot2::geom_point(ggplot2::aes(x = year, y = pred_recr)) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = pred_recr), linewidth = 1) +
        ggplot2::labs(x = "Year",
                      y = paste("Recruitment (", rec_units, ")", sep = "")) +
        ggplot2::theme(legend.position = "none")
    } else if (return == "recruitment_deviations") {
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

      sr2 <- dplyr::left_join(sr, params_fin, by = c("yr" = "Year")) |>
        dplyr::mutate(Value = as.numeric(Value))

      plt <- ggplot2::ggplot(data = sr2) +
        # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
        ggplot2::geom_pointrange(ggplot2::aes(x = year, y = exp_recr, ymax = Value, ymin = 0),  fatten = 1, size = 2, shape = 1) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(x = "Year",
                      y = "logR Deviations")
    }
    plt_fin <- add_theme(plt)
  } # close SS3 if statement

  if(model == "BAM"){
    # read in output
    output <- dget(dat)

    R_virg <- output$parms$R.virgin.bc
    R0 <- output$parms$R0
    SSBmsy <- output$parms$SSBmsy
    SSB0 <- output$parms$SSB0

    # SSB time series
    sr <- data.frame(year = output$t.series$year,
                     spawn_bio = output$t.series$SSB,
                     pred_recr = output$t.series$recruits,
                     # exp_recr = output$t.series,
                     log_rec_dev = output$t.series$logR.dev)

    # Check if units were declared
    sb_units = spawning_biomass_units
    rec_units = recruitment_units
    message("Default units for both SB and R are metric tons.")

    if (return == "stock_recruitment") {
      # plot stock recruitment in 1000
      plt <- ggplot2::ggplot(data = sr) +
        # ggplot2::geom_line(ggplot2::aes(x = spawn_bio, y = exp_recr), linewidth = 1) + # exp. R
        # add exp R after bias adjustment (dotted line)
        ggplot2::geom_point(ggplot2::aes(x = spawn_bio, y = pred_recr, color = year)) + # change colors
        # ggplot2::geom_text() +
        ggplot2::labs(x = paste("Spawning Biomass (", sb_units, ")", sep = ""),
                      y = paste("Recruitment (", rec_units, ")", sep = "")) +
        ggplot2::theme(legend.position = "none")
    } else if (return == "recruitment") {
      plt <- ggplot2::ggplot(data = sr) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = pred_recr), linewidth = 1) + # exp. R
        ggplot2::geom_point(ggplot2::aes(x = year, y = pred_recr)) + # change colors
        # ggplot2::geom_text() +
        ggplot2::labs(x = "Year",
                      y = paste("Recruitment (", rec_units, ")", sep = "")) +
        ggplot2::theme(legend.position = "none")
    } else if (return == "recruitment_deviations") {
      plt <- ggplot2::ggplot(data = sr) +
        # ggplot2::geom_point(ggplot2::aes(x = year, y = log_rec_dev), shape = 1, size = 2.5) +
        ggplot2::geom_pointrange(ggplot2::aes(x = year, y = log_rec_dev, ymax = log_rec_dev, ymin = 0),  fatten = 1, size = 2, shape = 1) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(x = "Year",
                      y = "logR Deviations")
    }
    plt_fin <- add_theme(plt)
  } # close BAM if statement
  return(plt_fin)
}

