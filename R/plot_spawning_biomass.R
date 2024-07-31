#' Plot Spawning Biomass
#'
#' @template dat
#' @template model
#' @param refpts Output a dataframe of reference points for spawning biomass
#' rather than plot. Default is FALSE.
#' @param show_warnings Option to suppress warnings
#' @param units If units are not available in the output file or are not the
#' default of metric tons, then state the units of spawning biomass applicable
#' to the stock. For plotting purposes, spawning biomass is divided by 1000.
#' @param ref_line choose with reference point to plot a reference line and use
#' in relative sb calculations
#' @param endyr input the end year of the stock assessment data (not including
#' projections). This parameter will be depricated once the output converter is fully developed.
#'
#' @return Plot spawning biomass from a stock assessment model as found in a NOAA
#' stock assessment report. Units of spawning biomass can either be manually added
#' or will be extracted from the provided file if possible.
#' @export
#'
plot_spawning_biomass <- function(dat,
                         model = c('SS3','BAM', 'ASAP', 'AMAK', 'WHAM'),
                         refpts = FALSE,
                         show_warnings = FALSE,
                         units = NULL,
                         ref_line = c("target", "MSY", "msy", "unfished"),
                         endyr = NULL
                         ) {
  # if(warnings){
  #
  # } else {
  #   suppressWarnings()
  # }

  model <- match.arg(model, several.ok = FALSE)

  if(length(ref_line)>1){
    ref_line = "target"
  } else {
    ref_line <- match.arg(ref_line, several.ok = FALSE)
  }

  if(!is.null(units)){
    sb_unit <- units
  } else {
    sb_unit <- "metric tons"
  }

  if (model == "SS3") {
    # load SS3 data file
    # check if dat parameter is a report file or a mutates df
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

    bio_info <- SS3_extract_df(output, "DERIVED_QUANTITIES")[-c(1:4),]
    colnames(bio_info) <- bio_info[1,]
    bio_info <- bio_info[-1,] |>
      tidyr::separate_wider_delim(cols = LABEL, delim = "_", names = c("label", "year"))

    # Extract spawning biomass from data
    biomass_info <- SS3_extract_bio_info(dat = dat, parameter = "SPB")
    # Extract B0 from DERIVED_QUANITIES df
    # B0 <- as.numeric(biomass_info[['Value']][biomass_info[['year']]=='Virgin'])
    sb <- biomass_info |>
      dplyr::filter(year != 'Virgin' & year != "Initial") |>
      dplyr::mutate(year = as.numeric(year),
                    value = as.numeric(Value),
                    stddev = as.numeric(StdDev)) |>
      dplyr::select(year, value, stddev) |>
      dplyr::filter(year < endyr + 1)

    # Extract reference points, Btarget, Unfished biomass, and Bmsy
      # tgt <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="Btgt"])
      # unfished <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="Unfished"])
      # msy <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="MSY"])

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

    # Select value for reference line and label
    if (ref_line == "target") {
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="Btgt"])
      ref_line_label <- "target"
      ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[target]))
    } else if (ref_line == "MSY" | ref_line == "msy") {
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="MSY"])
      ref_line_label <- "MSY"
      ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[MSY]))
    } else if (ref_line == "unfished") {
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="Unfished"])
      ref_line_label <- "unfished"
      ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[unfished]))
    }

    # max_yr <- max(unique(sb$year))
    # SB ts
    plt <- ggplot2::ggplot(data = sb) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = value/1000), linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = ref_line_val/1000, linetype = 2) +
      ggplot2::labs(x = "Year",
                    y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
      ggplot2::scale_x_continuous(n.breaks = round(length(subset(sb, year<=endyr)$year)/10),
                                  guide = ggplot2::guide_axis(minor.ticks = TRUE))
    plt <- plt + ann_add

    plt_fin <- add_theme(plt)

    # Plot of rel. SB
    plt_rel <- ggplot2::ggplot(data = sb) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = value/ref_line_val), linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/ref_line_val - stddev/ref_line_val), ymax = (value/ref_line_val + stddev/ref_line_val)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = ref_line_val/ref_line_val, linetype = 2) +
      ggplot2::labs(x = "Year",
                    y = paste("Spawning Biomass (", sb_unit, ")", sep = "")) +
      ggplot2::scale_x_continuous(n.breaks = round(length(subset(sb, year<=endyr)$year)/10),
                                  guide = ggplot2::guide_axis(minor.ticks = TRUE))

    plt_rel2 <- add_theme(plt_rel)
  } # close SS3 if statement

  if (model == "BAM"){
    # extract spawning biomass from output
    output <- dget(dat)
    sb <- data.frame(sapply(output$t.series, c)) |>
      dplyr::select(year, SSB)

    # Projection years
    proj <- data.frame(sapply(output$proj.t.series, c)) |>
      dplyr::select(year, SSB.proj) |>
      dplyr::rename(SSB = SSB.proj)

    sb2 <- rbind(sb, proj) |>
      dplyr::rename(value = SSB)

    # max_yr <- max(unique(sb2$year))
    if(is.null(endyr)){
      endyr <- output$parms$endyr
    }
    stryr <- output$parms$styr

    # Check if units were declared
    if(!is.null(units)){
      sb_units <- units[1]
      rec_units <- units[2]
      message("Please check the units on your axes are correct. If they are flipped, change the order of names in the units argument.")
    } else {
      sb_units <- output$info$units.ssb
      rec_units <- output$info$units.rec
    }

    # Pull reference pts
    # unfished <- output$parms$SSB0
    # msy <- output$parms$SSBmsy
    # tgt <- output$parms$SSB.F30 # SSBF30 but calling tgt to use same plotting for all

    # Select value for reference line and label
    if (ref_line == "target") {
      ref_line_val <- output$parms$SSB.Fproxy
      ref_line_label <- "target"
      ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[F30])) # this might need to change
    } else if (ref_line == "MSY" | ref_line == "msy") {
      ref_line_val <- output$parms$SSBmsy
      ref_line_label <- "MSY"
      ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[MSY]))
    } else if (ref_line == "unfished") {
      ref_line_val <- output$parms$SSB0
      ref_line_label <- "unfished"
      ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[unfished]))
    }

    plt <- ggplot2::ggplot(data = subset(sb2, year<=endyr)) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = value/1000), linewidth = 1) +
      # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = ref_line_val/1000, linetype = 2) +
      ggplot2::labs(x = "Year",
                    y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
      ggplot2::scale_x_continuous(n.breaks = round(length(subset(sb2, year<=endyr)$year)/10),
                                  guide = ggplot2::guide_axis(minor.ticks = TRUE))
    plt <- plt + ann_add

    plt_fin <- add_theme(plt)

    # plot relative SB
    plt_rel <- ggplot2::ggplot(data = subset(sb2, year<=endyr)) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = value/ref_line_val), linewidth = 1) +
      # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/ref_line_val - stddev/ref_line_val), ymax = (value/ref_line_val + stddev/ref_line_val)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = ref_line_val/ref_line_val, linetype = 2) +
      ggplot2::labs(x = "Year",
                    y = paste("Spawning Biomass (", sb_unit, ")", sep = "")) +
      ggplot2::scale_x_continuous(n.breaks = round(length(subset(sb2, year<=endyr)$year)/10),
                                  guide = ggplot2::guide_axis(minor.ticks = TRUE))
    plt_rel2 <- add_theme(plt_rel)
  }

  return(plt_fin)
}
