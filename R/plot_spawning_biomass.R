#' Plot Spawning Biomass
#'
#' @template dat
#' @template model
#' @param show_warnings Option to suppress warnings
#' @param units If units are not available in the output file or are not the
#' default of metric tons, then state the units of spawning biomass applicable
#' to the stock. For plotting purposes, spawning biomass is divided by 1000.
#' @param biomass_units units for biomass
#' @param spawning_biomass_units units of spawning biomass if different from biomass
#' @param scaled TRUE/FALSE; indicate whether the output values for biomass and recruitment are scaled
#' @param scale_amount indicate the exact amount of scale (i.e. 1000)
#' @param ref_line choose with reference point to plot a reference line and use
#' in relative sb calculations
#' @param end_year input the end year of the stock assessment data (not including
#' projections). This parameter will be deprecated once the output converter is fully developed.
#' @param relative Plot relative spawning biomass. Ref line indicates which reference point to use
#'
#' @return Plot spawning biomass from a stock assessment model as found in a NOAA
#' stock assessment report. Units of spawning biomass can either be manually added
#' or will be extracted from the provided file if possible. In later releases, model will not
#' @export
#'
plot_spawning_biomass <- function(dat,
                         model = c('SS3','BAM', 'ASAP', 'AMAK', 'WHAM', "standard"),
                         show_warnings = FALSE,
                         units = NULL,
                         biomass_units = NULL,
                         spawning_biomass_units = NULL,
                         scaled = FALSE,
                         scale_amount = 1000,
                         ref_line = c("target", "MSY", "msy", "unfished"),
                         end_year = NULL,
                         relative = FALSE
                         ){

  model <- match.arg(model, several.ok = FALSE)

  if(length(ref_line)>1){
    ref_line = "target"
  } else {
    ref_line <- match.arg(ref_line, several.ok = FALSE)
  }

  # check units
  # biomass
  if(!is.null(biomass_units)){
    bu <- biomass_units
  } else {
    bu <- "metric tons"
  }
  # spawning biomass
  if(!is.null(spawning_biomass_units)){
    sbu <- spawning_biomass_units
  } else {
    sbu <- "metric tons"
  }

  if(model == "standard"){
    output <- read.csv(dat)
    sb <- output |>
      dplyr::filter(label == "spawning_biomass",
                    module_name == "DERIVED_QUANTITIES" | module_name == "") |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year))
    if (is.null(end_year)){
      endyr <- max(sb$year)
    }
    # Select value for reference line and label
    if (any(grepl("target", output$label))) {
      ref_line_val <- as.numeric(output[grep("(?=.*spawning_biomass)(?=.*target)", output$label, perl = TRUE),]$estimate)
      ref_line_label <- "target"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val, label = bquote(SB[target])) # this might need to change
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val/1000, label = bquote(SB[target]))
      }
    } else if (ref_line == "MSY" | ref_line == "msy") {
      ref_line_val <- as.numeric(output[grep("(?=.*spawning_biomass)(?=.*msy)", output$label, perl = TRUE),]$estimate)
      ref_line_label <- "MSY"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val, label = bquote(SB[MSY]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val/1000, label = bquote(SB[MSY]))
      }
    } else if (ref_line == "unfished") {
      ref_line_val <- as.numeric(output[grep("(?=.*spawning_biomass)(?=.*target)", output$label, perl = TRUE),]$estimate)
      ref_line_label <- "unfished"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val, label = bquote(SB[unfished]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val/1000, label = bquote(SB[unfished]))
      }
    }
    # Choose number of breaks for x-axis
    x_n_breaks <- round(length(subset(sb, year<=endyr)$year)/10)
    if (x_n_breaks <= 5) {
      x_n_breaks <- round(length(subset(sb, year<=endyr)$year)/5)
    }
    if (relative) {
    # plot relative SB
      plt <- ggplot2::ggplot(data = subset(sb, year<=endyr)) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = estimate/ref_line_val), linewidth = 1) +
        # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/ref_line_val - stddev/ref_line_val), ymax = (value/ref_line_val + stddev/ref_line_val)), colour = "grey", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = ref_line_val/ref_line_val, linetype = 2) +
        ggplot2::labs(x = "Year",
                      y = paste("Spawning Biomass (", sbu, ")", sep = "")) +
        ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                    guide = ggplot2::guide_axis(minor.ticks = TRUE))
    } else {
      if(scaled){
        plt <- ggplot2::ggplot(data = sb) +
          ggplot2::geom_line(ggplot2::aes(x = year, y = estimate), linewidth = 1) +
          ggplot2::geom_hline(yintercept = ref_line_val, linetype = 2) +
          ggplot2::labs(x = "Year",
                        y = paste("Spawning Biomass (", sbu, ")", sep = "")) +
          ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                      guide = ggplot2::guide_axis(minor.ticks = TRUE))
        plt <- plt + ann_add
      } else {
        plt <- ggplot2::ggplot(data = sb) +
          ggplot2::geom_line(ggplot2::aes(x = year, y = estimate/1000), linewidth = 1) +
          # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
          ggplot2::geom_hline(yintercept = ref_line_val/1000, linetype = 2) +
          ggplot2::labs(x = "Year",
                        y = paste("Spawning Biomass (", sbu, ")", sep = "")) +
          ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                      guide = ggplot2::guide_axis(minor.ticks = TRUE))
        plt <- plt + ann_add
      }
    }
    plt_fin <- add_theme(plt)
  } else if (model == "SS3") {
    # load SS3 data file
    # check if dat parameter is a report file or a mutates df
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
        colClasses = "character", nrows = -1, comment.char = "",
        blank.lines.skip = FALSE
      )
    } else {
      output <- dat
    }

    bio_info <- SS3_extract_df(output, "DERIVED_QUANTITIES")[-c(1:4),]
    colnames(bio_info) <- bio_info[1,]
    bio_info <- bio_info[-1,] |>
      dplyr::mutate(year = stringr::str_extract(Label, "[0-9]+$"),
                    Label = stringr::str_remove(Label, "_[0-9]+$"))

    sb <- bio_info |>
      dplyr::filter(Label == "SSB") |>
      dplyr::mutate(Value = as.numeric(Value),
                    year = as.numeric(year),
                    StdDev = as.numeric(StdDev))
    if(is.null(end_year)){
      endyr <- max(sb$year)
    }

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
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['Label']]=="SSB_Btgt"])
      ref_line_label <- "target"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[Btarget]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[Btarget]))
      }
    } else if (ref_line == "MSY" | ref_line == "msy") {
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['Label']]=="SSB_MSY"])
      ref_line_label <- "MSY"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[MSY]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[MSY]))
      }
    } else if (ref_line == "unfished") {
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['Label']]=="SSB_unfished"])
      ref_line_label <- "unfished"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[unfished]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[unfished]))
      }
    } else if (ref_line == "spr") {
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['Label']]=="SSB_SPR"])
      ref_line_label <- "SPR"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[SPR]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[SPR]))
      }
    } else {
      ref_line_name <- paste("SSB_", ref_line, sep = "")
      ref_line_val <- as.numeric(bio_info[['Value']][bio_info[['Label']]==ref_line_name])
      ref_line_label <- ref_line
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[ref]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[ref]))
      }
    }
    # Choose number of breaks for x-axis
    x_n_breaks <- round(length(subset(sb, year<=endyr)$year)/10)
    if (x_n_breaks <= 5) {
      x_n_breaks <- round(length(subset(sb, year<=endyr)$year)/5)
    }
    # Plot of rel. SB
    if (relative) {
      plt <- ggplot2::ggplot(data = subset(sb, year <= endyr)) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = Value/ref_line_val), linewidth = 1) +
        ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (Value/ref_line_val - StdDev/ref_line_val), ymax = (Value/ref_line_val + StdDev/ref_line_val)), colour = "grey", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = ref_line_val/ref_line_val, linetype = 2) +
        ggplot2::labs(x = "Year",
                      y = paste("Rel. Spawning Biomass", sep = "")) +
        ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                    guide = ggplot2::guide_axis(minor.ticks = TRUE))
    } else {
      if (scaled) {
        plt <- ggplot2::ggplot(data = subset(sb, year < endyr + 1)) +
          ggplot2::geom_line(ggplot2::aes(x = year, y = Value), linewidth = 1) +
          ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (Value - StdDev), ymax = (Value + StdDev)), colour = "grey", alpha = 0.3) +
          ggplot2::geom_hline(yintercept = ref_line_val, linetype = 2) +
          ggplot2::labs(x = "Year",
                        y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
          ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                      guide = ggplot2::guide_axis(minor.ticks = TRUE))
      } else {
        plt <- ggplot2::ggplot(data = subset(sb, year < endyr + 1)) +
          ggplot2::geom_line(ggplot2::aes(x = year, y = Value/1000), linewidth = 1) +
          ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (Value/1000 - StdDev/1000), ymax = (Value/1000 + StdDev/1000)), colour = "grey", alpha = 0.3) +
          ggplot2::geom_hline(yintercept = ref_line_val/1000, linetype = 2) +
          ggplot2::labs(x = "Year",
                        y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
          ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                      guide = ggplot2::guide_axis(minor.ticks = TRUE))
      }
      plt <- plt + ann_add
    }
    plt_fin <- add_theme(plt)
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
    if(is.null(end_year)){
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
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[F30]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[F30])) # this might need to change
      }
    } else if (ref_line == "MSY" | ref_line == "msy") {
      ref_line_val <- output$parms$SSBmsy
      ref_line_label <- "MSY"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[MSY]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[MSY]))
      }
    } else if (ref_line == "unfished") {
      ref_line_val <- output$parms$SSB0
      ref_line_label <- "unfished"
      if (scaled) {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val, label = bquote(SB[unfished]))
      } else {
        ann_add <- ggplot2::annotate("text", x = endyr+0.5, y=ref_line_val/1000, label = bquote(SB[unfished]))
      }
    }
    # Choose number of breaks for x-axis
    x_n_breaks <- round(length(subset(sb2, year<=endyr)$year)/10)
    if (x_n_breaks <= 5) {
      x_n_breaks <- round(length(subset(sb2, year<=endyr)$year)/5)
    }
    if (relative) {
      # plot relative SB
      plt <- ggplot2::ggplot(data = subset(sb2, year<=endyr)) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = value/ref_line_val), linewidth = 1) +
        # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/ref_line_val - stddev/ref_line_val), ymax = (value/ref_line_val + stddev/ref_line_val)), colour = "grey", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = ref_line_val/ref_line_val, linetype = 2) +
        ggplot2::labs(x = "Year",
                      y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
        ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                    guide = ggplot2::guide_axis(minor.ticks = TRUE))
    } else {
      if (scaled) {
        plt <- ggplot2::ggplot(data = subset(sb2, year<=endyr)) +
          ggplot2::geom_line(ggplot2::aes(x = year, y = value), linewidth = 1) +
          # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
          ggplot2::geom_hline(yintercept = ref_line_val, linetype = 2) +
          ggplot2::labs(x = "Year",
                        y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
          ggplot2::scale_x_continuous(n.breaks = xn_n_breaks,
                                      guide = ggplot2::guide_axis(minor.ticks = TRUE))
      } else {
        plt <- ggplot2::ggplot(data = subset(sb2, year<=endyr)) +
          ggplot2::geom_line(ggplot2::aes(x = year, y = value/1000), linewidth = 1) +
          # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
          ggplot2::geom_hline(yintercept = ref_line_val/1000, linetype = 2) +
          ggplot2::labs(x = "Year",
                        y = paste("Spawning Biomass (", sb_units, ")", sep = "")) +
          ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                      guide = ggplot2::guide_axis(minor.ticks = TRUE))
      }
      plt <- plt + ann_add
    }
    plt_fin <- suppressWarnings(add_theme(plt))
  }
  return(plt_fin)
}
