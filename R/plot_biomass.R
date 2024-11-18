#' Plot Total Biomass
#'
#' @inheritParams plot_recruitment
#' @param show_warnings Option to suppress warnings
#' @param units units for biomass
#' @param scaled TRUE/FALSE; indicate whether the output values for biomass and recruitment are scaled
#' @param scale_amount indicate the exact amount of scale (i.e. 1000)
#' @param ref_line choose with reference point to plot a reference line and use
#' in relative totb calculations
#' @param end_year input the end year of the stock assessment data (not including
#' projections). This parameter will be deprecated once the output converter is fully developed.
#' @param relative Plot relative total biomass. Ref line indicates which reference point to use
#'
#' @return Plot total biomass from a stock assessment model as found in a NOAA
#' stock assessment report. Units of total biomass can either be manually added
#' or will be extracted from the provided file if possible. In later releases, model will not
#' @export
#'
plot_total_biomass <- function(dat,
                               show_warnings = FALSE,
                               units = NULL,
                               scaled = FALSE,
                               scale_amount = 1000,
                               ref_line = c("target", "MSY", "msy", "unfished"),
                               end_year = NULL,
                               relative = FALSE,
                               make_rda = FALSE,
                               rda_dir = getwd()
){

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "biomass"
  fig_or_table <- "figure"

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table)

  if(length(ref_line)>1){
    ref_line = "target"
  } else {
    ref_line <- match.arg(ref_line, several.ok = FALSE)
  }

  # check units
  if(!is.null(units)){
    bu <- units
  } else {
    bu <- "metric tons"
  }

  output <- dat
  totb <- output |>
    dplyr::filter(label == "biomass",
                  module_name == "DERIVED_QUANTITIES" | module_name == "t.series") |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year))
  if (is.null(end_year)){
    endyr <- max(totb$year)
  }
  # Select value for reference line and label
  # update the target option later
  if (any(grepl("target", output$label))) {
    ref_line_val <- as.numeric(output[grep("(?=.*biomass)(?=.*target)", output$label, perl = TRUE),]$estimate)
    ref_line_label <- "target"
    if (scaled) {
      ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val, label = bquote(B[target])) # this might need to change
    } else {
      ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val/1000, label = bquote(B[target]))
    }
  } else if (ref_line == "MSY" | ref_line == "msy") {
    ref_line_val <- as.numeric(output[grep("(^biomass_msy)", output$label, perl = TRUE),]$estimate)
    ref_line_label <- "MSY"
    if (scaled) {
      ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val, label = bquote(B[ref_line]))
    } else {
      ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val/1000, label = bquote(B[MSY]))
    }
  } else if (ref_line == "unfished") {
    ref_line_val <- as.numeric(output[grep("(^biomass_unfished)", output$label, perl = TRUE),]$estimate)
    ref_line_label <- "unfished"
    if (scaled) {
      ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val, label = bquote(B[unfished]))
    } else {
      ann_add <- ggplot2::annotate("text", x = endyr + 0.05, y=ref_line_val/1000, label = bquote(B[unfished]))
    }
  }
  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(subset(totb, year<=endyr)$year)/10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(subset(totb, year<=endyr)$year)/5)
  }
  if (relative) {
    # plot relative TOTB
    plt <- ggplot2::ggplot(data = subset(totb, year<=endyr)) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = estimate/ref_line_val), linewidth = 1) +
      # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/ref_line_val - stddev/ref_line_val), ymax = (value/ref_line_val + stddev/ref_line_val)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = ref_line_val/ref_line_val, linetype = 2) +
      ggplot2::labs(x = "Year",
                    y = paste("Biomass (", bu, ")", sep = "")) +
      ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                  guide = ggplot2::guide_axis(minor.ticks = TRUE))
  } else {
    if(scaled){
      plt <- ggplot2::ggplot(data = totb) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = estimate), linewidth = 1) +
        ggplot2::geom_hline(yintercept = ref_line_val, linetype = 2) +
        ggplot2::labs(x = "Year",
                      y = paste("Biomass (", bu, ")", sep = "")) +
        ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                    guide = ggplot2::guide_axis(minor.ticks = TRUE))
      plt <- plt + ann_add
    } else {
      plt <- ggplot2::ggplot(data = totb) +
        ggplot2::geom_line(ggplot2::aes(x = year, y = estimate/1000), linewidth = 1) +
        # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = ref_line_val/1000, linetype = 2) +
        ggplot2::labs(x = "Year",
                      y = paste("Biomass (", bu, ")", sep = "")) +
        ggplot2::scale_x_continuous(n.breaks = x_n_breaks,
                                    guide = ggplot2::guide_axis(minor.ticks = TRUE))
      plt <- plt + ann_add
    }
  }
  plt_fin <- add_theme(plt)

  # add alt text and caption
  plt_fin <- plt_fin +
    ggplot2::labs(caption = caps_alttext[[1]],
                  alt = caps_alttext[[2]]
    )

  # export figure to rda if argument = T
  if (make_rda == TRUE){

    export_rda(plt_fin = plt_fin,
               caps_alttext = caps_alttext,
               rda_dir = rda_dir,
               topic_label = topic_label,
               fig_or_table = fig_or_table)

  }

  return(plt_fin)
}
