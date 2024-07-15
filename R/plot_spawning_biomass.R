#' Plot Spawning Biomass
#'
#' @template dat
#' @template model
#' @param refpts Output a dataframe of reference points for spawning biomass
#' rather than plot. Default is FALSE.
#' @param warnings Option to suppress warnings
#' @param units If units are not available in the output file or are not the
#' default of metric tons, then state the units of spawning biomass applicable
#' to the stock. For plotting purposes, spawning biomass is divided by 1000.
#'
#' @return Plot spawning biomass from a stock assessment model as found in a NOAA
#' stock assessment report. Units of spawning biomass can either be manually added
#' or will be extracted from the provided file if possible.
#' @export
#'
#' @examples plot_spawning_biomass(dat = "spp.rdat",model = "BAM")
plot_spawning_biomass <- function(dat,
                         model = c('SS3','BAM', 'ASAP', 'AMAK'),
                         refpts = FALSE,
                         warnings = FALSE,
                         units = NULL) {
  # if(warnings){
  #
  # } else {
  #   suppressWarnings()
  # }

  model <- match.arg(model, several.ok = FALSE)

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
      dplyr::mutate(year = as.numeric(year),
                    value = as.numeric(Value),
                    stddev = as.numeric(StdDev)) |>
      dplyr::select(year, value, stddev) |>
      dplyr::filter(year!='Virgin' & year!="Initial")

    # Extract reference points, Btarget, Unfished biomass, and Bmsy
    tgt <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="Btgt"])
    unfished <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="Unfished"])
    msy <- as.numeric(bio_info[['Value']][bio_info[['label']]=="SSB" & bio_info[['year']]=="MSY"])

    # sbr <- SS3_extract_df(dat, "Spawning_Biomass_Report_1")[-c(1:3),]
    # colnames(sbr) <- c("year", "era", "value")
    # sbr <- sbr |>
    #   dplyr::filter(era!='VIRG' & era!="INIT") |>
    #   dplyr::mutate(year = as.numeric(year),
    #                 value = as.numeric(value))
    # ggplot2::ggplot(dat = sbr) +
    #   ggplot2::geom_line(ggplot2::aes(x = year, y = value/1000, color = era), linewidth = 1)

    max_yr <- max(unique(sb$year))
    plt <-ggplot2::ggplot(data = sb) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = value/1000), linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = unfished/1000, linetype = 2) +
      ggplot2::annotate("text", x = max_yr+0.5, y=unfished/1000, label = "SB_unfished") +
      ggplot2::geom_hline(yintercept = tgt/1000, linetype = 3) +
      ggplot2::annotate("text", x = max_yr+0.5, y=tgt/1000, label = "SBtarg") +
      ggplot2::geom_hline(yintercept = msy/1000, linetype = 4) +
      ggplot2::annotate("text", x = max_yr+0.5, y=msy/1000, label = "SBmsy") +
      ggplot2::labs(x = "Year",
                    y = paste("Spawning Biomass (", sb_unit, ")", sep = "")) +
      ggplot2::theme_classic()
  } # close SS3 if statement

  if (model == "BAM"){
    # extract spawning biomass from output
    output <- dget(dat)
    sb <- data.frame(sapply(output$t.series, c))|>
      dplyr::select(year, SSB)

    # Projection years
    proj <- data.frame(sapply(output$projection, c)) |>
      dplyr::select(year, SSB.proj) |>
      dplyr::rename(SSB = SSB.proj)

    sb2 <- rbind(sb, proj) |>
      dplyr::rename(value = SSB)

    # Pull reference pts
    unfished <- output$parms$SSB0
    msy <- output$parms$SSBmsy
    tgt <- output$parms$SSB.F30 # SSBF30 but calling tgt to use same plotting for all

    max_yr <- max(unique(sb2$year))

    plt <- ggplot2::ggplot(data = sb2) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = value/1000), linewidth = 1) +
      # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = (value/1000 - stddev/1000), ymax = (value/1000 + stddev/1000)), colour = "grey", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = unfished/1000, linetype = 2) +
      ggplot2::annotate("text", x = max_yr+0.5, y=unfished/1000, label = "SB_unfished") +
      ggplot2::geom_hline(yintercept = tgt/1000, linetype = 3) +
      ggplot2::annotate("text", x = max_yr+0.5, y=tgt/1000, label = "SBtarg") +
      ggplot2::geom_hline(yintercept = msy/1000, linetype = 4) +
      ggplot2::annotate("text", x = max_yr+0.5, y=msy/1000, label = "SBmsy") +
      ggplot2::labs(x = "Year",
                    y = paste("Spawning Biomass (", sb_unit, ")", sep = "")) +
      ggplot2::theme_classic()

  }

  return(plt)
}
