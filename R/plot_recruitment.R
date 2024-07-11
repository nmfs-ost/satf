plot_recruitment <- function(dat,
                             model,
                             params = FALSE,
                             params_only = FALSE){
  if(model == "SS3"){
    # extract recruitment
    sr_info <- SS3_extract_df(dat, "SPAWN_RECRUIT")
    sr <- sr_info[-c(1:13),]
    colnames(sr) <- sr_info[12,]
    sr <- sr  |>
      dplyr::mutate(spawn_bio = as.numeric(spawn_bio),
                    exp_recr = as.numeric(exp_recr),
                    with_env = as.numeric(with_env),
                    adjusted = as.numeric(adjusted),
                    pred_recr = as.numeric(pred_recr),
                    dev = as.numeric(dev),
                    biasadj = as.numeric(biasadj))


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

    # Export message
    message(cat("Stock Recrutiment Fxn: ", sr_fxn, "\n", # need to add conversion to what this number means to analyst
                "    ", "    ", "   ", "ln(R0): ", sr_vars$value[sr_vars$variable=="Ln(R0)"], "\n",
                "     ", "      ", "     ", "h: ", sr_vars$value[sr_vars$variable=="steep"], "\n",
                "    ", "    ", "   ", "sigmaR: ", sr_vars$value[sr_vars$variable=="sigmaR"], "\n",
                "    ", "    ", " ", "env_link: ", sr_vars$value[sr_vars$variable=="env_link_"], "\n",
                "    ", "    ", "  ", "init_eq: ", sr_vars$value[sr_vars$variable=="init_eq"], "\n"
                )
            )


    # sr_plt <-
    sr2 <- sr |> dplyr::filter(year!="Init" & year!="Virg")

    # need to rescale to 1000s rather than xe..
    sr_plt <- ggplot2::ggplot(data = sr2) +
      ggplot2::geom_line(ggplot2::aes(x = spawn_bio, y = exp_recr)) + # exp. R
      # add exp R after bias adjustment (dotted line)
      ggplot2::geom_point(ggplot2::aes(x = spawn_bio, y = pred_recr, color = year))+
      ggplot2::geom_text()
      ggplot2::labs(x = "Spawning Biomass",
           y = "Recruitment") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none")
  }
}

