plot_land <- function(dat,
                      model){
  output <- dat |> dplyr::filter(label == "landings")

  narea <- length(unique(output$area))
  nseas <- length(unique(output$season))

  if (narea > 1 | nseas > 1) {

  } else {
    plt <- ggplot2::ggplot(data = output) +
      ggplot2::geom_line(ggplot2::aes(x = time, y = label))+
      ggplot2::facet_wrap(~fleet)
  }
  add_theme(plt)
}
