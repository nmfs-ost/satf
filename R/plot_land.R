plot_land <- function(dat){
  output <- dat |> filter(label == "landings")

  narea <- length(unique(output$area))
  nseas <- length(unique(output$season))

  if (narea > 1 | nseas > 1) {

  } else {
    plt <- ggplot(data = output) +
      geom_line(aes(x = time, y = label))+
      facet_wrap(~fleet)
  }
  add_theme(plt)
}
