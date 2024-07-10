plot_land <- function(output){
  dat <- output |> filter(label == "landings")

  narea <- length(unique(dat$area))
  nseas <- length(unique(dat$season))

  if (narea > 1 | nseas > 1) {

  } else {
    plt <- ggplot(data = dat) +
      geom_line(aes(x = time, y = label))+
      facet_wrap(~fleet)
  }
  add_theme(plt)
}
