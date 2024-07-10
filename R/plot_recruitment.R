plot_recruitment <- function(dat, model){
  if(model == "SS3"){
    plt <- ggplot2::ggplot(data = dat) +
      ggplot2::geom_line(aes(x = time, y = estimate)) +
      ggplot2::geom_errorbar(aes(ymin = c(estimate - uncertainty), ymax = c(estimate + uncertainty)))
  }
}

