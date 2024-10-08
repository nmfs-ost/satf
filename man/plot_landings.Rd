% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_landings.R
\name{plot_landings}
\alias{plot_landings}
\title{Plot observed landings by fleet}
\usage{
plot_landings(dat, units = NULL)
}
\arguments{
\item{dat}{A data frame returned from `asar::convert_output()`.}

\item{units}{indicate the name of the units of landings as to label the axis}
}
\value{
Create a plot ready for a stock assessment report of cumulative landings
over time by fleet.Includes options to plot by fleet, total observed landings
with and without predicted landings. Indicate if fleet should be faceted or on one plot (default). Warning: i
}
\description{
Plot observed landings by fleet
}
