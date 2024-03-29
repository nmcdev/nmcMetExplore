% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obs.R
\name{draw_rain_china}
\alias{draw_rain_china}
\title{Display total rain observation over China.}
\usage{
draw_rain_china(
  totalRain,
  nx = 128,
  ny = 76,
  myat = seq(0, 2400, by = 300),
  xlim = c(72, 136),
  ylim = c(16, 54),
  colors = NULL,
  main = NULL,
  interpKrige = FALSE,
  dgrid = 0.5,
  imageFile = NULL
)
}
\arguments{
\item{totalRain, }{data.frame, {lon, lat, totalRain}}

\item{nx, }{interpolated grid nx}

\item{ny, }{interpolated grid ny}

\item{myat, }{contour levels}

\item{xlim, }{map longitude limit}

\item{ylim, }{map latitude limit}

\item{colors, }{color pallete}

\item{main, }{plot titile}

\item{interpKrige, }{interpolate totalRain by krige method (slowly)}

\item{dgrid, }{grid interval}

\item{imageFile, }{output image file}
}
\description{
Display total rain observation over China.
}
\examples{
  display_total_rain_china(totalRain)

}
