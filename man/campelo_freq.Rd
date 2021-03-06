% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/false_ring_proportion.R
\name{campelo_freq}
\alias{campelo_freq}
\title{iadf frequency per ring width class}
\usage{
campelo_freq(iadf, rwl, n = 20)
}
\arguments{
\item{iadf}{A data frame with numeric columns representing individual series
and years as rownames where years with IADF are marked binary with 1,
those without with 0, years not covered by the series are set to NA.}

\item{rwl}{data frame containing ring widths with years in rows and
series in columns}

\item{n}{number of ring width classes}
}
\value{
a data frame
}
\description{
Calculate the frequency per ring width class as suggested
 by Campelo (2015).
}
\examples{
data('example_iadf')
data('example_rwl')
model <- campelo_chapman(campelo_freq(example_iadf, example_rwl))
campelo_index(example_iadf, example_rwl, model)
}
\references{
Campelo, F., Vieira, J., Battipaglia, G. et al. Which matters most for the
  formation of intra-annual density fluctuations in Pinus pinaster: age or
  size? Trees (2015) 29: 237. doi:10.1007/s00468-014-1108-9
}
\seealso{
\code{\link{campelo_chapman}}, \code{\link{campelo_index}}
}
