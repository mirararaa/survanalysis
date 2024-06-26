% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rect.R
\name{rect}
\alias{rect}
\title{Calculate rectangularity of a survival curve}
\usage{
rect(data)
}
\arguments{
\item{data}{Data frame containing lifespan data and optional censoring data.}
}
\value{
A data frame containing rectangularity metrics.
}
\description{
Calculate rectangularity of a survival curve
}
\examples{
data <- data.frame('Censoring'=c(1,0,0,0),
  'Lifespan'=c(2,7,8,3))
rect(data)
}
