% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backshift.R
\name{backshift}
\alias{backshift}
\title{Shift demand to the past}
\usage{
backshift(input_vct, horizon, storage = list(), fit = ~1 * .demand)
}
\arguments{
\item{input_vct}{raw consumption}

\item{horizon}{how much do you look back}

\item{storage}{list of R6 class storage}

\item{fit}{formula to calulate the fit of the flexible demand}
}
\value{
object 
\describe{
  \item{demand fixed}{First item}
  \item{demand flex}{Second item}
  \item{fit curve initial}{Second item}
  \item{fit curve final}{Second item}   
}
}
\description{
Shift demand to the past
}
\examples{
1+1
}
