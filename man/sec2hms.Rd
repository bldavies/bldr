% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sec2hms.R
\name{sec2hms}
\alias{sec2hms}
\alias{min2hms}
\title{Convert decimal times to h:mm:ss format}
\usage{
sec2hms(x, nearest = T)

min2hms(x, nearest = T)
}
\arguments{
\item{x}{Decimal number to be converted.}

\item{nearest}{Whether x should be rounded to the nearest second.
Otherwise x is rounded down.
Default is TRUE.}
}
\value{
A character scalar.
}
\description{
\code{sec2hms} and \code{min2hms} convert decimal numbers of seconds and
  minutes to h:mm:ss format.
  h may exceed 23 and is omitted if h = 0.
}
\examples{
sec2hms(97448.7)  # 27:04:09
min2hms(1624.145)  # 27:04:09

}
