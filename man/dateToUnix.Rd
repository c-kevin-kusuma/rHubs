% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateToUnix.R
\name{dateToUnix}
\alias{dateToUnix}
\title{Convert Date to 'UNIX'}
\usage{
dateToUnix(date, tz = "GMT", type = "Second")
}
\arguments{
\item{date}{The date values or column to be converted to 'UNIX'.}

\item{tz}{Stands for 'Time Zone' which defaults to 'GMT'. This input governs the time zone of the output.}

\item{type}{The granularity of the second can be set using this parameter which defaults to 'Second'. Valid inputs include: 'Millisecond', 'Microsecond', and 'Nanosecond'.}
}
\description{
This function allows you to conveniently convert date values to 'UNIX' values.
}
\examples{
output <- dateToUnix(
  date = date,
  tz = "GMT",
  type = "Millisecond")
}
