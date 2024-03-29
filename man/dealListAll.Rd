% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dealListAll.R
\name{dealListAll}
\alias{dealListAll}
\title{Retrieve ALL 'HubSpot' Current Deals}
\usage{
dealListAll(accessToken)
}
\arguments{
\item{accessToken}{An 'Access Token' can be generated from a 'HubSpot' instance through a private app.}
}
\description{
This function allows you to quickly retrieve all deals from 'HubSpot' via 'API' connection and output them as a dataframe.
}
\examples{
dealListAll <- dealListAll(accessToken = accessToken)
}
