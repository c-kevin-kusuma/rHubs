% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleteDeals.R
\name{deleteDeals}
\alias{deleteDeals}
\title{Delete Contacts on 'HubSpot'}
\usage{
deleteDeals(accessToken, dealId)
}
\arguments{
\item{accessToken}{An 'Access Token' can be generated from a 'HubSpot' instance through a private app.}

\item{dealId}{Values that represent the internal IDs of the deals on your 'HubSpot' instance.}
}
\description{
This is a DESTRUCTIVE method which is not a best practice because Deals play a central role in the CRM. Typical use case for this method is when the companies are created by your application. This function rely on 'API' connection.
}
\examples{
output <- deleteDeals(
  accessToken = accessToken,
  dealId = dealId)
}
