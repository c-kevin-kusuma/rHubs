% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleteCompanies.R
\name{deleteCompanies}
\alias{deleteCompanies}
\title{Delete Companies on 'HubSpot'}
\usage{
deleteCompanies(apiKey, companyId)
}
\arguments{
\item{apiKey}{An 'API' key that can be generated from a 'HubSpot' instance.}

\item{companyId}{Values that represent the internal IDs of the companies on your 'HubSpot' instance.}
}
\description{
This is a DESTRUCTIVE method which is not a best practice because companies play a central role in the CRM. Typical use case for this method is when the companies are created by your application. This function rely on 'API' connection.
}
\examples{
output <- deleteCompanies(
  apiKey = apiKey,
  companyId = companyId)
}
