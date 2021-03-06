% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleteContacts.R
\name{deleteContacts}
\alias{deleteContacts}
\title{Delete Contacts on 'HubSpot'}
\usage{
deleteContacts(apiKey, vid)
}
\arguments{
\item{apiKey}{An 'API' key that can be generated from a 'HubSpot' instance.}

\item{vid}{Values that represent the internal IDs of the contacts on your 'HubSpot' instance.}
}
\description{
This is a DESTRUCTIVE method which is not a best practice because contacts play a central role in the CRM. Typical use case for this method is when the companies are created by your application. This function rely on 'API' connection.
}
\examples{
output <- deleteCompanies(
  apiKey = apiKey,
  vid = vid)
}
