#' Retrieve ALL 'HubSpot' Current Companies
#'
#' This function allows you to quickly retrieve all companies from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param properties A vector of character values corresponds to 'HubSpot' internal names of the properties. Use 'companyProperties()' to check the internal names of the company properties. Defaults to 'c("name", "website")'.
#' @examples contactListAll <- contactListAll(accessToken = "accessToken")
#' @export

contactListAll <- function(accessToken, properties = c("name", "website")) {
  has_more <- 'Yes'
  after <- NA
  contactListAll <- list()

  for (i in 1:9999999999999) {
    if (!is.na(has_more) & is.na(after)) {
      y <- rhubs::contactList(accessToken = accessToken, limit = 100, properties = properties)
      after <- y$after[1]
      has_more <- y$has_more[1]
      contactListAll[[i]] <- y
    }
    else if (!is.na(has_more) & !is.na(after)) {
      y <- rhubs::contactList(accessToken = accessToken, limit = 100, properties = properties, after = after)
      after <- y$after[1]
      has_more <- y$has_more[1]
      contactListAll[[i]] <- y
    }
    else if (is.na(has_more)) {
      break
    }
  }

  contactListAll <- dplyr::bind_rows(contactListAll)
  return(contactListAll)
}
