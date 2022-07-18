#' Retrieve ALL 'HubSpot' Current Contacts
#'
#' This function allows you to quickly retrieve all contacts from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'contactProperties()' to check the internal names of the company properties. Defaults to 'c("firstname", "lastname", "email", "company")'.
#' @examples contactListAll <- contactList(
#'   apiKey = "demo",
#'   properties = c("firstname", "lastname", "email", "company"))
#' @export

contactListAll <- function(apiKey, properties = c("firstname", "lastname", "email", "company")) {
  offset <- NA
  has_more <- TRUE
  contactListAll <- list()

  for (i in 1:9999999) {
    if (is.na(offset) == TRUE & has_more == TRUE) {
      y <- companyList(apiKey = apiKey, properties = properties)
      offset <- y$offset[1]
      has_more <- y$has_more[1]
      contactListAll[[i]] <- y
    }
    else if (is.na(offset) == FALSE & has_more == TRUE) {
      y <- companyList(apiKey = apiKey, offset = offset, properties = properties)
      offset <- y$offset[1]
      has_more <- y$has_more[1]
      contactListAll[[i]] <- y
    }
    else if (has_more == FALSE) {
      break
    }
  }

  contactListAll <- dplyr::bind_rows(contactListAll)


  return(contactListAll)
}
