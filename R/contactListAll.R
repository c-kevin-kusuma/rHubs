#' Retrieve ALL 'HubSpot' Current Contacts
#'
#' This function allows you to quickly retrieve all contacts from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @examples contactListAll <- contactListAll(apiKey = "demo")
#' @export

contactListAll <- function(apiKey) {
  offset <- NA
  has_more <- TRUE
  contactListAll <- list()

  for (i in 1:9999999) {
    if (is.na(offset) == TRUE & has_more == TRUE) {
      y <- rHubs::contactList(apiKey = apiKey)
      offset <- y$offset[1]
      has_more <- y$has_more[1]
      contactListAll[[i]] <- y
    }
    else if (is.na(offset) == FALSE & has_more == TRUE) {
      y <- rHubs::contactList(apiKey = apiKey, offset = offset)
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
