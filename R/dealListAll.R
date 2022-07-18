#' Retrieve ALL 'HubSpot' Current Deals
#'
#' This function allows you to quickly retrieve all deals from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @examples dealListAll <- dealListAll(apiKey = "demo")
#' @export

dealListAll <- function(apiKey) {
  offset <- NA
  has_more <- TRUE
  dealListAll <- list()

  for (i in 1:9999999) {
    if (is.na(offset) == TRUE & has_more == TRUE) {
      y <- rHubs::contactList(apiKey = apiKey)
      offset <- y$offset[1]
      has_more <- y$has_more[1]
      dealListAll[[i]] <- y
    }
    else if (is.na(offset) == FALSE & has_more == TRUE) {
      y <- rHubs::contactList(apiKey = apiKey, offset = offset)
      offset <- y$offset[1]
      has_more <- y$has_more[1]
      dealListAll[[i]] <- y
    }
    else if (has_more == FALSE) {
      break
    }
  }

  dealListAll <- dplyr::bind_rows(dealListAll)
  return(dealListAll)
}
