#' Retrieve ALL 'HubSpot' Current Deals
#'
#' This function allows you to quickly retrieve all deals from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @examples dealListAll <- dealListAll(accessToken = accessToken)
#' @export

dealListAll <- function(accessToken) {
  after <- NA
  has_more <- TRUE
  dealListAll <- list()

  for (i in 1:9999999) {
    if (is.na(offset) == TRUE & has_more == TRUE) {
      y <- rHubs::dealList(accessToken = accessToken)
      after <- y$after[1]
      has_more <- y$has_more[1]
      dealListAll[[i]] <- y
    }
    else if (is.na(after) == FALSE & has_more == TRUE) {
      y <- rHubs::dealList(accessToken = accessToken, after = after)
      after <- y$after[1]
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
