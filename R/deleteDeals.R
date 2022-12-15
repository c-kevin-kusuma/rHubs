#' Delete Contacts on 'HubSpot'
#'
#' This is a DESTRUCTIVE method which is not a best practice because Deals play a central role in the CRM. Typical use case for this method is when the companies are created by your application. This function rely on 'API' connection.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param dealId Values that represent the internal IDs of the deals on your 'HubSpot' instance.
#' @examples output <- deleteDeals(
#'   accessToken = accessToken,
#'   dealId = dealId)
#' @export

deleteDeals <- function(accessToken, dealId) {

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=10&archived=false', '&properties=name&limit=1'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}


  deleteList <- list()
  for (i in 1:length(dealId)) {
    hubSpotOutput <- httr::DELETE(url = paste0('https://api.hubapi.com/crm/v3/objects/deals/', dealId), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken), "Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode %in% c(204)) {deleteList[[i]] <- dplyr::tibble(rowid = i, dealId = dealId[i], statusCode = statusCode, message = 'The Deal has been deleted')} else {deleteList[[i]] <- dplyr::tibble(rowid = i, dealId = dealId[i], statusCode = statusCode, message = outputContent$message)}
  }

  deleteList <- dplyr::bind_rows(deleteList)

  return(deleteList)
}
