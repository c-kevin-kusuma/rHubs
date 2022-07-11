#' Delete Contacts on 'HubSpot'
#'
#' This is a DESTRUCTIVE method which is not a best practice because Deals play a central role in the CRM. Typical use case for this method is when the companies are created by your application. This function rely on 'API' connection.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param dealId Values that represent the internal IDs of the deals on your 'HubSpot' instance.
#' @examples output <- deleteDeals(
#'   apiKey = apiKey,
#'   dealId = dealId)
#' @export

deleteDeals <- function(apiKey, dealId) {

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey , '&count=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.', call. = FALSE)}


  deleteList <- list()
  for (i in 1:length(dealId)) {
    hubSpotOutput <- httr::DELETE(url = paste0('https://api.hubapi.com/deals/v1/deal/', dealId[i], '?hapikey=', apiKey))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode %in% c(200,204)) {deleteList[[i]] <- dplyr::tibble(rowid = i, dealId = dealId[i], statusCode = statusCode, status = 'Successful', message = 'The Deal has been deleted')} else {deleteList[[i]] <- dplyr::tibble(rowid = i, dealId = dealId[i], statusCode = statusCode, status = outputContent$status, message = outputContent$message)}
  }

  deleteList <- dplyr::bind_rows(deleteList)

  return(deleteList)
}
