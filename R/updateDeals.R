#' Update Deals' Properties on 'HubSpot'
#'
#' This function allows you to update the properties of deals from a dataframe on 'HubSpot' via 'API' connection.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param data A dataframe that contains the internal IDs of the deals and the properties associated with the deals. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'dealProperties()'.
#' @examples output <- updateDeals(
#'   apiKey = apiKey,
#'   data = data)
#' @export

updateDeals <- function(apiKey, data) {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("tidyr", quietly = TRUE)) {stop("Package \"tidyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("jsonlite", quietly = TRUE)) {stop("Package \"jsonlite\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("lubridate", quietly = TRUE)) {stop("Package \"lubridate\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("stats", quietly = TRUE)) {stop("Package \"stats\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(apiKey) == TRUE) {stop('The required "apiKey" is missing!', call. = FALSE)}
  if ('dealId' %in% colnames(data)) {} else {stop('A required \"dealId\" field that represents the the internal ID of the deal is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey , '&count=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.', call. = FALSE)}

  updateList <- list()
  for (i in 1:nrow(data)) {
    dealId <- data$dealId[i]
    x <- data[i, ] %>% dplyr::select(-dealId) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    pre_json <- x %>% tidyr::pivot_longer(cols = colnames(x), names_to = 'name', values_to = 'value', values_drop_na = TRUE) %>% dplyr::filter(value != '')
    post_json <- jsonlite::toJSON(list(properties = pre_json), pretty = TRUE, auto_unbox = TRUE)

    hubSpotOutput <- httr::PUT(url = paste0('https://api.hubapi.com/deals/v1/deal/', dealId, '?hapikey=', apiKey), body = post_json, httr::add_headers(.headers = c("Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode %in% c(200, 204)) {updateList[[i]] <- dplyr::tibble(rowid = i, dealId = as.character(dealId), statusCode = statusCode, name = 'Relevant properties have been updated',  value = 'Relevant values have been updated', mainMessage = 'Contact has been updated', propertyMessage = 'Successful', propertyError = 'No Error')}
    else if(statusCode == 404) {updateList[[i]] <- dplyr::tibble(rowid = i, dealId = as.character(dealId), statusCode = statusCode, name = '', value = '', mainMessage = outputContent$category,  propertyMessage = outputContent$message, propertyError = outputContent$status)}
    else {
      contactMessage <- dplyr::tibble(rowid = i, dealId = as.character(dealId), statusCode = statusCode, pre_json, mainMessage = outputContent$message) %>% dplyr::mutate(name1 = tolower(name))
      propertyMessage <- outputContent$validationResults %>% rlist::list.stack() %>% dplyr::rename(propertyMessage = message, propertyError = error, name = name) %>% dplyr::select(-name) %>% dplyr::select(-isValid)
      updateList[[i]] <- dplyr::left_join(contactMessage, propertyMessage, by = c('name'='name')) %>% stats::na.omit() }
  }

  updateList <- dplyr::bind_rows(updateList)

  return(updateList)

}
