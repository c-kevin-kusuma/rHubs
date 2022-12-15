#' Update Contacts' Properties on 'HubSpot'
#'
#' This function allows you to update the properties of contacts from a dataframe on 'HubSpot' via 'API' connection.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param data A dataframe that contains the internal IDs of the contacts and the properties associated with the contacts. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'contactProperties()'.
#' @examples output <- updateContacts(
#'   accessToken = accessToken,
#'   data = data)
#' @export

updateContacts <- function(accessToken, data) {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rjson", quietly = TRUE)) {stop("Package \"rjson\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(accessToken) == TRUE) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if ('hs_object_id' %in% colnames(data)) {} else {stop('A required \"hs_object_id\" field that represents the the internal ID of the contact is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=10&archived=false', '&properties=name&limit=1'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}

  updateList <- list()
  for (i in 1:nrow(data)) {
    hs_object_id <- data$hs_object_id[i]
    x <- data[i, ] %>% dplyr::select(-hs_object_id) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    json <- rjson::toJSON(list(properties = x))

    hubSpotOutput <- httr::POST(url = paste0('https://api.hubapi.com/crm/v3/objects/contacts', hs_object_id), body = json, httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken), "Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 200) {updateList[[i]] <- dplyr::tibble(rowid = i, hs_object_id = as.character(hs_object_id), statusCode = as.character(statusCode), message = 'Successful')}
    else {updateList[[i]] <- dplyr::tibble(rowid = i, hs_object_id = as.character(hs_object_id), statusCode = 'Error', message = outputContent$message)}
  }

  updateList <- dplyr::bind_rows(updateList)

  return(updateList)

}
