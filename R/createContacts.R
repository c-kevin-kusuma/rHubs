#' Create or Add New Contacts on 'HubSpot'
#'
#' This function allows you to create or add new contacts from a dataframe to 'HubSpot' via 'API' connection.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param data A dataframe that contains the contact's email and the properties associated with the new contact. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'contactProperties()'.
#' @examples output <- createContacts(
#'   accessToken = accessToken,
#'   data = data)
#' @export

createContacts <- function(accessToken, data) {
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
  if (is.na(accessToken) == TRUE) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if ('email' %in% colnames(data)) {} else {stop('A required \"email\" field that represents the email of the contact is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=1&archived=false&properties=name'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}

  addList <- list()
  for (i in 1:nrow(data)) {
    svMisc::progress(i*100/nrow(data))
    email <- data$email[i]
    x <- data[i, ] %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    json <- rjson::toJSON(list(properties = x))
    hubSpotOutput <- httr::POST(url = 'https://api.hubapi.com/crm/v3/objects/contacts', body = json, httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken), "Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 201) {addList[[i]] <- dplyr::tibble(rowid = i, id = as.character(outputContent$id), email = email, statusCode = statusCode, message = 'Successful')}
    else {addList[[i]] <- dplyr::tibble(rowid = i, id = 'Execution Failed', email = email, statusCode = statusCode, message = outputContent$message)}
    if (i == nrow(data)) cat("Done!\n")
  }

  addList <- dplyr::bind_rows(addList)

  return(addList)
}
