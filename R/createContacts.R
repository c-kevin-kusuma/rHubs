#' Create or Add New Contacts on 'HubSpot'
#'
#' This function allows you to create or add new contacts from a dataframe to 'HubSpot' via 'API' connection.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param data A dataframe that contains the contact's email and the properties associated with the new contact. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'contactProperties()'.
#' @examples output <- createContacts(
#'   apiKey = apiKey,
#'   data = data)
#' @export

createContacts <- function(apiKey, data) {
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
  if ('email' %in% colnames(data)) {} else {stop('A required \"email\" field that represents the email of the contact is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey , '&properties=name&limit=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.', call. = FALSE)}

  addList <- list()
  for (i in 1:nrow(data)) {
    contactEmail <- data$email[i]
    x <- data[i, ] %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    pre_json <- x %>% tidyr::pivot_longer(cols = colnames(x), names_to = 'property', values_to = 'value', values_drop_na = TRUE) %>% dplyr::filter(value != '')
    post_json <-jsonlite::toJSON(list(properties = pre_json), pretty = TRUE, auto_unbox = TRUE)

    hubSpotOutput <- httr::POST(url = paste0('https://api.hubapi.com/contacts/v1/contact/?hapikey=', apiKey), body = post_json, httr::add_headers(.headers = c("Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 200) {addList[[i]] <- dplyr::tibble(rowid = i, email = contactEmail, vid = as.character(outputContent$vid), statusCode = statusCode, property = 'All properties have been added', value = 'All Values have been added', mainMessage = 'Contact has been added', propertyMessage = 'Successful', propertyError = 'No Error')}
    else if(statusCode == 409) {addList[[i]] <- dplyr::tibble(rowid = i, email = contactEmail, vid = 'No execution', statusCode = statusCode, property = 'Contact already exists', value = 'Contact already exists', mainMessage = outputContent$message, propertyMessage = 'No execution', propertyError = 'No execution')}
    else if(statusCode == 400) {
      contactMessage <- dplyr::tibble(rowid = i, email = contactEmail, vid = 'Not Created', statusCode = statusCode, pre_json, mainMessage = outputContent$message) %>% dplyr::mutate(property1 = tolower(property))
      propertyMessage <- outputContent$validationResults %>% rlist::list.stack() %>% dplyr::rename(propertyMessage = message, propertyError = error, property = name) %>% dplyr::select(-isValid)
      addList[[i]] <- dplyr::left_join(contactMessage, propertyMessage, by = c('property1'='property')) %>% dplyr::select(-property1) %>%  stats::na.omit() }
  }

  addList <- bind_rows(addList)

  return(addList)
}
