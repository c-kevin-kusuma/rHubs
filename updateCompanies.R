#' Update Companies' Properties on 'HubSpot'
#'
#' This function allows you to update the properties of companies on 'HubSpot' via 'API' connection.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param data A dataframe that contains the company's internal IDs and the properties associated with the companies. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'companyProperties()'.
#' @examples output <- updateCompanies(
#'   apiKey = apiKey,
#'   data = data)
#' @export

updateCompanies <- function(apiKey, data){
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
  if ('companyId' %in% colnames(data)) {} else {stop('A required \"companyId\" field that represents the internal ID of the company is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey , '&properties=name&limit=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.', call. = FALSE)}

  updateList <- list()
  for (i in 1:nrow(data)) {
    companyId <- data$companyId[i]
    x <- data[i, ] %>% dplyr::select(-companyId) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    pre_json <- x %>% tidyr::pivot_longer(cols = colnames(x), names_to = 'name', values_to = 'value', values_drop_na = TRUE) %>% dplyr::filter(value != '')
    post_json <-jsonlite::toJSON(list(properties = pre_json), pretty = TRUE, auto_unbox = TRUE)

    hubSpotOutput <- httr::PUT(url = paste0('https://api.hubapi.com/companies/v2/companies/', companyId, '?hapikey=', apiKey), body = post_json, httr::add_headers(.headers = c("Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 200) {updateList[[i]] <- dplyr::tibble(rowid = i, companyId = as.character(companyId), statusCode = statusCode, property = 'Relevant properties have been updated', value = 'Relevant values have been updated', mainMessage = 'Company has been updated', propertyMessage = 'Successful', propertyError = 'No Error')}
    else if(statusCode == 404) {updateList[[i]] <- dplyr::tibble(rowid = i, companyId = as.character(companyId), statusCode = statusCode, property = '', value = '', mainMessage = outputContent$subCategory,  propertyMessage = outputContent$message, propertyError = outputContent$status)}
    else {
      contactMessage <- dplyr::tibble(rowid = i, companyId = as.character(companyId), statusCode = statusCode, pre_json, mainMessage = outputContent$message) %>% dplyr::mutate(name1 = tolower(name))
      propertyMessage <- outputContent$validationResults %>% rlist::list.stack() %>% dplyr::rename(propertyMessage = message, propertyError = error, name = name) %>% dplyr::select(-isValid)
      updateList[[i]] <- dplyr::left_join(contactMessage, propertyMessage, by = c('name1'='name')) %>% dplyr::select(-name1) %>%  stats::na.omit() }
  }

  updateList <- dplyr::bind_rows(updateList)

  return(updateList)
}
