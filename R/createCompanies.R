#' Create or Add New Companies on 'HubSpot'
#'
#' This function allows you to create or add new companies from a dataframe to 'HubSpot' via 'API' connection.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param data A dataframe that contains the company's name and the properties associated with the new company. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'companyProperties()'.
#' @examples output <- createCompanies(
#'   apiKey = apiKey,
#'   data = data)
#' @export

createCompanies <- function(apiKey, data) {
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
  if ('name' %in% colnames(data)) {} else {stop('A required \"name\" field that represents the name of the company is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey , '&properties=name&limit=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.', call. = FALSE)}

  addList <- list()
  for (i in 1:nrow(data)) {
    companyName <- data$name[i]
    x <- data[i, ] %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    pre_json <- tidyr::pivot_longer(x, cols = colnames(x), names_to = 'name', values_to = 'value') %>% dplyr::filter(value != '')
    post_json <- jsonlite::toJSON(list(properties = pre_json), pretty = TRUE, auto_unbox = TRUE)

    hubSpotOutput <- httr::POST(url = paste0('https://api.hubapi.com/companies/v2/companies?hapikey=', apiKey), body = post_json, httr::add_headers(.headers = c("Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 200) {addList[[i]] <- dplyr::tibble(rowid = i, companyName = companyName, companyId = as.character(outputContent$companyId), statusCode = statusCode, property = 'All properties have been created', value = 'All Values have been created', mainMessage = 'Company has been created', propertyMessage = 'Successful', propertyError = 'No Error')}
    else if(statusCode == 409) {addList[[i]] <- dplyr::tibble(rowid = i, companyName = companyName, companyId = 'No execution', statusCode = statusCode, property = 'Company already exists', value = 'Company already exists', mainMessage = outputContent$message, propertyMessage = 'No execution', propertyError = 'No execution')}
    else if(statusCode == 400) {
      companyMessage <- dplyr::tibble(rowid = i, companyName = companyName, companyId = 'Not Created', statusCode = statusCode, pre_json, mainMessage = outputContent$message) %>% dplyr::mutate(name1 = tolower(name))
      propertyMessage <- outputContent$validationResults %>% rlist::list.stack() %>% dplyr::rename(propertyMessage = message, propertyError = error, name = name) %>% dplyr::select(-isValid)
      addList[[i]] <- dplyr::left_join(companyMessage, propertyMessage, by = c('name1'='name')) %>% dplyr::select(-name1) %>%  stats::na.omit() }
  }

  addList <- dplyr::bind_rows(addList)

  return(addList)
}
