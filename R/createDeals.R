#' Create or Add New Deals on 'HubSpot'
#'
#' This function allows you to create or add new deals from a dataframe to 'HubSpot' via 'API' connection.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param data A dataframe that contains the deal's name and the properties associated with the new deal. The field names in the data must match (case sensitive) the property's internal name on HubSpot', use 'dealProperties()'.
#' @examples output <- createDeals(
#'   accessToken = accessToken,
#'   data = data)
#' @export

createDeals <- function(accessToken, data) {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rjson", quietly = TRUE)) {stop("Package \"rjson\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("svMisc", quietly = TRUE)) {stop("Package \"svMisc\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(accessToken) == TRUE) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if ('dealname' %in% colnames(data)) {} else {stop('A required \"dealname\" field that represents the name of the deal is missing from the \"data\".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=10&archived=false', '&properties=name&limit=1'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}

  addList <- list()
  for (i in 1:nrow(data)) {
    svMisc::progress(i*100/nrow(data))
    dealname <- data$dealname[i]
    x <- data[i, ] %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    json <- rjson::toJSON(list(properties = x))
    hubSpotOutput <- httr::POST(url = 'https://api.hubapi.com/crm/v3/objects/deals', body = json, httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken), "Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 201) {addList[[i]] <- dplyr::tibble(rowid = i, id = as.character(outputContent$id), dealname = dealname, statusCode = statusCode, message = 'Successful')}
    else {addList[[i]] <- dplyr::tibble(rowid = i, id = 'Execution Failed', dealname = dealname, statusCode = statusCode, message = outputContent$message)}
    if (i == nrow(data)) cat("Done!\n")
  }

  addList <- dplyr::bind_rows(addList)

  return(addList)
}
