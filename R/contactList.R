#' Retrieve 'HubSpot' Current Contacts
#'
#' This function allows you to quickly retrieve contact list from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'contactProperties()' to check the internal names of the company properties. Defaults to 'c("firstname", "lastname", "email", "company")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to "No Limit".
#' @examples contactList <- contactList(
#'   apiKey = "demo",
#'   properties = c("firstname", "lastname", "email", "company"),
#'   limit = 2)
#' @export

contactList <- function(apiKey, properties = c("firstname", "lastname", "email", "company"), limit = "No Limit") {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("tidyr", quietly = TRUE)) {stop("Package \"tidyr\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(apiKey) == TRUE) {stop('The required "apiKey" is missing!', call. = FALSE)}
  if (limit == 'No Limit' | is.double(limit) == TRUE | is.integer(limit) == TRUE) {} else {stop('The input for "limit" is invalid! Only accepts "integer" or "double".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey , '&properties=name&limit=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.')}

  properties = paste(paste0('&property=', properties), collapse = '')

  if(limit == "No Limit") {url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey, properties)}
  else {url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey, properties, '&count=', limit)}

  allContactsRaw <- httr::GET(url = url) %>% httr::content()

  contList <- list()
  for (i in 1:length(allContactsRaw$contacts)) {
    vid <- allContactsRaw$contacts[[i]]$vid
    propertyName <- allContactsRaw$contacts[[i]]$properties %>% names()
    propertyValue <- allContactsRaw$contacts[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    contList[[i]] <- dplyr::tibble(vid = vid, propertyName = propertyName, propertyValue = propertyValue$value) }

  contList <- bind_rows(contList)
  contList <- contList %>% tidyr::pivot_wider(id_cols = vid, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)

  return(contList)
}
