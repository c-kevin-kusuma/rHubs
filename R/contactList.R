#' Retrieve 'HubSpot' Current Contacts
#'
#' This function allows you to quickly retrieve contact list from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'contactProperties()' to check the internal names of the company properties. Defaults to 'c("firstname", "lastname", "email", "company")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to "100".
#' @param offset 'HubSpot' paginates the output of contact list into batches of 100. If your 'HubSpot' instance have more than 100 companies, you will need to include the offset code to get the next batch of 100 companies. Offset value of the page will be included in the result of this function, you can then use that to get the next page and so on.
#' @examples contactList <- contactList(
#'   apiKey = "demo",
#'   properties = c("firstname", "lastname", "email", "company"),
#'   limit = 2)
#' @export

contactList <- function(apiKey, properties = c("firstname", "lastname", "email", "company"), limit = 100, offset = "First Page") {
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

  if(limit == 100) {url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey, '&count=100', properties)}
  else {url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey, properties, '&count=', limit)}

  if(offset == "First Page") {url = url} else {url = paste0(url, '&vidOffset=', offset)}

  allContactsRaw <- httr::GET(url = url) %>% httr::content()
  has_more <- allContactsRaw$`has-more`
  if(has_more == TRUE) {offsetValue <- as.character(allContactsRaw$`vid-offset`)} else {offsetValue <- ''}

  contList <- list()
  for (i in 1:length(allContactsRaw$contacts)) {
    vid <- allContactsRaw$contacts[[i]]$vid
    propertyName <- allContactsRaw$contacts[[i]]$properties %>% names()
    propertyValue <- allContactsRaw$contacts[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    contList[[i]] <- dplyr::tibble(vid = vid, propertyName = propertyName, propertyValue = propertyValue$value) }

  contList <- bind_rows(contList)
  contList <- contList %>% tidyr::pivot_wider(id_cols = vid, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)
  contList <- dplyr::tibble(has_more = has_more, offset = offsetValue, contList)


  return(contList)
}
