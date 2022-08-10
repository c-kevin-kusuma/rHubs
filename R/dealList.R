#' Retrieve 'HubSpot' Current Deals
#'
#' This function allows you to quickly retrieve deal list from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'dealProperties()' to check the internal names of the company properties. Defaults to 'c("dealname", "dealstage", "amount", "closedate")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to "100".
#' @param offset 'HubSpot' paginates the output of deal list into batches of 100. If your 'HubSpot' instance have more than 100 deals, you will need to include the offset code to get the next batch of 100 deals. Offset value of the page will be included in the result of this function, you can then use that to get the next page and so on.
#' @examples dealList <- dealList(
#'   apiKey = "demo",
#'   properties = c("firstname", "lastname", "email", "company"),
#'   limit = 2)
#' @export

dealList <- function(apiKey, properties = c("dealname", "dealstage", "amount", "closedate"), limit = 100, offset = "First Page") {
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

  properties = paste(paste0('&properties=', properties), collapse = '')

  if(limit == 100) {url = paste0('https://api.hubapi.com/deals/v1/deal/paged?hapikey=', apiKey, '&includeAssociations=true', properties, '&limit=100')}
  else {url = paste0('https://api.hubapi.com/deals/v1/deal/paged?hapikey=', apiKey, '&includeAssociations=true', properties, '&limit=', limit)}

  if(offset == "First Page") {url = url} else {url = paste0(url, '&offset=', offset)}

  allDealsRaw <- httr::GET(url = url) %>% httr::content()
  if(length(allDealsRaw$deals)==0) {stop('No deals can be found')}
  has_more <- allDealsRaw$hasMore
  if(has_more == TRUE) {offsetValue <- as.character(allDealsRaw$offset)} else {offsetValue <- ''}

  deaList <- list()
  for (i in 1:length(allDealsRaw$deals)) {
    dealId <- allDealsRaw$deals[[i]]$dealId
    propertyName <- allDealsRaw$deals[[i]]$properties %>% names()
    # associationName <- allDealsRaw$deals[[i]]$associations %>% names()
    propertyValue <- allDealsRaw$deals[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    # if(nrow(rlist::list.stack(allDealsRaw$deals[[i]]$associations))==0) {associationValue <- dplyr::tibble(value = '')} else {associationValue <- allDealsRaw$deals[[i]]$associations %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)}
    deaList[[i]] <- dplyr::tibble(dealId = dealId, propertyName = propertyName, propertyValue = propertyValue$value)  }

  deaList <- dplyr::bind_rows(deaList)
  deaList <- deaList %>% tidyr::pivot_wider(id_cols = dealId, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)
  deaList <- dplyr::tibble(has_more = has_more, offset = offsetValue, deaList)

  return(deaList)
}
