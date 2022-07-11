#' Retrieve 'HubSpot' Current Companies
#'
#' This function allows you to quickly retrieve company list from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param properties A vector of character values corresponds to 'HubSpot' internal names of the properties. Use 'companyProperties()' to check the internal names of the company properties. Defaults to 'c("name", "website")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to "No Limit".
#' @param offset 'HubSpot' paginates the output of company list into batches of 100. If your 'HubSpot' instance have more than 100 companies, you will need to include the offset code to get the next batch of 100 companies. Offset value of the page will be included in the result of this function, you can then use that to get the next page and so on.
#' @examples companyList <- companyList(
#'   apiKey = "demo",
#'   properties = c("name", "website", "is_public"),
#'   limit = 2)
#' @export

companyList <- function(apiKey, properties = c("name", "website"), limit = "No Limit", offset = "First Page") {
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

  if(limit == "No Limit") {url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey, properties)}
  else {url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey, properties, '&limit=', limit)}

  if(offset == "First Page") {url = url} else {url = paste0(url, '&offset=', offset)}

  allCompaniesRaw <- httr::GET(url = url) %>% httr::content()
  has_more <- allCompaniesRaw$`has-more`
  if(has_more == TRUE) {offsetValue <- as.character(allCompaniesRaw$offset)} else {offsetValue <- ''}

  compList <- list()
  for (i in 1:length(allCompaniesRaw$companies)) {
    companyId <- allCompaniesRaw$companies[[i]]$companyId
    propertyName <- allCompaniesRaw$companies[[i]]$properties %>% names()
    propertyValue <- allCompaniesRaw$companies[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    compList[[i]] <- dplyr::tibble(companyId = companyId, propertyName = propertyName, propertyValue = propertyValue$value) }

  compList <- dplyr::bind_rows(compList)
  compList <- compList %>% tidyr::pivot_wider(id_cols = companyId, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)
  compList <- dplyr::tibble(has_more = has_more, offset = offsetValue, compList)

  return(compList)
}
