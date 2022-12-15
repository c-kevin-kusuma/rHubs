#' Retrieve 'HubSpot' Current Deals
#'
#' This function allows you to quickly retrieve deal list from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'dealProperties()' to check the internal names of the company properties. Defaults to 'c("dealname", "dealstage", "amount", "closedate")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to "100".
#' @param after 'HubSpot' paginates the output of deal list into batches of 100. If your 'HubSpot' instance have more than 100 deals, you will need to include the offset code to get the next batch of 100 deals. Offset value of the page will be included in the result of this function, you can then use that to get the next page and so on.
#' @examples dealList <- dealList(
#'   accessToken = accessToken,
#'   properties = c("dealname", "dealstage", "amount", "closedate"),
#'   limit = 2)
#' @export

dealList <- function(accessToken, properties = c("dealname", "dealstage", "amount", "closedate"), limit = 100, after = "First Page") {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(accessToken) == TRUE) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if (limit == 'No Limit' | is.double(limit) == TRUE | is.integer(limit) == TRUE) {} else {stop('The input for "limit" is invalid! Only accepts "integer" or "double".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=1&archived=false&properties=name'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}

  properties = paste(paste0('&properties=', properties), collapse = '')

  url = paste0('https://api.hubapi.com/crm/v3/objects/deals?&archived=false', properties, '&limit=', limit)
  if(after == "First Page") {url = url} else {url = paste0(url, '&after=', after)}

  allDealsRaw <- httr::GET(url = url, httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::content()

  if('paging' %in% names(allDealsRaw)) {has_more <- 'Yes'} else {has_more <- NA}
  if('paging' %in% names(allDealsRaw)) {after <- allDealsRaw$paging$`next`$after} else {after <- NA}

  deaList <- list()
  for (i in 1:length(allDealsRaw$results)) {
    properties <- allDealsRaw$results[[i]]$properties %>% list() %>% rlist::list.stack() %>% suppressWarnings()
    deaList[[i]] <- properties }
  deaList <- dplyr::bind_rows(deaList)
  deaList <- dplyr::tibble(has_more = has_more, after = after, deaList)

  return(deaList)
}
