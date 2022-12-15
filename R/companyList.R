#' Retrieve 'HubSpot' Current Companies
#'
#' This function allows you to quickly retrieve company list from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param properties A vector of character values corresponds to 'HubSpot' internal names of the properties. Use 'companyProperties()' to check the internal names of the company properties. Defaults to 'c("name", "website")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to "10" and max to "100".
#' @param after 'HubSpot' paginates the output of company list into batches. If your 'HubSpot' instance have more than 100 companies, you will need to include the after code to get the next batch of 100 companies. After value of the page will be included in the result of this function, you can then use that to get the next page and so on.
#' @examples companyList <- companyList(
#'   accessToken = "accessToken",
#'   properties = c("name", "website", "is_public"),
#'   limit = 2)
#' @export

companyList <- function(accessToken, properties = c("name", "website"), limit = 10, after = "First Page") {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rjson", quietly = TRUE)) {stop("Package \"rjson\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(accessToken)) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if (is.double(limit) | is.integer(limit)) {} else {stop('The input for "limit" is invalid! Only accepts "integer" or "double".', call. = FALSE)}
  if (limit > 100) {stop('The input for "limit" cannot be greater than "100", use "companyListAll()".')}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=1&archived=false&properties=name'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}

  properties = paste(paste0('&properties=', properties), collapse = '')
  url = paste0('https://api.hubapi.com/crm/v3/objects/companies?&archived=false', properties, '&limit=', limit)
  if(after == "First Page") {url = url} else {url = paste0(url, '&after=', after)}

  allCompaniesRaw <- httr::GET(url = url, httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::content()

  if('paging' %in% names(allCompaniesRaw)) {has_more <- 'Yes'} else {has_more <- NA}
  if('paging' %in% names(allCompaniesRaw)) {after <- allCompaniesRaw$paging$`next`$after} else {after <- NA}

  compList <- list()
  for (i in 1:length(allCompaniesRaw$results)) {
    properties <- allCompaniesRaw$results[[i]]$properties %>% list() %>% rlist::list.stack()
    compList[[i]] <- properties }
  compList <- dplyr::bind_rows(compList)
  compList <- dplyr::tibble(has_more = has_more, after = after, compList)

  return(compList)
}
