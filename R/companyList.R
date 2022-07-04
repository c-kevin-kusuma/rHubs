#' Retrieve 'HubSpot' Current Companies
#'
#' This function allows you to quickly retrieve company list from 'HubSpot' via 'API' connection.
#' @param apiKey An API key that can be generated on a HubSpot instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'companyProperties()' to check the internal names of the company properties. Defaults to 'c("name", "website")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to no limit.
#' @examples companyList <- companyList(apiKey = "demo", properties = c("name", "website", "is_public"), limit = 2)
#' @export

companyList <- function(apiKey = apiKey, properties = c("name", "website"), limit = "Default") {
  properties = paste(paste0('&properties=', properties), collapse = '')

  if(limit == "Default") {url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey, properties)}
  else {url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey, properties, '&limit=', limit)}

  allCompaniesRaw <- httr::GET(url = url) %>% httr::content()

  compList <- list()
  for (i in 1:length(allCompaniesRaw$companies)) {
    companyId <- allCompaniesRaw$companies[[i]]$companyId
    propertyName <- allCompaniesRaw$companies[[i]]$properties %>% names()
    propertyValue <- allCompaniesRaw$companies[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    compList[[i]] <- dplyr::tibble(companyId = companyId, propertyName = propertyName, propertyValue = propertyValue$value) }
  compList <- dplyr::bind_rows(compList)

  compList <- compList %>% tidyr::pivot_wider(id_cols = companyId, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)
  return(compList)
}
