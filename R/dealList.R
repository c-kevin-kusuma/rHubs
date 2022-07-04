#' Retrieve 'HubSpot' Current Deals
#'
#' This function allows you to quickly retrieve deal list from 'HubSpot' via 'API' connection.
#' @param apiKey An API key that can be generated on a HubSpot instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'dealProperties()' to check the internal names of the company properties. Defaults to 'c("dealname", "dealstage", "amount", "closedate")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to no limit.
#' @examples dealList <- dealList(apiKey = "demo", properties = c("firstname", "lastname", "email", "company"), limit = 2)
#' @export

dealList <- function(apiKey = apiKey, properties = c("dealname", "dealstage", "amount", "closedate"), limit = "Default") {
  properties = paste(paste0('&properties=', properties), collapse = '')

  if(limit == "Default") {url = paste0('https://api.hubapi.com/deals/v1/deal/paged?hapikey=', apiKey, '&includeAssociations=true', properties)} else {url = paste0('https://api.hubapi.com/deals/v1/deal/paged?hapikey=', apiKey, '&includeAssociations=true', properties, '&limit=', limit)}

  allDealsRaw <- httr::GET(url = url) %>% httr::content()
  if(length(allDealsRaw$deals)==0) {stop('No deals can be found')}

  deaList <- list()
  for (i in 1:length(allDealsRaw$deals)) {
    dealId <- allDealsRaw$deals[[i]]$dealId
    propertyName <- allDealsRaw$deals[[i]]$properties %>% names()
    associationName <- allDealsRaw$deals[[i]]$associations %>% names()
    propertyValue <- allDealsRaw$deals[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    if(nrow(rlist::list.stack(allDealsRaw$deals[[i]]$associations))==0) {associationValue <- dplyr::tibble(value = '')} else {associationValue <- allDealsRaw$deals[[i]]$associations %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)}
    deaList[[i]] <- dplyr::bind_rows(dplyr::tibble(dealId = dealId, propertyName = propertyName, propertyValue = propertyValue$value),
                                     dplyr::tibble(dealId = dealId, propertyName = associationName, propertyValue = associationValue$value)) }
  deaList <- dplyr::bind_rows(deaList)

  deaList <- deaList %>% tidyr::pivot_wider(id_cols = dealId, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)
  return(deaList)
}
