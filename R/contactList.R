#' Retrieve 'HubSpot' Current Contacts
#'
#' This function allows you to quickly retrieve contact list from 'HubSpot' via 'API' connection.
#' @param apiKey An API key that can be generated on a HubSpot instance.
#' @param properties A vector of character values corresponds to 'HubSpot' interval names of the properties. Use 'contactProperties()' to check the internal names of the company properties. Defaults to 'c("firstname", "lastname", "email", "company")'.
#' @param limit An integer to limit the number of companies to be retrieved. Defaults to no limit.
#' @examples contactList <- contactList(apiKey = "demo", properties = c("firstname", "lastname", "email", "company"), limit = 2)
#' @export

contactList <- function(apiKey = apiKey, properties = c("firstname", "lastname", "email", "company"), limit = "Default") {
  properties = paste(paste0('&property=', properties), collapse = '')

  if(limit == "Default") {url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey, properties)}
  else {url = paste0('https://api.hubapi.com/contacts/v1/lists/all/contacts/all?hapikey=', apiKey, properties, '&count=', limit)}

  allContactsRaw <- httr::GET(url = url) %>% httr::content()

  contList <- list()
  for (i in 1:length(allContactsRaw$contacts)) {
    vid <- allContactsRaw$contacts[[i]]$vid
    propertyName <- allContactsRaw$contacts[[i]]$properties %>% names()
    propertyValue <- allContactsRaw$contacts[[i]]$properties %>% rlist::list.stack() %>% suppressWarnings() %>% dplyr::select(value)
    contList[[i]] <- dplyr::tibble(vid = vid, propertyName = propertyName, propertyValue = propertyValue$value) }
  contList <- dplyr::bind_rows(contList)

  contList <- contList %>% tidyr::pivot_wider(id_cols = vid, names_from = propertyName, values_from = propertyValue, names_sort = TRUE)
  return(contList)
}
