#' Associate 'CRM' Object
#'
#' This function allows you to quickly create associations between objects on 'HubSpot' such as company, contact, and deal.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param action Valid values are "Add" & "Delete".
#' @param from The internal ID of the object to be associated from.
#' @param to The internal ID of the object to be associated to.
#' @param definitionId The ID that codifies what assciation to be made. See \href{https://legacydocs.hubspot.com/docs/methods/crm-associations/crm-associations-overview}{HubSpot Documentation}
#' @examples result <- asCrm(
#'   apiKey = apiKey,
#'   action = "Add",
#'   from = vid,
#'   to = companyId,
#'   definitionId = 1)
#' @export

asCrm <- function(apiKey, action, from, to, definitionId) {
  def = dplyr::case_when(definitionId == 1 ~ 'Contact to Company', definitionId == 2 ~ 'Company to Contact', definitionId == 3 ~ 'Deal to Contact', definitionId == 4 ~ 'Contact to Deal', definitionId == 5 ~ 'Deal to Company', definitionId == 6 ~ 'Company to Deal', TRUE ~ 'Other')
  if(action == 'Add') {url = paste0('https://api.hubapi.com/crm-associations/v1/associations?hapikey=', apiKey)} else if(action == 'Delete') {url = paste0('https://api.hubapi.com/crm-associations/v1/associations/delete?hapikey=', apiKey)} else {break}

  table = dplyr::tibble(fromObjectId = from, toObjectId = to, category = "HUBSPOT_DEFINED", definitionId = definitionId)
  body = jsonlite::toJSON(table, pretty = TRUE, auto_unbox = TRUE)
  x = paste('{','"fromObjectId":', from, ',', '"toObjectId":', to, ',', '"category": "HUBSPOT_DEFINED",', '"definitionId":', definitionId , '}')

  result <- httr::PUT(url = url, body = x, httr::add_headers(.headers = c("Content-Type"="application/json")))
  statusCode <- httr::status_code(result)
  if(statusCode == 204) {out <- dplyr::tibble(action = action, statusCode = statusCode, status = 'Successful', message = 'Successful', from = from, to = to, association = def)} else {out <- dplyr::tibble(action = action, statusCode = statusCode, status = 'Failed', message = httr::content(result)$message, from = from, to = to, association = def)}

  return(out)
}
