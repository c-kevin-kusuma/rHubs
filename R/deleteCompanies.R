#' Delete Companies on 'HubSpot'
#'
#' This is a DESTRUCTIVE method which is not a best practice because companies play a central role in the CRM. Typical use case for this method is when the companies are created by your application. This function rely on 'API' connection.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param companyId Values that represent the internal IDs of the companies on your 'HubSpot' instance.
#' @examples output <- deleteCompanies(
#'   accessToken = accessToken,
#'   companyId = companyId)
#' @export

deleteCompanies <- function(accessToken, companyId) {

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(accessToken) == TRUE) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if (is.na(companyId) == TRUE) {} else {stop('The required "companyId" is missing".', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=10&archived=false', '&properties=name&limit=1'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}


  deleteList <- list()
  for (i in 1:length(companyId)) {
    hubSpotOutput <- httr::DELETE(url = paste0('https://api.hubapi.com/crm/v3/objects/companies/', companyId[i]), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken), "Content-Type"="application/json")))

    outputContent <- hubSpotOutput %>% httr::content()
    statusCode <- hubSpotOutput %>% httr::status_code()

    if(statusCode == 204) {deleteList[[i]] <- dplyr::tibble(rowid = i, companyId = companyId[i], statusCode = statusCode, message = 'The company has been deleted')} else {deleteList[[i]] <- dplyr::tibble(rowid = i, companyId = companyId[i], statusCode = statusCode, message = outputContent$message)}
  }

  deleteList <- dplyr::bind_rows(deleteList)

  return(deleteList)
}
