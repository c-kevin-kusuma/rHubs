#' Retrieve 'HubSpot' Contact Properties
#'
#' This function allows you to quickly retrieve contact properties from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param apiKey An 'API' key that can be generated from a 'HubSpot' instance.
#' @param maxOptions The maximum number of options of each property to be included in the output dataframe. Defaults to 50.
#' @param readOnlyValue A Boolean or Logical input to specify whether to include the readOnlyValue properties or not. Defaults to 'FALSE' which excludes read-only properties. 'TRUE' will include read-only properties.
#' @examples contactProperties <- contactProperties(
#'   apiKey = "demo",
#'   maxOptions = 100,
#'   readOnlyValue = TRUE)
#' @export

contactProperties <- function(apiKey, maxOptions = 50, readOnlyValue = FALSE) {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(apiKey) == TRUE) {stop('The required "apiKey" is missing!', call. = FALSE)}
  if (is.double(maxOptions) == TRUE | is.integer(maxOptions) == TRUE) {} else {stop('The input for "maxOptions" is invalid! Only accepts "integer" or "double".', call. = FALSE)}
  if (is.logical(readOnlyValue) == FALSE) {stop('The input for "readOnlyValue" parameter is invalid! Only accepts logical value.', call. = FALSE)}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/companies/v2/companies/paged?hapikey=', apiKey , '&properties=name&limit=1')) %>% httr::status_code()
  if(checkConnection != 200) {stop('apiKey fails to establish connection with HubSpot, please check your apiKey.')}

  url = paste0('https://api.hubapi.com/properties/v1/contacts/properties?hapikey=', apiKey)
  if(readOnlyValue == FALSE) {allContPropRaw <- httr::GET(url = url) %>% httr::content() %>% rlist::list.filter(readOnlyValue == FALSE)} else {allContPropRaw <- httr::GET(url = url) %>% httr::content()}
  allContPropRawStack <- allContPropRaw %>% rlist::list.stack() %>% suppressWarnings()
  allContPropTidy <- allContPropRawStack %>% dplyr::select(-options) %>% unique()

  if(maxOptions == 'All') {n_options <- allContPropRawStack %>% dplyr::select(name) %>% unique()}
  else {n_options <- allContPropRawStack %>% dplyr::group_by(name) %>% dplyr::summarise(n_options = n()) %>% dplyr::filter(n_options <= maxOptions)}

  allContPropRaw <- allContPropRaw %>% rlist::list.filter(name %in% n_options$name)

  #-------------------------------------------------------------------------------------------------------------------------------
  allContOptions <- list()
  for (i in 1:length(allContPropRaw)) {
    if(length(allContPropRaw[[i]]$options) > 0) {
      name <- allContPropRaw[[i]]$name

      optionList <- list()
      for (j in 1:length(allContPropRaw[[i]]$options)) {
        optionLabel <- allContPropRaw[[i]]$options[[j]]$label
        optionValue <- allContPropRaw[[i]]$options[[j]]$value
        optionList[[j]] <- dplyr::tibble(optionLabel = optionLabel, optionValue = optionValue) }
      optionList <- dplyr::bind_rows(optionList)

      allContOptions[[i]] <- dplyr::tibble(name = name, optionLabel = optionList$optionLabel, optionValue = optionList$optionValue) }
    else {allContOptions[[i]] <- dplyr::tibble(name = allContPropRaw[[i]]$name, optionLabel = '', optionValue = '') } }
  allContOptions <- dplyr::bind_rows(allContOptions)
  #-------------------------------------------------------------------------------------------------------------------------------

  allContProp <- dplyr::left_join(allContPropTidy, allContOptions, by = c('name' = 'name'))

  return(allContProp)
}
