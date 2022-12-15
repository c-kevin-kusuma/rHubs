#' Retrieve 'HubSpot' Properties of A Certain Object
#'
#' This function allows you to quickly retrieve company properties from 'HubSpot' via 'API' connection and output them as a dataframe.
#' @param accessToken An 'Access Token' can be generated from a 'HubSpot' instance through a private app.
#' @param object_type A string input to specify what object the properties need to be pulled from such as "companies", "contacts", or "deals".
#' @param archived A Boolean or Logical input to specify whether pulling the archived properties or not. Defaults to 'FALSE' which excludes archived properties. 'TRUE' will include archived properties.
#' @examples propertyList <- propertyList(
#'   accessToken = "accessToken",
#'   object_type = "companies",
#'   archived = FALSE)
#' @export

propertyList <- function(accessToken, object_type, archived = FALSE) {
  options(scipen=999)

  # Check Required Packages
  if (!requireNamespace("magrittr", quietly = TRUE)) {stop("Package \"magrittr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("httr", quietly = TRUE)) {stop("Package \"httr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("rlist", quietly = TRUE)) {stop("Package \"rlist\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" must be installed to use this function.", call. = FALSE)}
  if (!requireNamespace("tidyr", quietly = TRUE)) {stop("Package \"tidyr\" must be installed to use this function.", call. = FALSE)}

  # Check Required Inputs
  if (is.na(accessToken)) {stop('The required "accessToken" is missing!', call. = FALSE)}
  if(object_type %in% c('companies', 'contacts', 'deals')) {} else {stop('Inputs for "object_type" must be "companies", "contacts", or "deals".')}

  # Check Connection
  checkConnection <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/objects/companies?limit=10&archived=false', '&properties=name&limit=1'), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::status_code()
  if(checkConnection != 200) {stop('\"accessToken\" fails to establish connection with HubSpot, please check your \"accessToken\"', call. = FALSE)}

  if(archived == TRUE) {archived <- 'true'} else if (archived == FALSE) {archived <- 'false'}
  allCompPropRaw <- httr::GET(url = paste0('https://api.hubapi.com/crm/v3/properties/', object_type,'?archived=', archived), httr::add_headers(.headers = c("authorization"=paste("Bearer", accessToken)))) %>% httr::content()

  #-------------------------------------------------------------------------------------------------------------------------------
  compPropList <- list()
  for (i in 1:length(allCompPropRaw$results)) {
    a_list <- allCompPropRaw$results[[i]]
    description <- tibble(column = names(a_list), value = a_list, rowid = i) %>%
      tidyr::pivot_wider(id_cols = rowid, names_from = 'column', values_from = value)

    if('updatedAt' %in% names(description)) {updatedAt <- description$updatedAt} else {updatedAt <- NA}
    if('createdAt' %in% names(description)) {createdAt <- description$createdAt} else {createdAt <- NA}
    if('name' %in% names(description)) {name <- description$name} else {name <- NA}
    if('label' %in% names(description)) {label <- description$label} else {label <- NA}
    if('type' %in% names(description)) {type <- description$type} else {type <- NA}
    if('fieldType' %in% names(description)) {fieldType <- description$fieldType} else {fieldType <- NA}
    if('description' %in% names(description)) {descriptions <- description$description} else {descriptions <- NA}
    if('groupName' %in% names(description)) {groupName <- description$groupName} else {groupName <- NA}
    if('displayOrder' %in% names(description)) {displayOrder <- description$displayOrder} else {displayOrder <- NA}
    if('calculated' %in% names(description)) {calculated <- description$calculated} else {calculated <- NA}
    if('hasUniqueValue' %in% names(description)) {hasUniqueValue <- description$hasUniqueValue} else {hasUniqueValue <- NA}
    if('hidden' %in% names(description)) {hidden <- description$hidden} else {hidden <- NA}

    if('archivable' %in% names(a_list$modificationMetadata)) {archivable <- a_list$modificationMetadata$archivable} else {archivable <- NA}
    if('readOnlyDefinition' %in% names(a_list$modificationMetadata)) {readOnlyDefinition <- a_list$modificationMetadata$readOnlyDefinition} else {readOnlyDefinition <- NA}
    if('readOnlyValue' %in% names(a_list$modificationMetadata)) {readOnlyValue <- a_list$modificationMetadata$readOnlyValue} else {readOnlyValue <- NA}

    description <- tibble(rowid = i, updatedAt=updatedAt, createdAt=createdAt, name=name, label=label, type=type, fieldType=fieldType, description=descriptions, groupName=groupName, displayOrder=displayOrder, calculated=calculated, hasUniqueValue=hasUniqueValue, hidden=hidden, archivable = archivable, readOnlyDefinition = readOnlyDefinition, readOnlyValue = readOnlyValue)

    options <- allCompPropRaw$results[[i]]$options
    if(length(options) == 0) {optionList <- tibble(option_label = NA, option_value = NA, option_order = NA, option_hidden = NA)} else{
      optionList <- list()
      for (j in 1:length(options)) {optionList[[j]] <- tibble(option_label = options[[j]]$label, option_value = options[[j]]$value, option_order = options[[j]]$displayOrder, option_hidden = options[[j]]$hidden) }
      optionList <- dplyr::bind_rows(optionList)
    }

    compPropList[[i]] <- merge(description, optionList)
  }
  #-------------------------------------------------------------------------------------------------------------------------------

  compPropList <- dplyr::bind_rows(compPropList)

  return(compPropList)
}
