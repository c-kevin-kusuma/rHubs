#' Retrieve 'HubSpot' Deal Properties
#'
#' This function allows you to quickly retrieve Deal properties from 'HubSpot' via 'API' connection.
#' @param apiKey An API key that can be generated on a HubSpot instance.
#' @param maxOptions The maximum number of options of a given property to be included in the output dataframe. Defaults to 50.
#' @examples dealProperties <- dealProperties(apiKey = "demo", maxOptions = 100)
#' @export

dealProperties <- function(apiKey = apiKey, maxOptions = 50) {
  url = paste0('https://api.hubapi.com/properties/v1/deals/properties?hapikey=', apiKey)
  allDealPropRaw <- httr::GET(url = url) %>% httr::content() %>% rlist::list.filter(readOnlyValue == FALSE)
  allDealPropRawStack <- allDealPropRaw %>% rlist::list.stack() %>% suppressWarnings()
  allDealPropTidy <- allDealPropRawStack %>% dplyr::select(-options) %>% dplyr::unique()

  if(maxOptions == 'All') {n_options <- allCompPropRawStack %>% dplyr::select(name) %>% unique()}
  else {n_options <- allDealPropRawStack %>% dplyr::group_by(name) %>% dplyr::summarise(n_options = n()) %>% dplyr::filter(n_options <= maxOptions)}

  allDealPropRaw <- allDealPropRaw %>% rlist::list.filter(name %in% n_options$name)

  #-------------------------------------------------------------------------------------------------------------------------------
  allDealOptions <- list()
  for (i in 1:length(allDealPropRaw)) {
    if(length(allDealPropRaw[[i]]$options) > 0) {
      name <- allDealPropRaw[[i]]$name

      optionList <- list()
      for (j in 1:length(allDealPropRaw[[i]]$options)) {
        optionLabel <- allDealPropRaw[[i]]$options[[j]]$label
        optionValue <- allDealPropRaw[[i]]$options[[j]]$value
        optionList[[j]] <- dplyr::tibble(optionLabel = optionLabel, optionValue = optionValue) }
      optionList <- dplyr::bind_rows(optionList)

      allDealOptions[[i]] <- dplyr::tibble(name = name, optionLabel = optionList$optionLabel, optionValue = optionList$optionValue) }
    else {allDealOptions[[i]] <- dplyr::tibble(name = allDealPropRaw[[i]]$name, optionLabel = '', optionValue = '') } }
  allDealOptions <- dplyr::bind_rows(allDealOptions)
  #-------------------------------------------------------------------------------------------------------------------------------

  allDealProp <- dplyr::left_join(allDealPropTidy, allDealOptions, by = c('name' = 'name'))

  return(allDealProp)
}
