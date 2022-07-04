#' Retrieve 'HubSpot' Contact Properties
#'
#' This function allows you to quickly retrieve contact properties from 'HubSpot' via 'API' connection.
#' @param apiKey An API key that can be generated on a HubSpot instance.
#' @param maxOptions The maximum number of options of a given property to be included in the output dataframe. Defaults to 50.
#' @examples contactProperties <- contactProperties(apiKey = "demo", maxOptions = 100)
#' @export

contactProperties <- function(apiKey = apiKey, maxOptions = 50) {
  url = paste0('https://api.hubapi.com/properties/v1/contacts/properties?hapikey=', apiKey)
  allContPropRaw <- httr::GET(url = url) %>% httr::content() %>% rlist::list.filter(readOnlyValue == FALSE)
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
