#' Retrieve 'HubSpot' Company Properties
#'
#' This function allows you to quickly retrieve company properties from 'HubSpot' via 'API' connection.
#' @param apiKey An API key that can be generated on a HubSpot instance.
#' @param maxOptions The maximum number of options of a given property to be included in the output dataframe. Defaults to 50.
#' @examples companyProperties <- companyProperties(apiKey = "demo", maxOptions = 100)
#' @export

companyProperties <- function(apiKey = apiKey, maxOptions = 50) {
  url = paste0('https://api.hubapi.com/properties/v1/companies/properties?hapikey=', apiKey)
  allCompPropRaw <- httr::GET(url = url) %>% httr::content() %>% rlist::list.filter(readOnlyValue == FALSE)
  allCompPropRawStack <- allCompPropRaw %>% rlist::list.stack() %>% suppressWarnings()
  allCompPropTidy <- allCompPropRawStack %>% dplyr::select(-options) %>% unique()

  if(maxOptions == 'All') {n_options <- allCompPropRawStack %>% dplyr::select(name) %>% unique()}
  else {n_options <- allCompPropRawStack %>% dplyr::group_by(name) %>% dplyr::summarise(n_options = n()) %>% dplyr::filter(n_options <= maxOptions)}

  allCompPropRaw <- allCompPropRaw %>% rlist::list.filter(name %in% n_options$name)

  #-------------------------------------------------------------------------------------------------------------------------------
  allCompOptions <- list()
  for (i in 1:length(allCompPropRaw)) {
    if(length(allCompPropRaw[[i]]$options) > 0) {
      name <- allCompPropRaw[[i]]$name

      optionList <- list()
      for (j in 1:length(allCompPropRaw[[i]]$options)) {
        optionLabel <- allCompPropRaw[[i]]$options[[j]]$label
        optionValue <- allCompPropRaw[[i]]$options[[j]]$value
        optionList[[j]] <- dplyr::tibble(optionLabel = optionLabel, optionValue = optionValue) }
      optionList <- dplyr::bind_rows(optionList)

      allCompOptions[[i]] <- dplyr::tibble(name = name, optionLabel = optionList$optionLabel, optionValue = optionList$optionValue) }
    else {allCompOptions[[i]] <- dplyr::tibble(name = allCompPropRaw[[i]]$name, optionLabel = '', optionValue = '') } }
  allCompOptions <- dplyr::bind_rows(allCompOptions)
  #-------------------------------------------------------------------------------------------------------------------------------

  allCompProp <- dplyr::left_join(allCompPropTidy, allCompOptions, by = c('name' = 'name'))

  return(allCompProp)
}
