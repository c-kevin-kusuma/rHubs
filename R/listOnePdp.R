#' Create A Single 'PDP'
#'
#' This function allows you to quickly create a single 'PDP' from a dataframe.
#' @param data The dataframe to update the 'PDP' list on a dataset, should include "Policy Name", "Policy Column", "User ID", and "Policy Value". "Policy ID" is optional.
#' @examples listedOnePdp <- listOnePdp(data = "DATAFRAME")
#' @export

listOnePdp <- function(data) {
  `%!in%` <- Negate(`%in%`)
  x <- data %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  if('Policy ID' %in% colnames(x)){id <- as.integer(x$`Policy ID`)} else{id <- NULL}
  filters <- list()
  users <- list()
  longFilters <- x$`Policy Value` %>% strsplit('|', fixed = TRUE) %>% unlist()
  longUsers <- x$`User ID` %>% strsplit('|', fixed = TRUE) %>% unlist()
  for (i in 1:length(longFilters)) {filters[[i]] <- list(column = x$`Policy Column`, values = list(longFilters[i]), operator = 'EQUALS', not = FALSE) } # Create Filters
  for (i in 1:length(longUsers)) {users[[i]] <- as.integer(longUsers) } # Create Users
  pdpList <- list(id = id, type = 'user', name = x$`Policy Name`, filters = filters, users = users, virtualUsers = list(), groups = list())
  if('Policy ID' %!in% colnames(x)){pdpList$id <- NULL}
  return(pdpList)
}
