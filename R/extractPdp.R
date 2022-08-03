#' Extract 'PDP' List
#'
#' This function allows you to quickly retrieve 'PDP' list from a certain 'DOMO' dataset via 'API' connection and output them as a dataframe.
#' @param clientID The ID that can be generated through the 'DOMO' developer page.
#' @param secret A secret that can be generated through the 'DOMO' developer page.
#' @param dsID The ID of a DOMO dataset that can be found on the URL of the dataset.
#' @examples extractedPdp <- extractPdp(
#'   clientID = "CLIENT ID",
#'   secret = "SECRET",
#'   dsID = "DATASET ID")
#' @export

extractPdp <- function(clientID, secret, dsID) {
  rhub <- rdomo::Domo(client_id = clientID, secret = secret)
  x <- rhub$pdp_list(ds = dsID)

  if(length(x) == 0) {break}
  for (i in 1:length(x)) {
    if(length(x[[i]]$users) == 0){users <- dplyr::tibble(users = '')} else{users <- dplyr::tibble(users = x[[i]]$users) %>% dplyr::mutate(users = as.character(users)) %>% dplyr::arrange(users) %>% dplyr::group_by() %>% dplyr::summarise(users = paste(users, collapse = '|'))} # Extract Users
    if(length(x[[i]]$filters) == 0){filters <- dplyr::tibble(column = '', values = '')} else{filters <- x[[i]]$filters %>% rlist::list.stack() %>% dplyr::select(column, values) %>% dplyr::mutate(values = as.character(values)) %>% dplyr::arrange(values) %>% dplyr::group_by(column) %>% dplyr::summarise(values = paste(values, collapse = '|'))}
    x[[i]] <- dplyr::tibble(`Policy ID` = x[[i]]$id, `Policy Name` = x[[i]]$name, `Policy Column` = filters$column, `User ID` = users$users, `Policy Value` = filters$values)}

  y <- dplyr::bind_rows(x)
  return(y)
}
