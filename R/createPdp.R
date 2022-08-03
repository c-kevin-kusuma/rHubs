#' Create 'PDP' Policies
#'
#' This function allows you to quickly create new 'PDP' policies on a certain 'DOMO' dataset via 'API' connection.
#' @param clientID The ID that can be generated through the 'DOMO' developer page.
#' @param secret A secret that can be generated through the 'DOMO' developer page.
#' @param data The dataframe to update the 'PDP' list on a dataset, should include "Dataset ID", "Policy Name", "Policy Column", "User ID", and "Policy Value".
#' @examples createdPdp <- createPdp(
#'   clientID = "CLIENT ID",
#'   secret = "SECRET",
#'   data = "DATAFRAME")
#' @export

createPdp <- function(clientID, secret, data) {
  f <- function(x) {
    if(length(x)==0) {break}
    for (i in 1:length(x)) {
      if(length(x[[i]]$users) == 0){users <- dplyr::tibble(users = '')} else{users <- dplyr::tibble(users = x[[i]]$users) %>% dplyr::mutate(users = as.character(users)) %>% dplyr::arrange(users) %>% dplyr::group_by() %>% dplyr::summarise(users = paste(users, collapse = '|'))} # Extract Users
      if(length(x[[i]]$filters) == 0){filters <- dplyr::tibble(column = '', values = '')} else{filters <- x[[i]]$filters %>% rlist::list.stack() %>% dplyr::select(column, values) %>% dplyr::mutate(values = as.character(values)) %>% dplyr::arrange(values) %>% dplyr::group_by(column) %>% dplyr::summarise(values = paste(values, collapse = '|'))}
      x[[i]] <- dplyr::tibble(`Policy ID` = x[[i]]$id, `Policy Name` = x[[i]]$name, `Policy Column` = filters$column, `User ID` = users$users, `Policy Value` = filters$values)}

    y <- dplyr::bind_rows(x)
  }

  rhub <- rdomo::Domo(client_id = clientID, secret = secret)
  y <- list()
  for (i in 1:nrow(data)) {
    dsID <- data$`Dataset ID`[i]
    policy <- rHubs::listOnePdp(data = data[i, ])
    y[[i]] <- rhub$pdp_create(ds = dsID, policy_def = policy)
  }

  z <- f(y)
  return(z)
}
