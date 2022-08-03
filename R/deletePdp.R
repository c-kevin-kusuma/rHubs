#' Delete 'PDP' Policies
#'
#' This function allows you to quickly delete 'PDP' policies on a certain 'DOMO' dataset via 'API' connection.
#' @param clientID The ID that can be generated through the 'DOMO' developer page.
#' @param secret A secret that can be generated through the 'DOMO' developer page.
#' @param data The dataframe that includes the "Dataset ID" and the "Policy ID" of the policies.
#' @examples deletedPdp <- deletePdp(
#'   clientID = "CLIENT ID",
#'   secret = "SECRET",
#'   data = "DATAFRAME")
#' @export

deletePdp <- function(clientID, secret, data) {
  rhub <- rdomo::Domo(client_id = clientID, secret = secret)
  y <- list()
  for (i in 1:nrow(data)) {
    dsID <- data$`Dataset ID`[i]
    polID <- data$`Policy ID`[i]
    x <- rhub$pdp_delete(ds = dsID, policy = polID)
    if(length(x) == 0) {y[[i]] <- dplyr::tibble(`Dataset ID` = dsID, `Policy ID` = polID, `Delete Status` = 'Successful')} else {y[[i]] <- dplyr::tibble(`Dataset ID` = dsID, `Policy ID` = polID, `Delete Status` = as.character(x$statusReason))}
  }
  y <- dplyr::bind_rows(y)
  return(y)
}
