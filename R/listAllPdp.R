#' Create 'PDP' List
#'
#' This function allows you to quickly create a list of 'PDP' from a dataframe.
#' @param data The dataframe to update the 'PDP' list on a dataset, should include "Policy Name", "Policy Column", "User ID", and "Policy Value". "Policy ID" is optional.
#' @examples listedAllPdp <- listAllPdp(data = "DATAFRAME")
#' @export

listAllPdp <- function(data) {
  x <- data %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  y <- list()
  for (i in 1:nrow(x)) {if(nrow(x) == 0) {break} else{y[[i]] <- rHubs::listOnePdp(x[i, ])} }
  return(y)
}
