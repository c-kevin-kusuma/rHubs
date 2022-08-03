#' Reorder 'PDP' List
#'
#' This function allows you to prepare your dataframe to be compared to the current 'PDP' list on DOMO by reordering the user id and policy value.
#' @param data The dataframe to reorder, should include "Policy Name", "Policy Column", "User ID", and "Policy Value".
#' @examples reorderedPdp <- reorderPdp(data = "DATAFRAME")
#' @export

reorderPdp <- function(data) {
  x <- data %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  z <- list()
  for (i in 1:nrow(x)) {
    if(nrow(x) == 0){break} else{
      di = x$`Dataset ID`[i]
      pm = x$`Policy Name`[i]
      pc = x$`Policy Column`[i]

      `User ID` <- dplyr::tibble(`User ID` = strsplit(x$`User ID`[i], '|', fixed = TRUE) %>% unlist() %>% as.character())
      `User ID` <- `User ID` %>% dplyr::arrange(`User ID`) %>% dplyr::group_by() %>% dplyr::summarise(`User ID` = paste(`User ID`, collapse = '|'))

      `Policy Value` <- dplyr::tibble(`Policy Value` = strsplit(x$`Policy Value`[i], '|', fixed = TRUE) %>% unlist() %>% as.character())
      `Policy Value` <- `Policy Value` %>% dplyr::arrange(`Policy Value`) %>% dplyr::group_by() %>% dplyr::summarise(`Policy Value` = paste(`Policy Value`, collapse = '|'))

      z[[i]] <- dplyr::tibble(`Dataset ID` = di, `Policy Name` = pm, `Policy Column` = pc, `User ID` = `User ID`$`User ID`, `Policy Value` = `Policy Value`$`Policy Value`)
    }
  }
  z <- dplyr::bind_rows(z)
}
