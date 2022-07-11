#' Convert Date to 'UNIX'
#'
#' This function allows you to conveniently convert date values to 'UNIX' values.
#' @param date The date values or column to be converted to 'UNIX'.
#' @param tz Stands for 'Time Zone' which defaults to 'GMT'. This input governs the time zone of the output.
#' @param type The granularity of the second can be set using this parameter which defaults to 'Second'. Valid inputs include: 'Millisecond', 'Microsecond', and 'Nanosecond'.
#' @examples output <- dateToUnix(
#'   date = date,
#'   tz = "GMT",
#'   type = "Millisecond")
#' @export

dateToUnix <- function(date, tz = "GMT", type = "Second") {
  timeZone <- case_when(tz == 'GMT' ~ 'GMT', TRUE ~ tz)
  secondType <- case_when(type == 'Second' ~ 1, type == 'Millisecond' ~ 1000, type == 'Microsecond' ~ 1000000, type == 'Nanosecond' ~ 1000000000, TRUE ~ 1)
  a <- as.Date(date)
  a <- as.POSIXct(a, tz = timeZone)
  a <- as.numeric(a) * secondType
  return(a)
}
