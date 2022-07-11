#' Convert 'UNIX' to Date
#'
#' This function allows you to conveniently convert 'UNIX' values to date values.
#' @param unix The 'UNIX' values to be converted to date.
#' @param tz Stands for 'Time Zone' which defaults to 'GMT'. This input specifies the time zone of the 'UNIX' values.
#' @param type The second granularity of the 'UNIX' values. Valid inputs include: 'Millisecond', 'Microsecond', and 'Nanosecond'.
#' @examples output <- dateToUnix(
#'   unix = unix,
#'   tz = "GMT",
#'   type = "Millisecond")
#' @export

unixToDate <- function(unix, tz = "GMT", type = "Second") {
  timeZone <- case_when(tz == 'GMT' ~ 'GMT', TRUE ~ tz)
  secondType <- case_when(type == 'Second' ~ 1, type == 'Millisecond' ~ 1000, type == 'Microsecond' ~ 1000000, type == 'Nanosecond' ~ 1000000000, TRUE ~ 1)
  a <- as.POSIXct(unix/secondType, origin="1970-01-01", tz = timeZone)
  return(a)
}
