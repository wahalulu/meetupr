#' Converts the meetup time value to a POSIXct value in EST
#' @param meetup_time The the timestamp value provided by meetup
#' @return POSIXct object with EST timezone
#' @export
get_real_time <- function(meetup_time) {
  t <- as.POSIXct(as.numeric(meetup_time)/1000,
                  origin = "1970-01-01",
                  tz = "GMT")
  as.POSIXct(format(t, tz = "America/New_York"))
}
