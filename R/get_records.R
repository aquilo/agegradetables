#' Get Record Times for a Specific Combination of Age and Gender
#'
#' @param gender character ("F" or "M")
#' @param age number (in years)

#'
#' @return A data.frame (a subset of agt_coefficents with columns coeff, distance, record_sec)
#' @export
#'
#' @examples
#' get_records("M", 51) # Marathon
get_records <- function(gender, age) {
  d <- agegradetables::agt_coefficients
  d <- d[d$gender == gender, ]
  d <- d[d$age == age, ]
  d <- d[, c("coeff", "distance", "record_sec")]
  d$hms <- convert_seconds_to_hms(as.numeric(d$record_sec))
  d <- d[order(d$distance), ]
  d
}
