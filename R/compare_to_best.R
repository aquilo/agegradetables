# source("R/utils.R")

#' Compare a Running Result
#'
#' @param mytime character. Time as HH:MM:SS
#' @param distance number (in kilometers)
#' @param gender character ("F" or "M")
#' @param age number (in years)

#'
#' @return A number (percentage of best in this distance/age/gender combination); 100 would be the world record.
#' @export
#'
#' @examples
#' compare_to_best("4:11:00", 42.190, "M", 51)  # Marathon
#' compare_to_best("0:54:33", 10.000, "F", 36)  # 10k
#' compare_to_best("1:38:15", 21.6648, "M", 40)  # Half marathon with some slope correction

# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
# https://r-pkgs.org

compare_to_best <- function(mytime, distance, gender, age) {
  interpolated_seconds <- perform_interpolation(distance, gender, age, "record_sec")
  interpolated_coeff <- perform_interpolation(distance, gender, age, "coeff")
  100 * interpolated_seconds / time_to_seconds(mytime) / interpolated_coeff
}

# Utility function for interpolation

#' Interpolate a specific column out of the agt_coefficients
#' @param distance number (in kilometers)
#' @param gender character ("F" or "M")
#' @param age number (in years)
#' @param column character ("record_sec" of "coeff")
#' @keywords internal
perform_interpolation <- function(distance, gender, age, column) {

  # Filter the data for the specific gender and age
  # filtered_data <- agt_coefficients |>
  #   filter(gender == gender & age == age) |>
  #   filter(distance > distance / 2) |>
  #   arrange(distance)
  # print(filtered_data)

  d <- agegradetables::agt_coefficients
  d <- d[d$gender == gender,]
  d <- d[d$age == age,]
  d <- d[d$distance > (distance / 2),]
  filtered_data <- d

  # Perform linear interpolation
  interpolation <- stats::approx(filtered_data$distance, filtered_data[[column]], xout = as.numeric(distance))
  return(interpolation$y)
}

#' Seconds from time string
#' @param time_str character. Time as HH:MM:SS
#' @keywords internal
time_to_seconds <- function(time_str) {
  time <- lubridate::hms(time_str)
  seconds <- lubridate::period_to_seconds(time)
  return(seconds)
}

#' Time string to seconds
#' @param seconds number
#' @keywords internal
convert_seconds_to_hms <- function(seconds) {
  period <- lubridate::seconds_to_period(seconds)
  return(sprintf("%d:%02d:%02d", period@hour, lubridate::minute(period), lubridate::second(period)))
}
