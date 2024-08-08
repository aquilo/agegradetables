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
#' get_agt_grade("4:11:00", 42.190, "M", 51) # Marathon
#' get_agt_grade("0:54:33", 10.000, "F", 36) # 10k
#' get_agt_grade("1:38:15", 21.6648, "M", 40) # Half marathon with some slope correction
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
# https://r-pkgs.org
get_agt_grade <- function(mytime, distance, gender, age) {
  if (is.character(mytime)) {
    # Convert time string to seconds
    seconds <- time_to_seconds(mytime)
  } else if (is.numeric(mytime)) {
    # Input is already in seconds
    seconds <- mytime
  } else {
    stop(paste("Invalid input: must be a time string in 'HH:MM:SS'",
               "format or a numeric value representing seconds."))
  }

  interpolated <- perform_interpolation(distance, gender, age)
  100 * interpolated[1] / seconds / interpolated[2]
}

# Utility function for interpolation

#' Interpolate a specific column out of the agt_coefficients
#' @param distance number (in kilometers)
#' @param gender character ("F" or "M")
#' @param age number (in years)
#' @return an array of 2: interpolated seconds, interpolated coefficient
#' @keywords internal
perform_interpolation <- function(distance, gender, age) {
  filtered_data <- get_records(gender, age)
  interpolation_s <- stats::approx(filtered_data$distance,
                                   filtered_data$record_sec,
                                   xout = as.numeric(distance))
  interpolation_c <- stats::approx(filtered_data$distance,
                                   filtered_data$coeff,
                                   xout = as.numeric(distance))
  return(c(interpolation_s$y, interpolation_c$y))
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
  if (!is.numeric(seconds)) {
    stop("Input must be numeric")
  }

  # Calculate hours, minutes, and seconds
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60)

  # Format as HH:MM:SS
  hms <- sprintf("%02d:%02d:%02d", hours, minutes, secs)

  return(hms)
}

# convert_seconds_to_hms <- function(seconds) {
#   period <- lubridate::seconds_to_period(round(seconds))
#   return(sprintf("%d:%02d:%02d",
#       period@hour, lubridate::minute(period), lubridate::second(period)))
# }

# Function to convert time string to seconds
# time_to_seconds <- function(time_str) {
#   time_parts <- unlist(strsplit(time_str, ":"))
#   hours <- as.numeric(time_parts[1])
#   minutes <- as.numeric(time_parts[2])
#   seconds <- as.numeric(time_parts[3])
#   total_seconds <- hours * 3600 + minutes * 60 + seconds
#   return(total_seconds)
# }
