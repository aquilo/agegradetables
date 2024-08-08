## code to prepare `agt_coefficients` dataset goes here
rm(list = ls())


library(dplyr)
library("lubridate")

# Function to process the file
process_file <- function(filename) {
  # Extract the code from the filename
  file_code <- sub(".*\\.(\\w+)$", "\\1", filename)

  # Read all lines from the file
  lines <- readLines(filename)

  data <- read.table(text = lines[1:2], header = FALSE,
                     fill = TRUE, col.names = c("gender", "record"))
  df1 <- data %>%
    mutate(track = file_code)

  # Read the remaining lines into a data frame
  data <- read.table(text = lines[3:length(lines)], header = FALSE,
                     fill = TRUE, col.names = c("gender", "age", "coeff"))

  # Create the first data frame with additional columns
  df2 <- data %>%
    mutate(track = file_code)

  return(list(df1 = df1, df2 = df2))
}

process_directory <- function(directory) {
  files <- list.files(directory, full.names = TRUE)
  all_df1 <- list()
  all_df2 <- list()

  for (file in files) {
    result <- process_file(file)
    all_df1 <- append(all_df1, list(result$df1))
    all_df2 <- append(all_df2, list(result$df2))
  }

  # Bind all data frames together
  final_df1 <- bind_rows(all_df1)
  final_df2 <- bind_rows(all_df2)

  return(list(df1 = final_df1, df2 = final_df2))
}

time_to_seconds <- function(time_str) {
  time <- lubridate::hms(time_str)
  seconds <- lubridate::period_to_seconds(time)
  return(seconds)
}


# Example usage
directory <- "data-raw/2020_Files/2020_RunScore"
result <- process_directory(directory)
tracks <- result$df1
coeff <- result$df2

unique(tracks$track)

laengen <- c(
  "100k", 100000,
  "100mi", 160934,
  "10k", 10000,
  "10mi", 16093,
  "12k", 12000,
  "150k", 150000,
  "15k", 15000,
  "1mi", 1609,
  "200k", 200000,
  "20k", 20000,
  "25k", 25000,
  "30k", 30000,
  "42k", 42190,
  "4mi", 6437,
  "50k", 50000,
  "50mi", 80477,
  "5k", 5000,
  "5mi", 8047,
  "6k", 6000,
  "8k", 8000,
  "hm", 21097.5
)
# Create a matrix and then convert it to a data frame
matrix_data <- matrix(laengen, ncol = 2, byrow = TRUE)
lengths <- data.frame(track = matrix_data[, 1], distance = matrix_data[, 2])

agt_coefficients <- coeff |>
  left_join(tracks, by = c("gender", "track")) |>
  left_join(lengths, by = c("track")) |>
  mutate(
    record_sec = time_to_seconds(record),
    distance = as.numeric(distance) / 1000,
  ) |>
  select(-c(record, track))


### Data initialized

rm(coeff, matrix_data, result, laengen)

usethis::use_data(agt_coefficients, overwrite = TRUE)
