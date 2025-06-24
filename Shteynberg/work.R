library(tidyverse)

#tracking_week_1 <- read_csv("C:/Users/emily/Downloads/nfl-big-data-bowl-2025/tracking_week_1.csv")
#tracking_week_2 <- read_csv("C:/Users/emily/Downloads/nfl-big-data-bowl-2025/tracking_week_2.csv")
#tracking_week_3 <- read_csv("C:/Users/emily/Downloads/nfl-big-data-bowl-2025/tracking_week_3.csv")


# first, load and stack all tracking data into one single object

library(tidyverse)
# tracking <- read_csv("data/tracking_week_1.csv") |>
#   bind_rows(read_csv("data/tracking_week_2.csv")) |>
#   bind_rows(read_csv("data/tracking_week_3.csv")) |>
#   bind_rows(read_csv("data/tracking_week_4.csv")) |>
#   bind_rows(read_csv("data/tracking_week_5.csv")) |>
#   bind_rows(read_csv("data/tracking_week_6.csv")) |>
#   bind_rows(read_csv("data/tracking_week_7.csv")) |>
#   bind_rows(read_csv("data/tracking_week_8.csv")) |>
#   bind_rows(read_csv("data/tracking_week_9.csv"))

# see here for installing the arrow package
# (there might be some installation issues depending on the operating system)
# https://arrow.apache.org/docs/r/articles/install.html

# write the combined tracking data object into a parquet file
#library(arrow)
#arrow::write_parquet(tracking, "data/tracking.parquet")

# every time a new session is started
# just read the single combined file
# should take ~10 sec
tracking <- arrow::read_parquet("data/tracking.parquet")

games <- read_csv("data/games.csv")

