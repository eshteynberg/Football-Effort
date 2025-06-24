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

# # write the combined tracking data object into a parquet file
# library(arrow)
# arrow::write_parquet(tracking, "data/tracking.parquet")

tracking <- arrow::read_parquet("data/tracking.parquet")

games <- read_csv("data/games.csv")

players <- read_csv("data/players.csv")
head(tracking, 10)
