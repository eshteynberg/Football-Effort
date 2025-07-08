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
player_play <- read_csv("data/player_play.csv")
plays <- read_csv("data/plays.csv")

## Cleaning
# Making sure players go in the same direction
tracking <- tracking |>
  mutate(
    # Plays will always go from left to right
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    # flip player direction and orientation
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

# Test
head <- head(tracking, 10)