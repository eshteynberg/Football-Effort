library(tidyverse)

# Original data frames
tracking <- arrow::read_parquet("data/tracking.parquet")
games <- read_csv("data/games.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
plays <- read_csv("data/plays.csv")

# Putting plays in the correct direction
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

# Getting velocity in the x (endzone) and y (sideline) direction
tracking <- tracking |>
  mutate(
    # Converting degrees into radians
    dir_rad = pi * (dir / 180),
    # get angle of motion in x (endzone) and y (sideline) direction
    # NA checks are for the ball
    dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
    dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),
    # directional speed (also known as velocity)
    s_x = dir_x * s,
    s_y = dir_y * s,
    # directional acceleration
    a_x = dir_x * a,
    a_y = dir_y * a,
    a_x2 = ((s_x - lag(s_x)) / 0.1),
    a_y2 = ((s_y - lag(s_y)) / 0.1)
  )  

# Removing frames before the snap
tracking <- tracking |> 
  filter(frameType != "BEFORE_SNAP")

# Calculating yards from endzone
plays <- plays |> 
  mutate(yards_from_endzone =
           ifelse((possessionTeam != yardlineSide) |
                    (yardlineNumber == 50), yardlineNumber,
                  100 - yardlineNumber),
         adj_x_first_down = yards_from_endzone - yardsToGo)

# Filtering for wide receivers who ran routes on the play
plays_pass_receivers <- player_play |> 
  filter(wasRunningRoute == 1) |> 
  left_join(select(players, nflId, position)) |>
  filter(position == "WR") |>
  select(gameId, playId, receiver_id = nflId, receiver_club = teamAbbr)

tracking_all <- tracking |> 
  inner_join(plays_pass_receivers)
