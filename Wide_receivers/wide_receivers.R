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
  select(gameId, playId, receiver_id = nflId, receiver_club = teamAbbr, hadPassReception, wasTargettedReceiver)

# Only keeping tracking frames of receiver running their routes
tracking_all <- tracking |> 
  inner_join(plays_pass_receivers,
             by = c("gameId", "playId", "nflId" = "receiver_id"))

# Filtering frames for when the ball is snapped (one frame after) to when the pass arrives to a receiver
tracking_all <- tracking_all |> 
  group_by(gameId, playId, nflId) |> 
  mutate(
    frame_go = frameId[which(event == "ball_snap")][1] + 1,
    frame_arrived = frameId[which(event == "pass_arrived")][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_go), !is.na(frame_arrived)) |> 
  filter(frameId >= frame_go & frameId <= frame_arrived)

# Filtering to eliminate wide receivers with low number of routes ran 
wrs <- plays_pass_receivers |> 
  count(receiver_id)

summary(wrs) # Median is 88 

wrs <- wrs |> 
  filter(n >= 88) |> 
  select(nflId = receiver_id)

# Filtering wide receivers who have ran 25 routes or more
tracking_filtered <- tracking_all |> 
  filter(nflId %in% wrs$nflId)

# Calculating directional acceleration for wide receivers
tracking_wrs <- tracking_filtered |> 
  mutate(dir_a = ifelse((s_x ^ 2 + s_y^2) == 0, 0, (s_x * a_x2 + s_y * a_y2) / sqrt(s_x ^ 2 + s_y^2)), # accounts for when speed = 0
         dir_a_mphs = dir_a * (3600/1760),
         s_mph = s * (3600/1760))

# Wide receiver names
wrs_names <- tracking_wrs |> 
  distinct(displayName)


# Rolling average ---------------------------------------------------------

#Example
#first 4 rows are NA
#frame 5's value= avg of frames 1-5
#frame 6's value= avg of frames 2-6
#frame 7's value=avg of frames 3-7

library(zoo) #for rollmean() function

tracking_wrs_avg <- tracking_wrs |>
  arrange(gameId, playId, nflId, frameId) |> 
  group_by(gameId, playId, nflId, displayName) |> 
  mutate(s_5 = rollmean(s_mph, k=5, fill=NA, align = "right"),
         dir_a_mphs_5 = rollmean(dir_a_mphs, k=5, fill=NA, align="right")) |> 
  ungroup() |> 
  filter(!is.na(s_5), !is.na(dir_a_mphs_5))


# Example Data Table ------------------------------------------------------

library(gt)
library(gtExtras)

hill <- tracking_wrs |> 
  filter(displayName == "Tyreek Hill")

tracking_wrs |> 
  filter(displayName == "Tyreek Hill") |> 
  slice_head(n = 3) |> 
  select(gameId, playId, nflId, displayName, frameId, event, x, y, s_mph, dir_a_mphs, wasTargettedReceiver) |> 
  gt() |> 
  cols_label(gameId = "Game", playId = "Play", nflId = "Receiver", displayName = "Name",
             frameId = "Frame", event = "Event", x = "X Coordinate", y = "Y Coordinate",
             s_mph = "Speed (MPH)", dir_a_mphs = "Directional Acceleration (MPH/S)",
             wasTargettedReceiver = "Targeted?") |> 
  gt_theme_espn() |> 
  gtsave(file = "WRsTable.png")
