library(tidyverse)

tracking <- arrow::read_parquet("data/tracking.parquet")
games <- read_csv("data/games.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
plays <- read_csv("data/plays.csv")

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

# Don't care about pre-snap plays
tracking <- tracking |> 
  filter(frameType != "BEFORE_SNAP")

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
    a_y = dir_y * a
  )  

plays <- plays |> 
  mutate(yards_from_endzone = ifelse((possessionTeam != yardlineSide) | (yardlineNumber == 50), 
                                     yardlineNumber, 100 - yardlineNumber),
         adj_x_first_down = yards_from_endzone - yardsToGo)

# Filtering for all running back plays
# bc_id = ball carrier id, bc_club = team id
plays_rb_runs <- player_play |> 
  # Make sure it's a running play
  filter(hadRushAttempt == 1) |> 
  left_join(select(players, nflId, position)) |> 
  filter(position == "RB") |> 
  select(gameId, playId, bc_id = nflId, bc_club = teamAbbr)

# Tracking data will now show only plays with running backs
tracking_rb_runs <- tracking |> 
  inner_join(plays_rb_runs)

## TRACKING FOR ALL RUNNING BACK PLAYS
# Keeping only frames between handoff and end of play event (out of bounds, tackle, TD)
tracking_rb_runs <- tracking_rb_runs |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_handoff = frameId[which(event == "handoff")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_handoff), !is.na(frame_end)) |> 
  filter(frameId >= frame_handoff & frameId <= frame_end)

# This data frame shows tracking only for running backs
tracking_bc_quang <- tracking_rb_runs |> 
  filter(nflId == bc_id) |> 
  select(gameId, playId, frameId, 
         bc_id, bc_club,
         bc_x = x, bc_y = y, bc_s = s, bc_a = a,
         bc_dis = dis, bc_o = o, bc_dir = dir) |> 
  mutate(adj_bc_x = 110 - bc_x, # Adjusting for the endzone
         adj_bc_y = bc_y - (160 / 6)) |> 
  left_join(select(plays, gameId, playId, adj_x_first_down)) |> 
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down,
         bc_position = "RB",
         bc_type = "rusher")

# Finding the nearest defender
tracking_def <- tracking_rb_runs |> 
  filter(club != bc_club, displayName != "football") |> 
  left_join(select(tracking_bc_quang, gameId, playId, frameId,
                   bc_x, bc_y, adj_bc_x, adj_bc_y, bc_s, bc_a),
            by = c("gameId", "playId", "frameId")) |> 
  mutate(dist_to_bc = sqrt((x - bc_x) ^ 2 + (y - bc_y) ^ 2)) |> 
  group_by(gameId, playId, frameId) |>
  arrange(dist_to_bc) |> 
  mutate(player_dist_bc_rank = row_number()) |> 
  ungroup() |> 
  filter(player_dist_bc_rank == 1) |> 
  select(gameId, playId, frameId, playDirection,
         nflId, displayName,
         dist_to_bc, def_x = x, def_y = y, def_s = s, def_a = a,
         bc_x, bc_y, adj_bc_x, adj_bc_y) |> 
  mutate(adj_x = 110 - def_x,
         adj_y = def_y - (160 / 6),
         adj_x_change = adj_bc_x - adj_x, adj_y_change = adj_bc_y - adj_y,
         angle_with_bc = atan2(adj_y_change, -adj_x_change)) |> 
  select(-bc_x, -bc_y, -adj_bc_x, -adj_bc_y) |> 
  left_join(select(tracking_bc_quang, gameId, playId, frameId, bc_id, bc_club,
                   bc_x, bc_y, adj_bc_x, adj_bc_y, bc_s, bc_a),
            by = c("gameId", "playId", "frameId")) |> 
  arrange(gameId, playId, frameId)


# Joining cols from plays with the tracking_def data frame
plays_filtered <- plays |> 
  select(gameId, playId, preSnapVisitorScore, preSnapHomeScore, quarter, down, yardsToGo, yards_from_endzone)

tracking_def <- tracking_def |> 
  left_join(plays_filtered, by=c("gameId", "playId")) |> 
  left_join(select(games, gameId, homeTeamAbbr, visitorTeamAbbr)) |> 
  left_join(select(players, bc_id = nflId, weight)) |> 
  mutate(score_diff = ifelse(bc_club == visitorTeamAbbr, preSnapVisitorScore - preSnapHomeScore, 
                             preSnapHomeScore - preSnapVisitorScore),
         bc_s_mph = bc_s * (3600 / 1760),
         def_s_mph = def_s * (3600 / 1760),
         bc_a_mpsh = bc_a * (3600 / 1760),
         def_a_mpsh = def_a * (3600 / 1760),
         down = as.factor(down),
         quarter = as.factor(quarter))

