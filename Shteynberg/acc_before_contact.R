library(tidyverse)
library(arrow)
# Quang Stuff --------------------------------------------------------------------
games <- read_csv("data/games.csv")
plays <- read_csv("data/plays.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
tracking <- arrow::read_parquet("data/tracking.parquet")

## TRACKING FOR RUNNING BACKS
theme_set(theme_bw())

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

## TRACKING FOR RUNNING BACKS ONLY FOR RUNNING BACK PLAYS
# Tracking data for ball carriers only
tracking_bc <- tracking_rb_runs |> 
  filter(nflId == bc_id) |> 
  left_join(select(players, nflId, weight)) |> 
  mutate(ke = 0.5 * weight * s^2,
         m_x = weight * s_x, # momentum in the x
         dis_x = ifelse(gameId == lag(gameId) & playId == lag(playId), x - lag(x), NA),
         work = ifelse(gameId==lag(gameId) & playId == lag(playId), ke-lag(ke), NA),
         positive_work = ifelse(gameId==lag(gameId) & playId == lag(playId), pmax(ke-lag(ke),0), NA),
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA),
         COD = ifelse(gameId==lag(gameId) & playId == lag(playId), abs(dir - lag(dir)), NA))

View(tracking_bc)

# Running back metrics per play and for weeks 1-9------------------------------------------------------
# Running back stats per play
rb_stats_per_play <- tracking_bc |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained = sum(dis),
            mean_ke = mean(ke),
            mean_m_x = mean(m_x),
            sd_ke=sd(ke),
            dis_gained_x = sum(dis_x, na.rm = TRUE),
            mean_pos_work = mean(positive_work, na.rm=TRUE),
            sd_pos_work = sd(positive_work, na.rm=TRUE),
            effort_consistency = mean_ke/sd_ke,
            total_pos_work=sum(positive_work, na.rm=TRUE),
            avg_accel = mean(a),
            avg_jerk = mean(jerk, na.rm=TRUE),
            avg_COD = mean(COD, na.rm = TRUE) / n()) |> 
  ungroup() |> 
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded))

# Overall rb stats
rb_stats_total <- rb_stats_per_play |> 
  group_by(bc_id, displayName) |> 
  summarize(
    total_dis_gained = sum(dis_gained),
    total_dis_gained_x = sum(dis_gained_x, na.rm = TRUE),
    avg_dis_gained_x = mean(dis_gained_x, na.rm=TRUE) / n(),
    mean_ke = mean(mean_ke),
    avg_sd_ke=mean(sd_ke, na.rm=TRUE),
    avg_sd_work= mean(sd_pos_work, na.rm=TRUE),
    avg_effort_consistency =mean(effort_consistency, na.rm=TRUE),
    mean_m_x = mean(mean_m_x),
    mean_pos_work = mean(mean_pos_work, na.rm=TRUE),
    total_pos_work = sum(total_pos_work, na.rm=TRUE),
    total_yards_gained = sum(yardsGained),
    avg_yards_gained = mean(yardsGained),
    avg_EPA = mean(expectedPointsAdded),
    num_of_rushes = n(),
    avg_accel = mean(avg_accel),
    avg_jerk = mean(avg_jerk),
    avg_COD = mean(avg_COD)
  ) |> 
  ungroup()


rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20)

#LABELING "starters" vs "backups"-------------------------
#get each RB's primary team based on where they had the most rushes
#basically most-played-for team (bc_club) based on rushes
primary_teams <- plays_rb_runs |> 
  semi_join(rb_stats_total_filtered, by="bc_id") |> #only look at the 69 RBs
  group_by(bc_id, bc_club) |> 
  summarise(rushes=n(), .groups="drop") |> #count how many rushes each player had for each team 
  group_by(bc_id) |> 
  slice_max(order_by=rushes,n=1,with_ties=FALSE) #top team per player 


#append players' primary team names to rb_stats_total_filtered
rb_stats_teams <- rb_stats_total_filtered |> 
  left_join(primary_teams, by="bc_id")

#get top rushers per team
top_rushers_labeled <- rb_stats_teams |> 
  group_by(bc_club) |> 
  slice_max(order_by=rushes, n=1, with_ties=FALSE) |> #pick single top rusher for each team
  mutate(starter=TRUE) #mark the top rushers with starter=TRUE

#join starter labels to rb_stats_teams
rb_stats_labeled <- rb_stats_teams |> 
  left_join(top_rushers_labeled |> #join starter label from top rusher df, matching on player name and team
              select(bc_id, bc_club, starter),
            by=c("bc_id", "bc_club")) |> 
  mutate(starter=ifelse(is.na(starter), FALSE, starter)) #if NA after join, then label FALSE for starter column. if not, then label original value from starter column (TRUE)


#ACCELERATION BEFORE CONTACT----------------------------------------------
#get change in acc 10 and 5 frames before first contact
acc_before_contact <- tracking_bc |> 
  #semi_join(rb_stats_teams, by="displayName") |>  #only include plays from the 69 RBs
  group_by(gameId, playId) |> 
  mutate(frame_contact = frameId[which(event=="first_contact")][1]) |> #find first frame ID where event is first_contact and store in new frame_contact column 
  filter(!is.na(frame_contact)) |> 
  #look up acc at 10 and 5 frames before contact, take [1] to avoid multiple matches
  #reframe() can return multiple rows per group (unlike summarise), we extract the first one
  #basically collapse each group (play) into one row
  reframe(
    displayName=displayName[1],
    acc_10_before=a[which(frameId == frame_contact-10)][1],
    acc_5_before=a[which(frameId==frame_contact-5)][1]) |> 
  filter(!is.na(acc_10_before), !is.na(acc_5_before)) |> 
  mutate(acc_change=acc_5_before - acc_10_before,
         label=case_when(
           acc_change > 0.5 ~ "acc",
           acc_change < -0.5~ "dec",
           TRUE ~ "maintain")) 

tracking_bc_play_stats_acc <- left_join(tracking_bc_play_stats, acc_before_contact, by=c("gameId", "playId", "displayName"))
rb_stats_labeled_2 <- rb_stats_labeled |> 
  select(starter, displayName)
tracking_bc_play_stats_acc <- left_join(tracking_bc_play_stats_acc, rb_stats_labeled_2, by=c("displayName"))

#number of times player accelerates and percentage of time out of their plays
num_acc_player <- tracking_bc_play_stats_acc |> 
  group_by(displayName) |> 
  filter(starter==TRUE) |> 
  summarise(num_frames = sum(!is.na(label)),
  num_acc_frames = sum(label=="acc", na.rm=TRUE),
  acc_frames_ratio = num_acc_frames/num_frames
  ) |> 
  ungroup()
