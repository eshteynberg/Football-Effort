library(tidyverse)
library(arrow)

# first, load and stack all tracking data into one single object

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
#arrow::write_parquet(tracking, "data/tracking.parquet")

# every time a new session is started
# just read the single combined file
# should take ~10 sec
#tracking <- arrow::read_parquet("data/tracking.parquet")

#head(tracking)

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
         positive_work = ifelse(gameId==lag(gameId) & playId == lag(playId), pmax(ke-lag(ke),0), NA))

View(tracking_bc)

# Running back metrics per play and for weeks 1-9------------------------------------------------------
# Running back stats per play
rb_stats_per_play <- tracking_bc |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained = sum(dis),
            mean_ke = mean(ke),
            mean_m_x = mean(m_x),
            dis_gained_x = sum(dis_x, na.rm = TRUE),
            mean_pos_work = mean(positive_work, na.rm=TRUE),
            total_pos_work=sum(positive_work, na.rm=TRUE),
            avg_accel = mean(a)) |> 
  ungroup() |> 
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded))

# Overall rb stats
rb_stats_total <- rb_stats_per_play |> 
  group_by(bc_id, displayName) |> 
  summarize(
    total_dis_gained = sum(dis_gained),
    total_dis_gained_x = sum(dis_gained_x, na.rm = TRUE),
    mean_ke = mean(mean_ke),
    mean_m_x = mean(mean_m_x),
    mean_pos_work = mean(mean_pos_work, na.rm=TRUE),
    total_pos_work = sum(total_pos_work, na.rm=TRUE),
    total_yards_gained = sum(yardsGained),
    avg_yards_gained = mean(yardsGained),
    avg_EPA = mean(expectedPointsAdded),
    num_of_rushes = n(),
    avg_accel = mean(avg_accel)
  ) |> 
  ungroup()


summary(rb_stats_total$num_of_rushes) # Do a minimum of 20 rushes to eliminate players with low rushes

rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20)

# Visualizations
# Distance gained x

distance <- rb_stats_per_play |> 
  pivot_longer(c(dis_gained, dis_gained_x, yardsGained),
               names_to = "distance_type",
               values_to = "dis") |> 
  select(displayName, distance_type, dis)

# Mapping out distance per play averages
distance |> 
  ggplot(aes(x = dis)) +
  geom_density(aes(fill = distance_type), alpha = .5) +
  xlim(-20, 75)

# Average Kinetic energy per play
rb_stats_per_play |> 
  ggplot(aes(x = mean_ke)) +
  geom_histogram()

#top avg KE players
rb_stats_total |> 
  slice_max(mean_ke, n=10) |> 
  ggplot(aes(x=reorder(displayName, mean_ke), y=mean_ke))+
  geom_col()+
  coord_flip()+
  labs(
    title="Top 10 RBs by Avg KE Across All Plays",
    x="Avg KE (joules)",
    y=NULL
  )

#top players by total dist gained in x-direction
rb_stats_total |> 
  slice_max(total_dis_gained_x, n=10) |> 
  ggplot(aes(x=reorder(displayName, total_dis_gained_x), y=total_dis_gained_x))+
  geom_col()+
  coord_flip()+
  labs(
    title="Top 10 RBs by Total Yards Gained in x-direction",
    x="Total Yards Gained in x-direction",
    y=NULL
  )

#top RBs by total pos work
rb_stats_total |> 
  slice_max(total_pos_work, n=10) |> 
  ggplot(aes(x=reorder(displayName, total_pos_work), y=total_pos_work))+
  geom_col()+
  coord_flip()+
  labs(
    title="Top 10 RBs by Total Positive Work Across All Plays",
    x="Total Work (Joules)",
    y=NULL
  )

#top 10 players by Avg positive work across all plays
rb_stats_total |> 
  slice_max(mean_pos_work, n=10) |> 
  ggplot(aes(x=reorder(displayName, mean_pos_work), y=mean_pos_work))+
  geom_col()+
  coord_flip()+
  labs(
    title="Top 10 RBs by Avg Positive Work Across All Plays",
    x="Total Work (Joules)",
    y=NULL
  )


#work displacement ratio by RB - who has to work harder per yard of horiz displacement?
rb_stats_total |> 
  filter(num_of_rushes>=20) |> 
  mutate(work_per_disp = total_pos_work / total_dis_gained_x) |> 
  slice_max(work_per_disp, n=10) |> 
  ggplot(aes(x=reorder(displayName, work_per_disp), y=work_per_disp)) +
  geom_col()+
  coord_flip()+
  labs(title="Top 10 RBs by Work per Horizontal Displacement (Yards)")

#work output ratio by RB - who has to work harder per yard gained for the team?
rb_stats_total |> 
  filter(num_of_rushes>=20) |> 
  mutate(work_per_yard = total_pos_work / total_yards_gained) |> 
  slice_max(work_per_yard, n=10) |> 
  ggplot(aes(x=reorder(displayName, work_per_yard), y=work_per_yard)) +
  geom_col()+
  coord_flip()+
  labs(title="Top 10 RBs by Work per Yard Gained for the Team")


#RBs with highest avg acceleration
rb_stats_total |> 
  slice_max(avg_accel, n=10) |> 
  ggplot(aes(x=reorder(displayName, avg_accel), y=avg_accel))+
  geom_col()+
  coord_flip()

#RBs with lowest EPA despite high work
rb_stats_total |> 
  filter(num_of_rushes>=20) |> 
  arrange(avg_EPA) |> 
  slice_max(total_pos_work, n=10) |> 
  ggplot(aes(x=reorder(displayName, -avg_EPA), y=total_pos_work))+
  geom_col()+
  coord_flip()+
  geom_text(aes(label=round(avg_EPA,2)),
            hjust=-0.1, size=3.5)

#top 10 EPA RBs, bar plot of their work
rb_stats_total |> 
  filter(num_of_rushes>=20) |> 
  slice_max(avg_EPA, n = 10) |> 
  ggplot(aes(x=reorder(displayName, avg_EPA), y=total_pos_work))+
  geom_col()+
  coord_flip()+
  geom_text(aes(label=round(avg_EPA,2)),
            hjust=-0.1, size=3.5)

#bottom 10 EPA RBs, bar plot of their work
rb_stats_total |> 
  filter(num_of_rushes>=20) |> 
  slice_min(avg_EPA, n = 10) |> 
  ggplot(aes(x=reorder(displayName, -avg_EPA), y=total_pos_work))+
  geom_col()+
  coord_flip()+
  geom_text(aes(label=round(avg_EPA,2)),
            hjust=-0.1, size=3.5)


rb_stats_per_play |> 
  ggplot(aes(x=dis_gained, y=yardsGained))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  labs(title="Total Movement vs Yards Gained")

rb_stats_total|> 
  ggplot(aes(x = total_dis_gained_x)) +
  geom_density(fill="lightgrey", alpha = 0.5) +
  labs(title = "Distribution of Total Displacement in x-direction Across All Plays")



rb_work_flips <- tracking_bc |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  mutate(
    work_sign=sign(work),
    prev_sign=lag(work_sign),
    sign_change=ifelse(!is.na(prev_sign) & work_sign!=prev_sign, 1, 0)) |> 
    summarize(
      num_sign_changes= sum(sign_change, na.rm=TRUE)
    ) |> 
  ungroup()

rb_stats_per_play_with_flips <- rb_stats_per_play |> 
  left_join(rb_work_flips, by = c("gameId", "playId", "bc_id", "displayName"))

rb_stats_per_play_with_flips |> 
  slice_max(num_sign_changes, n=10) |> 
  ggplot(aes(x=reorder(displayName, num_sign_changes), y=num_sign_changes))+
  geom_col()+
  coord_flip()
  

  
# rb_work_ratio <- tracking_bc |> 
#   group_by(playId, gameId, bc_id, displayName) |> 
#   summarise(
#     num_frames = sum(!is.na(dis_x)),
#     num_positive_frames = sum(positive_work>0, na.rm=TRUE),
#     positive_work_ratio= num_positive_frames/num_frames
#   ) |> 
#   ungroup()
# 
# rb_stats_per_play_with_ratio <- rb_stats_per_play |> 
#   left_join(rb_work_ratio, by=c("gameId", "playId", "bc_id", "displayName"))



#View(rb_stats_total)
#View(rb_test)

## EDA-------------------------------------

#speed and acceleration, work, power
#filter by total plays to see which ones have most KE


# "KE efficiency" = output per unit of KE
rb_stats_per_play <- rb_stats_per_play |> 
  mutate(
    yards_per_ke = yardsGained / mean_ke,
    epa_per_ke = expectedPointsAdded / mean_ke
  ) 

