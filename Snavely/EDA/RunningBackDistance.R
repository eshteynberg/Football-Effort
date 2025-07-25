## TRACKING FOR RUNNING BACKS
theme_set(theme_bw())

# Making sure tracking is same orientation
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
         COD = ifelse(gameId==lag(gameId) & playId == lag(playId), abs(dir - lag(dir)), NA),
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA))

tracking_bc_after_contact <- tracking_bc |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_contact = frameId[which(event == "first_contact")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_contact), !is.na(frame_end)) |> 
  filter(frameId >= frame_contact & frameId <= frame_end)

  

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
            avg_COD = mean(COD, na.rm = TRUE) / n(),
            avg_jerk = mean(jerk, na.rm=TRUE)) |> 
  ungroup() |> 
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded)) |> 
  left_join(select(player_play, playId, bc_id = nflId, gameId, rushingYards))

# Adding after contact stats
after_contact <- tracking_bc_after_contact |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained_x_ac = sum(dis_x),
            avg_accel_ac = mean(a)) |> 
  ungroup()

rb_stats_per_play <- rb_stats_per_play |> 
  left_join(after_contact) |> 
  mutate(dis_gained_x_ac = ifelse(is.na(dis_gained_x_ac), 0, dis_gained_x_ac),
         avg_accel_ac = ifelse(is.na(avg_accel_ac), 0, avg_accel_ac))

# Overall rb stats
rb_stats_total <- rb_stats_per_play |> 
  group_by(bc_id, displayName) |> 
  summarize(
    total_dis_gained = sum(dis_gained),
    total_dis_gained_x = sum(dis_gained_x, na.rm = TRUE),
    avg_dis_gained_x = mean(dis_gained_x, na.rm = TRUE) / n(),
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
    avg_COD = mean(avg_COD),
    avg_jerk = mean(avg_jerk),
    avg_dis_gained_ac = mean(dis_gained_x_ac),
    avg_acc_ac = mean(avg_accel_ac)
  ) |> 
  ungroup()

summary(rb_stats_total$num_of_rushes) # Do a minimum of 20 rushes to eliminate players with low rushes

rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20)

# Visualizations ----------------------------------------------------------

# Visualizations
# Distance gained x
distance <- rb_stats_per_play |> 
  pivot_longer(c(dis_gained, dis_gained_x, yardsGained, rushingYards),
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

# Distance gained histogram
rb_stats_per_play |> 
  ggplot(aes(x = dis_gained)) +
  geom_histogram()

# Top ke players
rb_stats_total_filtered |>
  slice_max(order_by = mean_ke, n = 10) |> 
  ggplot(aes(x = mean_ke, y = displayName)) +
  geom_col()

# Least ke players
rb_stats_total_filtered |>
  slice_min(order_by = mean_ke, n = 10) |> 
  ggplot(aes(x = mean_ke, y = displayName)) +
  geom_col()

# Top KE plays
rb_stats_per_play |>
  slice_max(order_by = mean_ke, n = 10) |> 
  ggplot(aes(x = mean_ke, y = as.character(playId))) +
  geom_col() +
  geom_label(aes(label = displayName))

# Relationship between mean_ke and EPA
rb_stats_per_play |> 
  ggplot((aes(x = expectedPointsAdded, y = mean_ke))) +
  geom_point() +
  scale_x_log10()

# Relationship between mean_ke and dis_gained_x
rb_stats_per_play |> 
  ggplot((aes(x = dis_gained_x, y = mean_ke))) +
  geom_point() +
  scale_x_log10()

# jerk and rushing yards
rb_stats_per_play |> 
  ggplot((aes(x = avg_jerk, y = rushingYards))) +
  geom_point()

# KE and rushing yards
rb_stats_per_play |> 
  ggplot((aes(x = mean_ke, y = rushingYards))) +
  geom_point()

# Avg acc and rushing yards
rb_stats_per_play |> 
  ggplot((aes(x = avg_accel, y = rushingYards))) +
  geom_point()

# distance x after contact vs. EPA
rb_stats_per_play |> 
  ggplot((aes(x = dis_gained_x_ac, y = expectedPointsAdded))) +
  geom_point()

# jerk and epa
rb_stats_per_play |> 
  ggplot((aes(x = avg_jerk, y = expectedPointsAdded))) +
  geom_point()

# COD and epa
rb_stats_per_play |> 
  ggplot((aes(x = avg_COD, y = expectedPointsAdded))) +
  geom_point()

# jerk and rushing yards
rb_stats_per_play |> 
  ggplot((aes(x = avg_jerk, y = rushingYards))) +
  geom_point()


# Animating the toavg_accel# Animating the top KE play -----------------------------------------------

library(gganimate)
library(sportyR)
library(ggtext)
library(magick)

# Christian animation
# Week 1: Giants @ Titans
View(play_2462)

christian <- tracking_rb_runs |> 
  filter(playId == 2462,
         gameId == 2022092500) |> 
  mutate(team_col = case_when(club == "NO" ~ "#ceb53b",
                              club == "CAR" & displayName != "Christian McCaffrey" ~ "#007FC8",
                              club == "football" ~ "#964B00",
                              displayName == "Christian McCaffrey" ~ "#D50A0A"),
         size = case_when(club == "NO" ~ 5,
                          club == "CAR" ~ 5,
                          club == "football" ~ 3))

# Field characteristics
field_params <- list(
  field_apron = "springgreen3",
  field_border = "springgreen3",
  offensive_endzone = "springgreen3",
  defensive_endzone = "springgreen3",
  offensive_half = "springgreen3",
  defensive_half = "springgreen3"
)

field_background <- geom_football(
  league = "nfl",
  display_range = "in_bounds_only",
  x_trans = 60,
  y_trans = 26.6667,
  xlims = c(30, 100),
  color_updates = field_params
)

a <- field_background +
  geom_point(data = christian,
             aes(120 - x, 160 / 3 - y),
             size = christian$size,
             color = christian$team_col) +
  labs(title = "<span style='color: #ceb53b;'> <br> New Orleans Saints</span> @ <span style='color: #007FC8;'>Carolina Panthers</span>, <br>week 3 of the 2022 NFL season",
       subtitle = "Q3: (7:16) C.McCaffrey left end pushed ob at NO 44 for 18 yards (P.Williams)") +
  theme(plot.subtitle = element_text(face = "italic",
                                     hjust = .5,
                                     size = 25,
                                     vjust = -1),
        plot.title = element_markdown(face = "bold",
                                  hjust = .5,
                                  size = 35,
                                  lineheight = 1.2)) +
  transition_time(christian$frameId)

# Animating the football play and saving it
a_gif <- animate(a, height = 600, width = 950, fps = 15)
a_gif
anim_save("AnimationA.gif", animation = a_gif)

just_christian <- tracking_bc |> 
  filter(playId == 2462,
         gameId == 2022092500) |> 
  pivot_longer(c(s_mph, dir_a_right_mpsh),
               names_to = "metric",
               values_to = "val") |> 
  mutate(metric = ifelse(metric == "s_mph", "Speed (mph)", "Directional Acceleration (mph/s)"))
  

b <- just_christian |> 
  ggplot(aes(x = frameId - 60, y = val, col = metric)) +
  geom_line(lwd = 2) +
  scale_color_manual("Metric", values = c("#007FC8", "#808080")) +
  labs(x = "Frame number since snap", 
       y = "Speed and acceleration",
       title = "<span style='color: #D50A0A;'>C. McCaffrey</span> speed and acceleration throughout play") +
  theme_minimal() +
  theme(plot.title = element_markdown(size = 33,
                                      face = "bold"),
        legend.title = element_markdown(size = 25,
                                    face = "bold"),
        legend.text = element_markdown(size = 25),
        axis.title = element_markdown(size = 25,
                                  face = "bold"),
        axis.text = element_markdown(size = 25)) +
  transition_reveal(frameId)

b_gif <- animate(b, height = 400, width = 950, fps = 15)
b_gif
anim_save("AnimationB.gif", animation = b_gif)

# Magick package
a_mgif <- image_read(path = "AnimationA.gif")
b_mgif <- image_read(path = "AnimationB.gif")

ab_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  ab_gif <- c(ab_gif, combined)
}

# Final gif
ab_gif <-image_write(ab_gif, path = "final_presentation/images/ab.gif")
image_read(ab_gif, path = "final_presentation/images/ab.gif")

# gt table for Saquon -----------------------------------------------------
# install.packages("gt")
library(gt)
# install.packages("gtExtras")
library(gtExtras)

tracking_bc |> 
  filter(playId == 1948,
         gameId == 2022091108) |> 
  select(gameId, playId, nflId, displayName, frameId, event, x, y, s, a, dir, dis, ke, work) |> 
  slice_head(n = 5) |> 
  gt() |> 
  tab_header(title = md("**Saquon Barkley tracking data**")) |> 
  cols_label(gameId = "Game ID", 
             playId = "Play ID",
             nflId = "NFL ID",
             displayName = "Name",
             frameId = "Fame Number",
             event = "Event",
             x = "x position",
             y = "y position",
             s = "Speed",
             a = "Acceleration",
             dir = "Direction",
             dis = "Distance",
             ke = "KE",
             work = "Work") |> 
  gt_theme_espn() |> 
  gt_highlight_rows(rows = seq(1, 5, 2), fill = "#a5acaf", font_weight = NULL)
