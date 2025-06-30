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
         dis_x = ifelse(gameId == lag(gameId) & playId == lag(playId), x - lag(x), NA))


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

# Visualizations ----------------------------------------------------------

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

# Animating the top KE play -----------------------------------------------
library(gganimate)
library(sportyR)
# install.packages("ggtext")
library(ggtext)
# install.packages("magick")  
library(magick)

# Filtering for the play with the highest avg KE
# Week 1: Giants @ Titans
play_1948 <- plays |> 
  filter(playId == 1948,
         gameId == 2022091108)

saquon <- tracking_rb_runs |> 
  filter(playId == 1948,
         gameId == 2022091108) |> 
  mutate(team_col = case_when(club == "TEN" ~ "#808080",
                              club == "NYG" & displayName != "Saquon Barkley" ~ "#0B2265",
                              club == "football" ~ "#964B00",
                              displayName == "Saquon Barkley" ~ "#CC5500"),
         size = case_when(club == "TEN" ~ 5,
                          club == "NYG" ~ 5,
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
?geom_football
field_background <- geom_football(
  league = "nfl",
  display_range = "in_bounds_only",
  x_trans = 60,
  y_trans = 26.6667,
  xlims = c(30, 112),
  color_updates = field_params
)

a <- field_background +
  geom_point(data = saquon,
             aes(120 - x, 160 / 3 - y),
             size = saquon$size,
             color = saquon$team_col) +
  labs(title = "<span style='color: #808080;'> <br> New York Giants</span> @ <span style='color: #0B2265;'>Tennessee Titans</span>, <br>week 1 of the 2022 NFL season",
       subtitle = "Q3: (12:45) S. Barkley left end pushed out of bounds at TEN 22 for 68 (K. Byard)") +
  theme(plot.subtitle = element_text(face = "italic",
                                     hjust = .5,
                                     size = 25,
                                     vjust = -1),
        plot.title = element_markdown(face = "bold",
                                  hjust = .5,
                                  size = 30,
                                  lineheight = 1.2)) +
  transition_time(saquon$frameId)

# Animating the football play and saving it
a_gif <- animate(a, height = 600, width = 950)
a_gif
anim_save("AnimationA.gif", animation = a_gif)

just_saquon <- tracking_bc |> 
  filter(playId == 1948,
         gameId == 2022091108) |> 
  pivot_longer(c(ke, work),
               names_to = "metric",
               values_to = "val") |> 
  mutate(metric = ifelse(metric == "ke", "KE", "Work"))
  

b <- just_saquon |> 
  ggplot(aes(x = frameId - 60, y = val, col = metric)) +
  geom_line(lwd = 2) +
  scale_color_manual("Metric", values = c("#0B2265", "#808080")) +
  labs(x = "Frame number since snap", 
       y = "Energy units",
       title = "<span style='color: #CC5500;'>S. Barkley</span> kinetic energy and work throughout play") +
  theme(plot.title = element_markdown(size = 28,
                                      face = "bold"),
        legend.title = element_text(size = 25,
                                    face = "bold"),
        legend.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 25)) +
  transition_reveal(frameId)

b_gif <- animate(b, height = 300, width = 950)
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
ab_gif <-image_read(path = "ab.gif")


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