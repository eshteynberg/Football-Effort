library(tidyverse)


rb_stats_dist <- tracking_bc |>
  filter(displayName %in% rb_stats_total_filtered$ displayName)


# Calculating total distance covered for each RB by play
glimpse(tracking_bc)

rb_stats_dist <- tracking_bc |> 
  arrange(gameId, playId, frameId) |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  mutate(dx = x - lag(x),
         dy = y - lag(y),
         dist = sqrt(dx^2 + dy^2)) |> 
  summarise(total_dist = sum(dist, na.rm = TRUE), .groups = "drop")



# plotting total distance distribution for each RB covered across all their plays

# rb_stats_dist |> 
#   filter(displayName == "D'Andre Swift") |>
#   ggplot(aes(x = total_dist)) +
#   geom_histogram(bins = 13, fill = "steelblue", color = "navyblue") +
# #  facet_wrap(~ displayName) +
#   labs(
#     title = "Distribution of Total Distance Covered per Play by Each D'Andre Swift",
#     x = "Total Distance (yards)",
#     y = "Number of Plays"
#   ) +
#   theme_minimal()


# Faceting only the top 9 running backs with the highest aggregated total distance covered

top_rb <- rb_stats_dist |>
  group_by(displayName) |>
  summarise(total_dist = sum(total_dist, na.rm = TRUE)) |>
  arrange(desc(total_dist)) |>
  slice_head(n = 9) 

rb_stats_dist |>
  filter(displayName %in% top_rb$displayName) |>
  ggplot(aes(x = total_dist)) +
  geom_histogram(bins = 13, fill = "steelblue", color = "navyblue") +
  facet_wrap(~ displayName, scales = "free_y") + # use free scale to not avoid fixed y scale
  labs(
    title = "Total Distance Covered per Play for Top 9 RBs with Highest Distance Covered",
    x = "Distance (yards)",
    y = "Number of Plays"
  ) +
  theme_minimal()



# ============================================================================================
# Faceting only yhe bottom 9 running backs with the highest aggregated total distance covered
# NOTE: these RBs with lowest distance covered have very few plays 
# ============================================================================================

bottom_rb <- rb_stats_dist |>
  group_by(displayName) |>
  summarise(total_dist = sum(total_dist, na.rm = TRUE)) |>
  arrange(total_dist) |>
  slice_head(n = 9) 

rb_stats_dist |>
  filter(displayName %in% bottom_rb$displayName) |>
  ggplot(aes(x = total_dist)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "navyblue") +
  facet_wrap(~ displayName, scales = "free_y") + 
  labs(
    title = "Total Distance Covered per Play for Top 9 RBs with Lowest Distance Covered",
    x = "Distance (yards)",
    y = "Number of Plays"
  ) +
  theme_minimal()







