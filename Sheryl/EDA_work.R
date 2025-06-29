library(tidyverse)


## players with the highest KE

rb_stats_total |>
  ungroup() |>                     
  arrange(desc(mean_ke)) |>
  slice_head(n = 5) |>
  ggplot(aes(x = reorder(displayName, mean_ke), y = mean_ke)) +
  geom_col(fill = "navyblue") +
  # making the background of the labels white so they're visible when overlapping with the bars
  geom_label(aes(
    label = round(mean_ke, 1)),
    hjust = 0.5,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Top 5 Players with Highest Average Kinetic Energy",
    x = "Player Name",
    y = "Average Kinetic Energy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# Avg expected points added for the top 5 players with the most kinetic energy

rb_stats_total_filtered |>
  ungroup() |>
  arrange(desc(mean_ke)) |>
  slice_head(n = 5) |>
  ggplot(aes(x = reorder(displayName, avg_EPA), y = avg_EPA)) +
  geom_col(fill = "darkred") +
  geom_label(
    aes(label = round(avg_EPA, 3)),
    hjust = 0.5,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Average Expected Points Added for Top 5 Players
                with Highest Kinetic Energy",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# Avg expected points added for the top 5 players with the least kinetic energy

rb_stats_total_filtered |>
  ungroup() |>
  arrange(mean_ke) |>
  slice_head(n = 5) |>
  ggplot(aes(x = reorder(displayName, avg_EPA), y = avg_EPA)) +
  geom_col(fill = "darkred") +
  geom_label(
    aes(label = round(avg_EPA, 3)),
    hjust = 0.5,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Average Expected Points Added for Top 5 Players
                with Lowest Kinetic Energy",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# ds of players with the highest KE
KE_high <- rb_stats_total_filtered |>
  ungroup() |>
  arrange(desc(mean_ke)) |>
  slice_head(n = 5) 

# ds of players with the lowest KE
KE_low <- rb_stats_total_filtered |>
  ungroup() |>
  arrange(mean_ke) |>
  slice_head(n = 5) 


# joining both data sets

KE_combined <- bind_rows(KE_high, KE_low)

# plot with combined datasets

KE_combined |>
  ungroup() |>
  arrange(desc(mean_ke)) |>
  ggplot(aes(x = reorder(displayName, avg_EPA), y = avg_EPA)) +
  geom_col(fill = "darkred") +
  geom_label(
    aes(label = round(avg_EPA, 2)),
    hjust = 0.4,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Average Expected Points Added by Kinetic Energy",
    subtitle = "Average expected points for the five players with the highest
kinetic energy and the five players with the lowest kinetic energy",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


#=========================[ expected points added for ] ====================== 
#================[ avg_yards_gained ]===========================================


# Avg expected points added for the top 5 players with the most avg_yards_gained
rb_stats_total_filtered |>
  ungroup() |>
  arrange(desc(avg_yards_gained)) |>
  slice_head(n = 5) |>
  ggplot(aes(x = reorder(displayName, avg_EPA), y = avg_EPA)) +
  geom_col(fill = "navyblue") +
  geom_label(
    aes(label = round(avg_EPA, 3)),
    hjust = 0.5,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Average Expected Points Added for Top 5 Players
                with the most Average yards gained ",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# Avg expected points added for the top 5 players with the least avg_yards_gained

rb_stats_total_filtered |>
  ungroup() |>
  arrange(avg_yards_gained) |>
  slice_head(n = 5) |>
  ggplot(aes(x = reorder(displayName, avg_EPA), y = avg_EPA)) +
  geom_col(fill = "navyblue") +
  geom_label(
    aes(label = round(avg_EPA, 3)),
    hjust = 0.5,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Average Expected Points Added for Top 5 Players
                with the least average yards gained ",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# joining datasets

low_yards <- rb_stats_total_filtered |>
  ungroup() |>
  arrange(desc(avg_yards_gained)) |>
  slice_head(n = 5) 

high_yeards <- rb_stats_total_filtered |>
ungroup() |>
  arrange(avg_yards_gained) |>
  slice_head(n = 5)


yards_combined <- bind_rows(low_yards, high_yeards)

# plotting combined ds 

yards_combined |>
  ungroup() |>
  arrange(desc(avg_yards_gained)) |>
  ggplot(aes(x = reorder(displayName, avg_EPA), y = avg_EPA)) +
  geom_col(fill = "navyblue") +
  geom_label(
    aes(label = round(avg_EPA, 2)),
    hjust = 0.4,
    size = 4,
    fill = "white"
  ) +
  coord_flip() +
  labs(
    title = "Average Expected Points Added by Average Yards Gained",
    subtitle = "Average expected points for the five players with the highest
Average Yards Gained and the five players with the lowest Average Yards Gained",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )






