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

rb_stats_total |>
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


EPA <- rb_stats_total |>
  ungroup() |>
  arrange(desc(mean_ke)) |>
  slice_head(n = 5) 



# Avg expected points added for the top 5 players with the most avg_yards_gained


rb_stats_total |>
  ungroup() |>
  arrange(desc(avg_yards_gained)) |>
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
                with Average yards gained ",
    x = "Player Name",
    y = "Average Expected Points Added"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

yards <- rb_stats_total |>
  ungroup() |>
  arrange(desc(avg_yards_gained)) |>
  slice_head(n = 5) 

