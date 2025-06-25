## players with the highest KE

rb_stats_total |>
  arrange(desc(mean_ke)) |>
  filter(mean_ke >= 8786) |>
  ggplot(aes(x = mean_ke, y = displayName)) +
  geom_col() +
  labs(
    x = "Average Kinetic Energy",
    y = "Player Name"
  )
