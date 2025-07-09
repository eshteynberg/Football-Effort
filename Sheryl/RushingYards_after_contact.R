library(tidyverse)



joined_ds <- tracking_bc_starters_only |>
  left_join(
    tracking_bc_combined |>
      group_by(gameId, playId) |>
      summarise(first_contact_occurred = any(event == "first contact")) |>
      ungroup(),
    by = c("gameId", "playId")
  )

# MLR predicting rushing yards with KE and jerk

model1 <- lm(dis_gained_x_ac ~ mean_ke + mean_jerk + eff_move_prop + 
             avg_COD + acc_change, data = tracking_bc_play_stats)
summary(model1)


model2 <- lm(rushingYards ~ mean_ke + mean_jerk + eff_move_prop + 
               avg_COD + acc_change, data = tracking_bc_play_stats)
summary(model2)

# checking residuals, normality and other conditions/assumptions
par(mfrow = c(2, 2))
plot(model1)





