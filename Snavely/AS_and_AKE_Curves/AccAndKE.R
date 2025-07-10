library(tidyverse)

# KE and Acceleration curve -----------------------------------------------

acc_ke_function <- function(name, graph = FALSE, player_table = FALSE) {
  # Filtering the data set to only include the inputted player
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # Making the bins
  bins <- seq(3, max(player_runs$ke) + 200, 200)
  
  # Picking out the top two accelerations for each speed in each bins
  max_acc <- map_dfr(1:(length(bins) - 1), function(i) {
    player_runs |> 
      filter(ke > bins[i], ke <= bins[i + 1]) |> 
      slice_max(a, n = 2, with_ties = FALSE) |> 
      select(ke = ke, acceleration = a)
  })
  
  # Fitting the first regression line
  player_lm <- lm(acceleration ~ ke, data = max_acc)
  
  # Adding confidence intervals
  confs <- predict(player_lm, interval = "confidence")
  max_acc <- max_acc |> 
    bind_cols(confs)
  
  # Filtering out outliers (any point that does not fall within a 95% conf interval)
  max_acc_clean <- max_acc |> 
    filter(acceleration >= lwr, acceleration <= upr)
  
  # Fitting the new regression line (without outliers)
  player_lm_clean <- lm(acceleration ~ ke, data = max_acc_clean)
  
  # Finding out how many points are near the line
  test_player_a <- data.frame(ke = player_runs$ke, acceleration = player_runs$a)
  test_preds <- predict(player_lm_clean, newdata = test_player_a)
  
  # Final calculation for distance away from the fitted line
  player_final <- player_runs |> 
    select(a, ke) |> 
    mutate(pred = test_preds) |> 
    mutate(diff = pred - a) |> 
    mutate(eff = ifelse(diff <= .25, TRUE, FALSE),
           eff_50 = ifelse(diff <= .5, TRUE, FALSE),
           eff_75 = ifelse(diff <= .75, TRUE, FALSE)) |> 
    mutate(gameId = player_runs$gameId, 
           playId = player_runs$playId, 
           bc_id = player_runs$bc_id, 
           displayName = player_runs$displayName,
           frameId = player_runs$frameId)
  
  # Final effort metric
  eff <- tibble(
    eff_metric = sum(player_final$eff == TRUE),
    eff_metric_perc = sum(player_final$eff == TRUE) / nrow(player_final) * 100,
    eff_metric_50 = sum(player_final$eff_50 == TRUE),
    eff_metric_perc_50 = sum(player_final$eff_50 == TRUE) / nrow(player_final) * 100,
    eff_metric_75 = sum(player_final$eff_75 == TRUE),
    eff_metric_perc_75 = sum(player_final$eff_75 == TRUE) / nrow(player_final) * 100,
  )
  
  # Building the graph if specified
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = ke, y = a)) +
      geom_smooth(method = lm, aes(x = ke, y = acceleration), data = max_acc_clean, lwd = 1.5, se = FALSE) +
      geom_abline(aes(color = "Regression line",
                      intercept = player_lm_clean$coefficients[1], slope = player_lm_clean$coefficients[2]),
                  lwd = 1.5,) +
      geom_abline(aes(color = "Minimum line (.25)",
                      intercept = player_lm_clean$coefficients[1] - .25, slope = player_lm_clean$coefficients[2]), 
                  lty = 2, lwd = 1.5) +
      geom_abline(aes(color = "Minimum line (.5)",
                      intercept = player_lm_clean$coefficients[1] - .5, slope = player_lm_clean$coefficients[2]),
                  lty = 2, lwd = 1.5) +
      geom_abline(aes(color = "Minimum line (.75)",
                      intercept = player_lm_clean$coefficients[1] - .75, slope = player_lm_clean$coefficients[2]), 
                  lty = 2, lwd = 1.5) +
      scale_color_manual("Line", values = c("#4B92DB", "darkgreen", "turquoise", "#FFB612")) +
      geom_point(size = 2, alpha = .5, col = "grey2") +
      labs(x = "Kinetic Energy",
           y = "Acceleration",
           title = paste0(name, "'s effort is defined as the percentage of points above the minimum line"),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme(plot.title = element_text(face = "bold",
                                      size = 20, 
                                      hjust = .5),
            legend.title = element_text(face = "bold",
                                        size = 15),
            axis.title = element_text(face = "bold",
                                      size = 15),
            axis.text = element_text(size = 13),
            plot.caption = element_text(face = "italic",
                                        size = 8))
    
    return(player_graph)
  }
  
  if (player_table == TRUE) {
    return(player_final)
  }
  
  return(eff)
}

# Testing to see if function works
acc_ke_function("Saquon Barkley", graph = TRUE)
acc_ke_function("Rex Burkhead", graph = TRUE)

# Combining player tables together
rbs <- unique(rb_stats_total_filtered$displayName)

acc_ke_movements <- purrr::map(rbs, acc_ke_function) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

acc_ke_function("Dameon Pierce", graph = TRUE)
