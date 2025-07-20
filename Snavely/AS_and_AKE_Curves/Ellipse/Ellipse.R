library(tidyverse)

# Player case to draw ellipse -----------------------------------
# Choosing name
player_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# Defining intercepts of the ellipse
a <- mean(player_runs$s_mph) + sd(player_runs$s_mph)
b <- mean(player_runs$a_mpsh) + sd(player_runs$a_mpsh)

# Generating x-vals and y-vals to graph
x_vals <- seq(0, a, length.out = 300)
y_vals <- b * sqrt(1 - (x_vals^2 / a^2))

# Building the ellipse df
ellipse_df <- data.frame(x = x_vals, y = y_vals)

# Plotting the ellipse on the AS curve
player_runs |> 
  ggplot(aes(x = s_mph, y = a_mpsh)) +
  geom_point(aes(x = s_mph, y = a_mpsh), alpha=.3, color="grey2")+
  geom_path(data = ellipse_df, aes(x = x, y = y), color = "#0072B2", linewidth = 1.2) +
  labs(x = "Speed (mph)",
       y = "Acceleration (mph/s)",
       title = "Saquon Barkley",
       caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.text=element_text(size=15),
        plot.caption = element_text(face = "italic", size = 8))

# Calculating the minimum distance away from the point and the line
min_dist <- function(s_mph, a_mpsh, ex, ey) {
  dist <- sqrt((s_mph - ex)^2 + (a_mpsh - ey)^2)
  return(min(dist))
}

# Finding the minimum distance from AS point to ellipse line
player_runs_dist <- player_runs |> 
  mutate(
    min_dist_to_ellipse = purrr::map2_dbl(
      s_mph, a_mpsh, ~ min_dist(.x, .y, ellipse_df$x, ellipse_df$y)
      )
  )
 

# Creating an ellipse function --------------------------------------------

ellipse <- function(name, graph = FALSE) {
  # Choosing name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # Defining intercepts of the ellipse
  a <- mean(player_runs$s_mph) 
  b <- mean(player_runs$a_mpsh) 
  
  # Generating x-vals and y-vals to graph and find distance
  x_vals <- seq(0, a, length.out = 1000)
  y_vals <- b * sqrt(1 - (x_vals^2 / a^2))
  
  # Building the ellipse df
  ellipse_df <- data.frame(x = x_vals, y = y_vals)
  
  min_dist <- function(s_mph, a_mpsh, ex, ey) {
    dist <- sqrt((s_mph - ex)^2 + (a_mpsh - ey)^2)
    return(min(dist))
  }
  
  player_runs_dist <- player_runs |> 
  mutate(
    min_dist_to_ellipse = purrr::map2_dbl(
      s_mph, a_mpsh, ~ min_dist(.x, .y, ellipse_df$x, ellipse_df$y)
      )
  )
  
  if (graph == TRUE) {
    graph <- player_runs |> 
      ggplot(aes(x = s_mph, y = a_mpsh)) +
      geom_point(alpha=.3, color="grey2")+
      geom_path(data = ellipse_df, aes(x = x, y = y), color = "#0072B2", size = 1.2) +
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme_minimal(base_size=16) +
      theme(plot.title = element_text(face = "bold.italic",
                                      size = 18, 
                                      hjust = .5),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.text=element_text(size=15),
            plot.caption = element_text(face = "italic", size = 8))
    return(graph)
  }
  
  out <- tibble(
    displayName = name,
    bc_id = player_runs$bc_id,
    gameId = player_runs$gameId,
    playId = player_runs$playId,
    frameId = player_runs$frameId,
    s_mph = player_runs$s_mph,
    a_mpsh = player_runs$a_mpsh,
    min_dist_ellipse = player_runs_dist$min_dist_to_ellipse,
    above = ifelse((player_runs$s_mph^2/(mean(player_runs$s_mph))^2) + (player_runs$a_mpsh^2/(mean(player_runs$a_mpsh))^2) >1, TRUE, FALSE)
    
  )
  return(out)
}

# Test
ellipse("Saquon Barkley")
ellipse("Aaron Jones", graph = TRUE)


# Calculating ellipse score -----------------------------------------------

ellipse_scores <- purrr::map(rbs_names, ellipse) |> 
  bind_rows() 

ellipse_above <- ellipse_scores |> 
  group_by(displayName) |> 
  summarize(ellipse_score = sum(min_dist_ellipse[above==TRUE])/n()) |> 
  ungroup()

ellipse_prop <- ellipse_scores |> 
  group_by(displayName) |> 
  summarize(prop_out = mean(above),
            prop_in = 1-prop_out)

ellipse_stats <- ellipse_above |> 
  left_join(ellipse_prop)

ellipse_stats |> 
  ggplot(aes(x=ellipse_score, y=prop_out))+
  geom_point()

cor(ellipse_stats$ellipse_score, ellipse_stats$prop_out)  

# Ellipse by play
ellipse_play <- ellipse_scores |> 
  group_by(displayName, gameId, playId) |> 
  summarize(ellipse_score = sum(min_dist_ellipse[above==TRUE])/n()) |> 
  ungroup()








# ELLIPSE with dir_a ------------------------------------------------------



#DIR_A 
# Player case to draw ellipse -----------------------------------
# Choosing name
player_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

player_runs_pos_a <- player_runs |> 
  filter(dir_a_mpsh>=0)

player_runs_neg_a <- player_runs |> 
  filter(dir_a_mpsh<0)

# Defining intercepts of the ellipse
a <- quantile(player_runs_pos_a$s_mph, probs=.99)
pos_b <- quantile(player_runs_pos_a$dir_a_mpsh, probs=.99)
neg_b <- quantile(player_runs_neg_a$dir_a_mpsh, probs=.01)


# Generating x-vals and y-vals to graph
x_vals <- seq(0, a, length.out = 300)
pos_y_vals <- pos_b * sqrt(1 - (x_vals^2 / a^2))
neg_y_vals <- neg_b * sqrt(1 - (x_vals^2 / a^2))

# Building the ellipse df
ellipse_df <- data.frame(x = x_vals, pos_y = pos_y_vals, neg_y = neg_y_vals)

# Plotting the ellipse on the AS curve
player_runs |> 
  ggplot(aes(x = s_mph, y = dir_a_mpsh)) +
  geom_point(aes(x = s_mph, y = dir_a_mpsh), alpha=.3, color="grey2")+
  geom_path(data = ellipse_df, aes(x = x, y = pos_y), color = "#0072B2", linewidth = 1.2) +
  geom_path(data = ellipse_df, aes(x = x, y = neg_y), color = "#0072B2", linewidth = 1.2) +
  labs(x = "Speed (mph)",
       y = "Acceleration (mph/s)",
       title = "Saquon Barkley",
       caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.text=element_text(size=15),
        plot.caption = element_text(face = "italic", size = 8))

# Calculating the minimum distance away from the point and the line
min_dist <- function(s_mph, dir_a_mpsh, ex, ey) {
  dist <- sqrt((s_mph - ex)^2 + (dir_a_mpsh - ey)^2)
  return(min(dist))
}

# Finding the minimum distance from AS point to ellipse line
player_runs_dist_pos <- player_runs_pos_a |> 
  mutate(min_dist_to_ellipse= purrr::map2_dbl(s_mph, dir_a_mpsh, ~ min_dist(.x, .y, ellipse_df$x, ellipse_df$pos_y)))

player_runs_dist_neg <- player_runs_neg_a |> 
  mutate(min_dist_to_ellipse= purrr::map2_dbl(s_mph, dir_a_mpsh, ~ min_dist(.x, .y, ellipse_df$x, ellipse_df$neg_y)))


player_runs_dist <- rbind(player_runs_dist_pos, player_runs_dist_neg)



# creating an ellipse function --------------------------------------------

ellipse <- function(name, graph = FALSE) {
  # Choosing name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  player_runs_pos_a <- player_runs |> 
    filter(dir_a_mpsh>=0)
  
  player_runs_neg_a <- player_runs |> 
    filter(dir_a_mpsh<0)
  
  # Defining intercepts of the ellipse
  a <- quantile(player_runs_pos_a$s_mph, probs=.99)
  pos_b <- quantile(player_runs_pos_a$dir_a_mpsh, probs=.99)
  neg_b <- quantile(player_runs_neg_a$dir_a_mpsh, probs=.01)
  
  
  
  # Generating x-vals and y-vals to graph
  x_vals <- seq(0, a, length.out = 300)
  pos_y_vals <- pos_b * sqrt(1 - (x_vals^2 / a^2))
  neg_y_vals <- neg_b * sqrt(1 - (x_vals^2 / a^2))
  
  # Building the ellipse df
  ellipse_df <- data.frame(x = x_vals, pos_y = pos_y_vals, neg_y = neg_y_vals)
  
  # Plotting the ellipse on the AS curve
  if (graph == TRUE) {
    graph <- player_runs |> 
    ggplot(aes(x = s_mph, y = dir_a_mpsh)) +
    geom_point(aes(x = s_mph, y = dir_a_mpsh), alpha=.3, color="grey2")+
    geom_path(data = ellipse_df, aes(x = x, y = pos_y), color = "#0072B2", linewidth = 1.2) +
    geom_path(data = ellipse_df, aes(x = x, y = neg_y), color = "#0072B2", linewidth = 1.2) +
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme_minimal(base_size=16) +
      theme(plot.title = element_text(face = "bold.italic",
                                      size = 18, 
                                      hjust = .5),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.text=element_text(size=15),
            plot.caption = element_text(face = "italic", size = 8))
    return(graph)
  }
  
  # Calculating the minimum distance away from the point and the line
  min_dist <- function(s_mph, dir_a_mpsh, ex, ey) {
    dist <- sqrt((s_mph - ex)^2 + (dir_a_mpsh - ey)^2)
    return(min(dist))
  }
  
  # Finding the minimum distance from AS point to ellipse line
  player_runs_dist_pos <- player_runs_pos_a |> 
    mutate(min_dist_to_ellipse= purrr::map2_dbl(s_mph, dir_a_mpsh, ~ min_dist(.x, .y, ellipse_df$x, ellipse_df$pos_y)))
  
  player_runs_dist_neg <- player_runs_neg_a |> 
    mutate(min_dist_to_ellipse= purrr::map2_dbl(s_mph, dir_a_mpsh, ~ min_dist(.x, .y, ellipse_df$x, ellipse_df$neg_y)))
  
  player_runs_dist <- rbind(player_runs_dist_pos, player_runs_dist_neg)
  
  
  out <- tibble(
    displayName = name,
    bc_id = player_runs$bc_id,
    gameId = player_runs$gameId,
    playId = player_runs$playId,
    frameId = player_runs$frameId,
    s_mph = player_runs$s_mph,
    dir_a_mpsh = player_runs$dir_a_mpsh,
    min_dist_ellipse = player_runs_dist$min_dist_to_ellipse,
    above = ifelse(dir_a_mpsh >= 0,
      ifelse(
        (player_runs$s_mph^2/(quantile(player_runs$s_mph, probs=.99))^2) + (player_runs$dir_a_mpsh^2/(quantile(player_runs$dir_a_mpsh, probs=.99))^2) >1, TRUE, FALSE
        ),
      ifelse(
        (player_runs$s_mph^2/(quantile(player_runs$s_mph, probs=.99))^2) + (player_runs$dir_a_mpsh^2/(quantile(player_runs$dir_a_mpsh, probs=.01))^2) >1, TRUE, FALSE
        )
      )
    )
  return(out)
}

# Test
ellipse("Saquon Barkley", graph=TRUE)
ellipse("Aaron Jones", graph = TRUE)
  


# Calculating ellipse score -----------------------------------------------

ellipse_scores <- purrr::map(rbs_names, ellipse) |> 
  bind_rows() 

ellipse_scores <- ellipse_scores |> 
  mutate(adj_min_dist_ellipse = ifelse(above, 0, min_dist_ellipse),
         ellipse_stat = 1/(1+adj_min_dist_ellipse))

ellipse_stats <- ellipse_scores |> 
  group_by(displayName) |> 
  summarize(final_ellipse_score = mean(ellipse_stat),
            prop_out = mean(above),
            prop_in = 1-prop_out) |> 
  ungroup()

ellipse_stats |> 
  ggplot(aes(x=final_ellipse_score, y=prop_out))+
  geom_point()

cor(ellipse_stats$final_ellipse_score, ellipse_stats$prop_out)  

ellipse_stats_play <- ellipse_scores |> 
  group_by(gameId, playId) |> 
  summarize(final_ellipse_score = mean(ellipse_stat),
            prop_out = mean(above),
            prop_in = 1-prop_out) |> 
  ungroup()

