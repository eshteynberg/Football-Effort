
# Load necessary packages
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(tidytext)
library(quantreg)
library(qgam)


load("web_data/tracking_bc.Rdata")
load("web_data/player_runs_modeling.Rdata")


set.seed(1)

eff_function_rqss <- function(name, graph = FALSE) {
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  N_FOLDS <- 5
  player_runs_modeling <- player_runs |> 
    select(s, a) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_cv <- function(x) {
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$s)
    test_data <- test_data |> 
      filter(s >= max(s_range[1], 1),
             s <= min(s_range[2], 9))
    
    rq_fit <- rqss(a ~ qss(s, lambda = 3), tau = 0.9, data = train_data)
    
    out <- tibble(
      rq_pred = predict(rq_fit, newdata = test_data),
      test_actual = test_data$a,
      res_rq = test_actual - rq_pred,
      test_fold = x
    )
    return(out)
  }
  
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    list_rbind()
  
  eff_score <- mean(player_runs_test_preds$res_rq < 0)
  
  if (graph == TRUE) {
    # Fit final model and compute smooth prediction line
    rq_fit <- rqss(a ~ qss(s, lambda = 3), tau = 0.9, data = player_runs)
    s_seq <- seq(min(player_runs$s, na.rm = TRUE), max(player_runs$s, na.rm = TRUE), length.out = 100)
    pred_df <- tibble(s = s_seq)
    pred_df$a <- predict(rq_fit, newdata = pred_df)
    
    # Interactive ggplot
    p <- ggplot() +
      geom_point(data = player_runs, aes(
        x = s, 
        y = a,
        text = paste0("Speed: ", round(s, 2),
                      "<br>Acceleration: ", round(a, 2))), 
        alpha = 0.5, 
        color = "grey40") +
      geom_line(data = pred_df, aes(x = s, y = a), 
                color = "blue", size = 1.2) +
      scale_x_continuous(name = "Speed") +
      scale_y_continuous(name = "Acceleration") +
      labs(
        title = paste0(name, "'s Effort Quantile Curve (90%)"),
        caption = "Data from Weeks 1–9 of the 2022 NFL Season"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        plot.caption = element_text(face = "italic", size = 9)
      )
    
    return(ggplotly(p, tooltip = "text"))
  }
  
  return(tibble(displayName = name, effort_ratio_90 = 1 - eff_score))
}

#================ [Interactive drop down menu to select any running backs]================

g1 <- ggplotly(eff_function_rqss("Saquon Barkley", graph = TRUE))
g2 <- ggplotly(eff_function_rqss("Rex Burkhead", graph = TRUE))



# Step 2: Initialize plotly figure
fig <- plot_ly()

# Step 3: Add Saquon traces (visible)
for (trace in g1$x$data) {
  trace$visible <- TRUE
  fig <- fig %>% add_trace(
    x = trace$x,
    y = trace$y,
    type = trace$type,
    mode = trace$mode,
    line = trace$line,
    name = trace$name,
    text = trace$text,
    hoverinfo = trace$hoverinfo,
    visible = TRUE
  )
}

# Step 4: Add Rex traces (initially hidden)
for (trace in g2$x$data) {
  trace$visible <- FALSE
  fig <- fig %>% add_trace(
    x = trace$x,
    y = trace$y,
    type = trace$type,
    mode = trace$mode,
    line = trace$line,
    name = trace$name,
    text = trace$text,
    hoverinfo = trace$hoverinfo,
    visible = FALSE
  )
}

# Step 5: Construct correct visibility vectors
n1 <- length(g1$x$data)
n2 <- length(g2$x$data)

vis_saquon <- c(rep(TRUE, n1), rep(FALSE, n2))
vis_rex    <- c(rep(FALSE, n1), rep(TRUE, n2))

# Step 6: Add dropdown menu with correct visibility toggles
fig <- fig %>% layout(
  title = "Acceleration vs Speed Curve by Player",
  updatemenus = list(
    list(
      type = "dropdown",
      y = 0.8,
      x = 1.1,
      buttons = list(
        list(method = "restyle",
             args = list("visible", vis_saquon),
             label = "Saquon Barkley"),
        list(method = "restyle",
             args = list("visible", vis_rex),
             label = "Rex Burkhead")
      )
    )
  )
)

fig
