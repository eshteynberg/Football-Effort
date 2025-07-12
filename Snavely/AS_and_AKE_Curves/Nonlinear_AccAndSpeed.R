library(quantreg)

# For nlrq, we need to estimate the shape of the model
tracking_bc |> 
  ggplot(aes(x = s_mph, y = a)) +
  geom_point(alpha = .3)

# Trying to match the general shape to give the nlrq function good predictions
plot(tracking_bc$s_mph, dgamma(tracking_bc$s_mph, shape = 2, scale = 4))
plot(tracking_bc$s_mph, dgamma(tracking_bc$s_mph, shape = 2, scale = 6))


nonlinear_eff_function <- function(name, graph = FALSE, table = FALSE) {
  # Filtering the data set to only include the inputted player
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  quantreg::
  N_FOLDS <- 5
  player_runs_modeling <- player_runs |> 
    select(s_mph, a) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    # Modeling
    # Quantile regression
    player_runs_rq <- rq(a ~ s_mph, tau = .9, data = train_data)
    ## Nonlinear quantile regression
    # Gamma estimate (using an estimate of the gamma function)
    player_runs_nlrq <- nlrq(a ~ param * s_mph^alpha * exp(-beta * s_mph), tau = .9, 
                             start = list(alpha = 2, beta = 3, param = 5), data = train_data,
                             control = nlrq.control(InitialStepSize = 0))

    # Predictions
    out <- tibble(
      rq_pred = predict.rq(player_runs_rq, newdata = test_data),
      test_actual = test_data$a,
      res_rq = test_actual-rq_pred,
      test_fold = x
    )
    return(out)
  }
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  return(player_runs_test_preds)
}