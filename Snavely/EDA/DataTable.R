library(gt)
library(gtExtras)

data_table <- tracking_bc |> 
  filter(displayName == "Saquon Barkley") |> 
  select(gameId, playId, bc_id, displayName, frameId, x, y, s_mph, dir_a_mpsh)
