# Test
library(tidyverse)
tracking <- read_csv("data/tracking_week_1.csv") |> 
  bind_rows(read_csv("data/tracking_week_2.csv")) |> 
  bind_rows(read_csv("data/tracking_week_3.csv"))


# library(tidyverse)
tracking <- read_csv("data/tracking_week_1.csv") |>
  bind_rows(read_csv("data/tracking_week_2.csv")) |>
  bind_rows(read_csv("data/tracking_week_3.csv")) |>
  bind_rows(read_csv("data/tracking_week_4.csv")) |>
  bind_rows(read_csv("data/tracking_week_5.csv")) |>
  bind_rows(read_csv("data/tracking_week_6.csv")) |>
  bind_rows(read_csv("data/tracking_week_7.csv")) |>
  bind_rows(read_csv("data/tracking_week_8.csv")) |>
  bind_rows(read_csv("data/tracking_week_9.csv"))



# write the combined tracking data object into a parquet file
# install.packages("arrow")
library(arrow)
arrow::write_parquet(tracking, "data/tracking.parquet")

