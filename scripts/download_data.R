
# This script downloads publicly available data from the MLB Stats API
# and writes them to the input/data directory.

if (!dir.exists("input/data/event")) {
  dir.create("input/data/event", recurive = TRUE)
}

if (!dir.exists("input/data/game")) {
  dir.create("input/data/game", recurive = TRUE)
}

if (!dir.exists("input/data/event")) {
  dir.create("input/data/pitch", recurive = TRUE)
}

if (!dir.exists("input/data/event")) {
  dir.create("input/data/play", recurive = TRUE)
}


cluster <- parallel::makeCluster(parallel::detectCores())

data_2022 <- sabRmetrics::download_statsapi(
  start_date = "2022-01-01",
  end_date = "2022-12-31",
  cl = cluster
)
data.table::fwrite(data_2022$event, file = "input/data/event/2022.csv")
data.table::fwrite(data_2022$game, file = "input/data/game/2022.csv")
data.table::fwrite(data_2022$pitch, file = "input/data/pitch/2022.csv")
data.table::fwrite(data_2022$play, file = "input/data/play/2022.csv")

data_2023 <- sabRmetrics::download_statsapi(
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  cl = cluster
)
data.table::fwrite(data_2023$event, file = "input/data/event/2023.csv")
data.table::fwrite(data_2023$game, file = "input/data/game/2023.csv")
data.table::fwrite(data_2023$pitch, file = "input/data/pitch/2023.csv")
data.table::fwrite(data_2023$play, file = "input/data/play/2023.csv")

parallel::stopCluster(cluster)
