library(tidyverse)
library(plotly)
library(spotifyr)
library(compmus)
library(ggplot2)

Before_Currents <-
  get_playlist_audio_features(
    "", "0zj7vBkFkrtkIy7bbiIDHT"
  ) |>
  slice(1:30) |>
  add_audio_analysis()

After_Currents <-
  get_playlist_audio_features(
    "", "2iF5St3YBr1AalBJFZK8uD"
  ) |>
  slice(1:30) |>
  add_audio_analysis()

track_level_summary <-
  Before_Currents |>
  mutate(genre = "Before currents") |>
  bind_rows(After_Currents |> mutate(genre = "After currents"))

track_level_summary |>
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) |>
  select(genre, timbre) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = basis, y = value, fill = genre)) +
  geom_violin() +
  scale_fill_viridis_d() +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Genre")


saveRDS(object = track_level_summary, file = "data/track_level_summary (track_level_summary).RDS")

