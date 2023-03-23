

library(tidyverse)
library(plotly)
library(spotifyr)
library(compmus)

before_currents <- get_playlist_audio_features("", "0zj7vBkFkrtkIy7bbiIDHT")
  ) |>
  slice(1:30) |>
  add_audio_analysis()
after_currents <-
  get_playlist_audio_features("",
    "2iF5St3YBr1AalBJFZK8uD"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
total <-
  bebop |>
  mutate(playlist = "Before_Currents") |>
  bind_rows(bigband |> mutate(genre = "Big Band"))

jazz |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )

jazz |>
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
