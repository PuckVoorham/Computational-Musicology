library(tidyverse)
library(plotly)
library(spotifyr)
library(compmus)
library(ggplot2)

elephant_ceptrogram <-
  get_tidy_audio_analysis("6qZjm61s6u8Ead9sWxCDro") |>  # Change URI.
  compmus_align(beats, segments) |>                     # Change `bars`
  select(beats) |>                                      #   in all three
  unnest(beats) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_gather_timbre()

elephant_ceptrogram |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +
  theme_classic()

saveRDS(object = elephant_ceptrogram, file = "data/elephant_ceptrogram (Ceptrogram).RDS")
