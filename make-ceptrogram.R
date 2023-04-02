library(tidyverse)
library(plotly)
library(spotifyr)
library(compmus)
library(ggplot2)

the_sun_ceptrogram <-
  get_tidy_audio_analysis("6wcBZjhU4dk813FYE1hybg") |>  # Change URI.
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

the_sun_ceptrogram |>
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

saveRDS(object = the_sun_ceptrogram, file = "data/the_sun_ceptrogram (Ceptrogram).RDS")


disciples_ceptrogram <-
  get_tidy_audio_analysis("2gNfxysfBRfl9Lvi9T3v6R") |>  # Change URI.
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

disciples_ceptrogram |>
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

saveRDS(object = disciples_ceptrogram, file = "data/disciples_ceptrogram (Ceptrogram).RDS")

