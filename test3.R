
library(tidyverse)
library(plotly)
library(spotifyr)
library(compmus)


Categorical <-
  get_tidy_audio_analysis("5JWPUEov2wlX7c0jhYZpeB") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)


Categorical |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value,
      box = FALSE
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  theme(
    text = element_text( color = "black"),
    plot.subtitle = element_text(size = 12, color = "black"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  ) +
  scale_fill_viridis_c()

