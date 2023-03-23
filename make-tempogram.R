library(tidyverse)
library(plotly)
library(spotifyr)
library(compmus)
library(ggplot2)

elephant <- get_tidy_audio_analysis("6qZjm61s6u8Ead9sWxCDro")

elephant_normal <- elephant |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

elephant_cyclic <- elephant |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(object = elephant_normal, file = "data/elephant_normal (Tempogram).RDS")
saveRDS(object = elephant_cyclic, file = "data/elephant_cyclic (Tempogram).RDS")


borderline <- get_tidy_audio_analysis("5hM5arv9KDbCHS0k9uqwjr")

borderline_normal <- borderline |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

borderline_cyclic <- borderline |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(object = borderline_normal, file = "data/borderline_normal (Tempogram).RDS")
saveRDS(object = borderline_cyclic, file = "data/borderline_cyclic (Tempogram).RDS")

elephant_normal |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

elephant_cyclic |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
