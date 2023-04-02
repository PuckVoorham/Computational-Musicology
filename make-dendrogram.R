library(tidyverse)
library(tidymodels)
library(plotly)
library(spotifyr)
library(ggdendro)
library(heatmaply)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |>
    collect_predictions() |>
    conf_mat(truth = outcome, estimate = .pred_class)
}

get_pr <- function(fit) {
  fit |>
    conf_mat_resampled() |>
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |>
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |>
    ungroup() |> filter(Prediction == Truth) |>
    select(class = Prediction, precision, recall)
}

before_currents <- get_playlist_audio_features("spotify", "0zj7vBkFkrtkIy7bbiIDHT")
currents_and_later <- get_playlist_audio_features("spotify", "2iF5St3YBr1AalBJFZK8uD")


tame_impala_complete <-
  bind_rows(
    before_currents |> mutate(playlist = "Before Currents") |> slice_head(n = 20),
    currents_and_later |> mutate(playlist = "Currents and later") |> slice_head(n = 20),
  ) |>
  add_audio_analysis()


tame_impala_features <-
  tame_impala_complete |>
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

tame_impala_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo,
      #duration +
      # + `C#|Db` + D + `D#|Eb` +
      #E + `F` + `F#|Gb` + G +
      #G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = tame_impala_complete
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  # step_range(all_predictors()) |>
  prep(tame_impala_complete |> mutate(track.name = str_trunc(track.name, 20))) |>
  juice() |>
  column_to_rownames("track.name")

tame_impala_dist <- dist(tame_impala_juice, method = "euclidean")

#tame_impala_dist |>
# hclust(method = "complete") |> # Try single, average, and complete.
# dendro_data() |>
# ggdendrogram()

data_for_tame_impala_clustering <- tame_impala_dist |>
  hclust(method = "average") |> # average for a balanced tree!
  dendro_data()

playlist_data_for_join <- tame_impala_complete %>%
  select(track.name, playlist_name) %>%
  mutate(label = str_trunc(track.name, 20))

data_for_tame_impala_clustering$labels <- data_for_tame_impala_clustering$labels %>%
  left_join(playlist_data_for_join)

# Add factor so can use colouring!
data_for_tame_impala_clustering$labels$label <- factor(data_for_tame_impala_clustering$labels$label)

saveRDS(object = data_for_tame_impala_clustering, file = "data/tame_impala_dendrogram (Dendrogram).RDS")

data_for_tame_impala_clustering |>
  ggdendrogram() +
  geom_text(data = label(data_for_tame_impala_clustering),aes(x, y,
                                                         label=label,
                                                         hjust=0,
                                                         colour=playlist_name), size=3) +
  coord_flip() +
  scale_y_reverse(expand=c(0.2, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank()) +
  labs(title = "Playlist Clustering") +
  scale_color_manual(values = c("#9900cc", "#009999")) +
  guides(
    colour = guide_legend(
      title = "Playlist"
    )
  )

# package protoclust

