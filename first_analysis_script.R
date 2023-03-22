library(tidyverse)
library(spotifyr)

#year_2008 <- get_playlist_audio_features("", "7HlA8Mo6Qmu0PnDTAy8KEx")
#year_2009 <- get_playlist_audio_features("", "4TZQdInyIBhEiVFD6th0pd")
#Innerspeaker <- get_playlist_audio_features("", "1AXmYpua1GLCYCabzoTNRU")
#Lonerism <- get_playlist_audio_features("", "6jMn2Al9SkT6Ego1EbRFJJ")

Before_Currents <- get_playlist_audio_features("", "0zj7vBkFkrtkIy7bbiIDHT")
After_Currents <- get_playlist_audio_features("", "2iF5St3YBr1AalBJFZK8uD")

#Currents <- get_playlist_audio_features("", "63F20lxwhrbbvkwyD2Qh2u")
#The_Slow_Rush <- get_playlist_audio_features("", "54uQpagM8ye2V7Q2WBwvaA")

Categorical <- rbind(Before_Currents, After_Currents)


# Again, you save the plot object into a variable so you can save typing later on
ilo_plot <- ggplot(Categorical, aes(x = valence, y = energy, size = tempo, color = playlist_name)) +
  geom_point() +
  labs(
    x = "Valence",
    y = "Energy"
  )

ilo_plot

Categorical |>
  ggplot(aes(x = playlist_name, y = danceability )) +
  geom_violin()

