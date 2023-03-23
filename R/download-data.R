Before_Currents <- get_playlist_audio_features("", "0zj7vBkFkrtkIy7bbiIDHT")
Currents_and_later <- get_playlist_audio_features("", "2iF5St3YBr1AalBJFZK8uD")

saveRDS(object = Before_Currents, file = "data/before_currents-data.RDS")
saveRDS(object = After_Currents, file = "data/after_currents-data.RDS")

elephant <- get_tidy_audio_analysis("6qZjm61s6u8Ead9sWxCDro")
saveRDS(object = elephant, file = "data/elephant-data.RDS")
