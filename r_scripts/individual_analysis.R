##Individual Analysis

#Load Libraries

library(spotifyr)
library(plyr)
library(tidyverse)
library(ggthemes)
library(ggridges)
library(knitr)
library(kableExtra)
library(lubridate)
library(jsonlite)
library(gghighlight)
library(ggplot2)
library(plotly)

Sys.setenv(SPOTIFY_CLIENT_ID = "052dec7a99bd42e888548b0f32f5d444")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "3e61d71877f2486fa2a17c703eb452a6")
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

#Callback: http://localhost:1410/

get_spotify_authorization_code()


# GET SPECIFIC PLAYLIST FEATURES
playlist_username <- 'cosmoduende'
playlist_uris <- c('0PV31w7ireeI6d0oSYJ2H0')
playlistFavsEnglish <- get_playlist_audio_features(playlist_username, playlist_uris)

# PLOT LESS POPULARITY TRACKS ON SPECIFIC PLAYLIST
playlistFavsEnglish %>% 
  group_by(track.popularity) %>% 
  filter(track.popularity <= "35") %>%
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x= "Track name", y= "Popularity") + 
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))


# GET FAVORITE TRACKS

myFavTracks <- ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% 
  reduce(rbind) %>%
  write_rds('raw_myFavTracks.rds')


# GET TOP ARTISTS BASED ON LIKED TRACKS
favTracksArtist <- myFavTracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name)
trackNumArtist <- favTracksArtist %>%
  count(id, sort = TRUE) %>%
  left_join(favTracksArtist, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(10, n)

# PLOT TOP 10 ARTISTS BASED ON LIKED TRACKS
plotMyFavs <- trackNumArtist %>%
  mutate(freq = case_when(n > 100 ~ '> 100 tracks',
                          between(n, 50, 99) ~ '50-99 tracks',
                          between(n, 20, 49) ~ '20-49 tracks',
                          TRUE ~ '< 20 tracks')) %>%
  mutate(freq = factor(freq, levels = c('> 100 tracks', '50-99 tracks', '20-49 tracks', '< 20 tracks'))) %>%
  ggplot(mapping = aes(x = reorder(name, -n), y = n, fill = freq)) +
  geom_col() +
  scale_fill_brewer(palette="Dark2") +
  labs(x= "Artist name", y= "Number of tracks", fill = NULL) +
  ggtitle("What are my Top 10 favorite artists?", "Based on my ♥ tracks") +
  theme(axis.text.x = element_text(angle = 90))
plotMyFavs

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()



get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at)


get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup 


get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name)


playlist_username <- 'b7iflt2s6um28zx952r2rp1q9'
playlist_uris <- c('37i9dQZF1EpNiqXss6LbGe')
playlisttop100_2020 <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_on_repeat <- get_playlist_audio_features(playlist_username, playlist_uris)
playlist_repeat_rewind <- get_playlist_audio_features(playlist_username, playlist_uris)


#Least Popular Tracks

playlisttop100_2020 %>% 
  group_by(track.popularity) %>% 
  filter(track.popularity <= "35") %>%
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x= "Track name", y= "Popularity") + 
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))


#On Repeat


playlist_on_repeat %>% 
  group_by(track.popularity) %>% 
  filter(track.popularity <= "35") %>%
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x= "Track name", y= "Popularity") + 
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))


#Repeat rewind

playlist_repeat_rewind %>% 
  group_by(track.popularity) %>% 
  filter(track.popularity <= "35") %>%
  ggplot(aes(x = track.name, y = track.popularity)) + 
  geom_col(aes(fill = track.album.name)) +
  labs(x= "Track name", y= "Popularity") + 
  ggtitle("What are the least popular songs I listen to on Spotify?", "Popularity ranking < 35 in a specific playlist") +
  theme(axis.text.x = element_text(angle = 90))


# GET FEATURES TOP FOUR FAVORITE ARTISTS
favArtist1 <- get_artist_audio_features(artist= "Brian Fallon")
favArtist2 <- get_artist_audio_features(artist= "The Gaslight Anthem")
favArtist3 <- get_artist_audio_features(artist= "Modest Mouse")
favArtist4 <- get_artist_audio_features(artist= "The Wonder Years")
favArtist5 <- get_artist_audio_features(artist= "Bruce Springsteen")
favArtist6 <- get_artist_audio_features(artist= "The Weakerthans")
favArtist7 <- get_artist_audio_features(artist= "The Smashing Pumpkins")
favArtist8 <- get_artist_audio_features(artist= "Dire Straits")
favArtist9 <- get_artist_audio_features(artist= "Katie Pruitt")
favArtist10 <- get_artist_audio_features(artist= "The Afghan Whigs")
favArtist11 <- get_artist_audio_features(artist= "Frank Turner")
favArtist12 <- get_artist_audio_features(artist= "Aaron West and The Roaring Twenties")

# MAKE A SINGLE DATA FRAME
topFourArtists <- rbind(favArtist1, favArtist2, favArtist3, favArtist4)


# PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS
emotionalQuadrant <- ggplot(data = topFourArtists, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry / Turbulent") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful / Happy") +
  annotate('text', 1.75 / 2, 0.05, label = "Peace / Chill") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing / Sad") +
  labs(x= "Valence", y= "Energy") +
  ggtitle("Emotional quadrant Top four artists", "Based on energy y valence")  

















































