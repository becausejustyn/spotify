#Spotify Playlist Clustering Demo

library(spotifyr)
library(cluster)
library(fpc)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = "052dec7a99bd42e888548b0f32f5d444")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "3e61d71877f2486fa2a17c703eb452a6")
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

#might not need this function
get_playlist_tracks_custom <- function(playlist_id_1) {
  pp  <- get_playlist(playlist_id_1)
  number_of_songs <- pp$tracks$total
  s <- seq(0,number_of_songs, 100)
  temp_s <- list()
  ind <- 1
  for (i in s){
    temp_s[[ind]] <- get_playlist_tracks(playlist_id_1, offset = i)
    ind <- ind + 1
  }
  return(bind_rows(temp_s))
}

put_songs_from_playlists_into_df <- function(user_id, pl_names){
  my_plists <- as_tibble(get_user_playlists(user_id, limit = 50))
  my_plists.filtered <- my_plists %>% 
    filter(name %in% pl_names)
  tracks_from_chosen_playlists <-  as_tibble(map_df(my_plists.filtered$id,get_playlist_tracks_custom)) 
  tracks_from_chosen_playlists_filtered <- tracks_from_chosen_playlists %>% 
    group_by(track.uri) %>% 
    mutate(number = n()) %>% 
    dplyr::filter(number == 1)
  return(ungroup(tracks_from_chosen_playlists_filtered))
}


streamHistory_tibble <- as_tibble(streamHistory)

streamHistory_filtered <- streamHistory_tibble %>%
  group_by(track.uri) %>% 
  mutate(number = n()) %>% 
  dplyr::filter(number == 1)





all_my_fav_tracks <- ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% reduce(rbind) %>%
  write_rds('raw_all_my_fav_tracks.rds')

glimpse(all_my_fav_tracks)

library(lubridate)
library(knitr)
all_my_fav_tracks %>%
  mutate(added_at = ymd_hms(added_at)) %>%
  arrange(added_at) %>%
  head(1, wt = added_at) %>%
  select(track.name,added_at)  %>%
  kable()

artist_from_fav_tracks <-
  all_my_fav_tracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  # I don't think we need Urls in further analyses, id (unique mark of artist) and name are selected here.
  select(id, name)


track_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n)

track_num_artist  %>%
  kable()


# For numerical variables, sometimes for simplifying problems, cut them into fractions is a good idea. Here, we go further, we fill the column plot with different color to represent different frequency group.
track_num_artist %>%
  mutate(
    freq = case_when(
      n > 100 ~ 'More than 100 tracks',
      between(n, 50, 99) ~ '50~99 tracks',
      between(n, 20, 49) ~ '20~49 tracks',
      TRUE ~ 'Less than 20 tracks'
    )
  ) %>%
  # To avoid mess up the order of frequency group, I always suggest to convert the category variables as factor variables, with built-in order, levels.
  mutate(freq = factor(
    freq,
    levels = c(
      'More than 100 tracks',
      '50~99 tracks',
      '20~49 tracks',
      'Less than 20 tracks'
    )
  )) %>%
  ggplot(mapping = aes(
    x = reorder(name, -n),
    y = n,
    fill = freq
  )) +
  geom_col() +
  labs(fill = NULL,title = 'Who is My Favorite Artist',caption = 'data from spotify via spotiyr') +
  xlab('Artist') +
  ylab('Tracks Number') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -60),
        axis.title = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        plot.caption = element_text(hjust = 1,face = 'bold.italic'))


if(!file.exists('audio_features.rds')){
  track_num_artist$name %>%
    map(function(x){
      get_artist_audio_features(x)
    }) %>%
    reduce(rbind) %>%
    inner_join(all_my_fav_tracks,
               by = c('track_id' = 'track.id')) %>%
    write_rds('audio_features.rds')
}

audio_features <- read_rds('audio_features.rds')

ggplot(data = audio_features, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Turbulent/Angry", fontface =
             "bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Happy/Joyful", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Chill/Peaceful", fontface =
             "bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Sad/Depressing", fontface =
             "bold")



tracks_from_playlists <- put_songs_from_playlists_into_df(user_id = my_id, pl_name =  c( "analyse this", "Favourites twenty"))

analyse_this_features <- get_playlist_audio_features("my_username", "0nNHFHJuBGd9ykLX6JEMza")

analyse_this_features$tit

> analyse_this_features <- analyse_this_features %>%
  + relocate(track.name, .before = danceability)
