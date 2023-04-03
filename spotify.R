devtools::install_github('charlie86/spotifyr')


library(spotifyr)
library(tidyverse)
library(lubridate)
library(knitr)
library(httr)

Sys.setenv(SPOTIFY_CLIENT_ID = '8f2b801c27da48029339dfa980720040')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '71f6fd94100b40ef945d689c4e7e0d99')

access_token <- get_spotify_access_token()

print(access_token)
#untuk mendapatkan artis audio
jay_ham <- get_artist_audio_features('jay ham')

#mendapatkan lagu yang baru diputar

last_played <-get_my_recently_played(limit = 50) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at)

#top artist
get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 50) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

