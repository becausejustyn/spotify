library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)

streamHistory <- fromJSON("C:/Users/justy/Downloads/spotify-main/spotify-main/StreamingHistory0.json", flatten = TRUE)

streamHistory 


# Adding date, time and playcount

mySpotify <- streamHistory %>% 
  as_tibble() %>% 
  mutate_at("endTime", ymd_hm) %>% 
  mutate(endTime = endTime - hours(6)) %>% 
  mutate(date = floor_date(endTime, "day") %>% 
           as_date, seconds = msPlayed / 1000, minutes = seconds / 60) %>%
  add_count(trackName) %>%
  rename(playCount = n)

#Moved columns  
mySpotify <- mySpotify %>%
  relocate(endTime, .after = minutes) %>%
  relocate(playCount, .after = trackName) %>%
  relocate(trackName, .before = artistName)

#Remove all duplicates

mySpotify <- mySpotify %>%
  distinct(trackName, artistName, .keep_all = TRUE)
    
