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

##Album/Artist Analysis

#DF of an Artist
MM <- get_artist_audio_features('Modest Mouse')

#Create Factors for categorical data
MM$album_name <- as.factor(MM$album_name)
MM$album_release_year <- as.factor(MM$album_release_year)

#Energy By Album
ggplot(MM, aes(x = energy,y=fct_rev(album_release_year))) + 
  geom_density_ridges(scale = 2, size = 0.1, rel_min_height = 0.03,quantile_lines=TRUE,quantiles=2) +
  theme_ridges()+
  scale_y_discrete(labels=levels(MM$album_name)) + 
  scale_x_continuous( expand = c(0, 0)) +
  labs(title= "Modest Mouse energy by album",subtitle = "Based on data from Spotify", y="Album", x = "Energy")

#A table representation of the same data:
MM %>% 
  group_by(album_name) %>% 
  summarise(avg = mean(energy),max = max(energy),min = min(energy)) %>% 
  arrange(-avg) %>% kable() %>% 
  kable_styling("striped", full_width = F, position = "left")%>% 
  row_spec(row = 1, color = "black",bold = TRUE)


## Songs with the most Energy
#Easily enough, we can also show a table with the 10 songs with the most energy

MM %>% 
  arrange(-energy) %>% 
  select(track_name, album_name, energy) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = c(1), color = "black",bold = TRUE)


## Analysis of a specific album

MM %>%
  arrange(track_number) %>% 
  filter(album_name == 'Strangers to Ourselves') %>%
  select(track_name,track_number,energy,valence,tempo) %>% 
  mutate(
    energy = cell_spec(energy, "html", bold = ifelse(energy == max(energy), TRUE, FALSE)),
    valence = cell_spec(valence, "html", bold = ifelse(valence == max(valence), TRUE, FALSE))
  ) %>%
  kable(format = 'html',escape=F) %>% 
  kable_styling("striped", full_width = F, position = "left",bootstrap_options = c("striped", "hover", "condensed", "responsive"))
























