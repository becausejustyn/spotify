#Analyse saved data

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

streamHistory <- fromJSON("StreamingHistory0.json", flatten = TRUE)



## Analysing evolution of music using Spotify data

#https://rpubs.com/bhasinrl/spotify__data__analysis

str(streamHistory)

#Missing Values
songs_clean <- songs %>% 
  filter(!is.na(track_name) & !is.na(track_artist) & !is.na(track_album_name))

#Checking duplicate records
songs_clean[duplicated(songs_clean$Names) | duplicated(songs_clean$Names, fromLast = TRUE), ]

#New Var
songs_clean$year <- as.numeric(substring(songs_clean$track_album_release_date,1,4))

#Preview of Dataset
head(songs_clean, 20) %>%
  datatable(options = list(scrollCollapse = TRUE,scrollX = TRUE,
                           columnDefs = list(list(className = 'dt-center', targets = 1:4))
  ))


#Proportion of playlist genres

songs_clean_pie_data <- songs_clean %>% 
  group_by(playlist_genre) %>% 
  summarise(Total_number_of_tracks = length(playlist_genre))

ggplot(songs_clean_pie_data, aes(x="", y=Total_number_of_tracks, fill=playlist_genre)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))

#Correlation between variables

songs_correlation <- cor(songs_clean[,-c(1,2,4,5,6,7,8)])
corrplot(songs_correlation, type = "upper", tl.srt = 45)


#Density Plots of Variables

correlated_density <- ggplot(songs_clean) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(acousticness, fill ="acousticness", alpha = 0.1)) + 
  geom_density(aes(speechiness, fill ="speechiness", alpha = 0.1)) + 
  geom_density(aes(liveness, fill ="liveness", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Danceability, Valence, Acousticness, Speechiness and Liveness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Danceability, Valence, Acousticness, Speechiness and Liveness") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        text = element_text(size = 10)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")


#Histograms of loudness, duration and track_popularity

loudness_density <- ggplot(songs_clean) +
  geom_density(aes(loudness, fill ="loudness")) + 
  scale_x_continuous(name = "Loudness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Loudness") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Paired")

duration_ms_density <- ggplot(songs_clean) +
  geom_density(aes(duration_ms, fill ="duration_ms")) + 
  scale_x_continuous(name = "duration_ms") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of duration_ms") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Dark2")

track_popularity_density <- ggplot(songs_clean) +
  geom_density(aes(track_popularity, fill ="track_popularity")) + 
  scale_x_continuous(name = "track_popularity") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of track_popularity") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="RdBu")

grid.arrange(loudness_density, duration_ms_density,track_popularity_density, nrow = 3)


#Visualizing top artists within each genre

top_genre <- songs_clean %>% 
  select(playlist_genre, track_artist, track_popularity) %>% 
  group_by(playlist_genre,track_artist) %>% 
  summarise(n = n()) %>% 
  top_n(15, n)

tm <- treemap(top_genre, index = c("playlist_genre", "track_artist"), 
              vSize = "n", vColor = 'playlist_genre', palette =  viridis(6),
              title="Top 15 Track Artists within each Playlist Genre")



#Plots showing variability of characteristics across genres

radar_chart <- function(arg){
  songs_clean_filtered <- songs_clean %>% filter(playlist_genre==arg)
  radar_data_v1 <- songs_clean_filtered %>%
    select(danceability,energy,loudness,speechiness,valence,instrumentalness,acousticness)
  radar_data_v2 <- apply(radar_data_v1,2,function(x){(x-min(x)) / diff(range(x))})
  radar_data_v3 <- apply(radar_data_v2,2,mean)
  radar_data_v4 <- rbind(rep(1,6) , rep(0,6) , radar_data_v3)
  return(radarchart(as.data.frame(radar_data_v4),title=arg))
}

par(mfrow = c(2, 3))
Chart_pop<-radar_chart("pop")
Chart_rb<-radar_chart("r&b")
Chart_edm<-radar_chart("edm")
Chart_latin<-radar_chart("latin")
Chart_rap<-radar_chart("rap")
Chart_rock<-radar_chart("rock")


#Plots showing change in tracks’ feature values in last one decade

trend_chart <- function(arg){
  trend_change <- songs_clean %>% filter(year>2010) %>% group_by(year) %>% summarize_at(vars(all_of(arg)), funs(Average = mean))
  chart<- ggplot(data = trend_change, aes(x = year, y = Average)) + 
    geom_line(color = "#00AFBB", size = 1) +
    scale_x_continuous(breaks=seq(2011, 2020, 1)) + scale_y_continuous(name=paste("",arg,sep=""))
  return(chart)
}

trend_chart_track_popularity<-trend_chart("track_popularity")
trend_chart_danceability<-trend_chart("danceability")
trend_chart_energy<-trend_chart("energy")
trend_chart_loudness<-trend_chart("loudness")
trend_chart_duration_ms<-trend_chart("duration_ms")
trend_chart_speechiness<-trend_chart("speechiness")

plot_grid(trend_chart_track_popularity, trend_chart_danceability, trend_chart_energy, 
          trend_chart_loudness, trend_chart_duration_ms, trend_chart_speechiness,ncol = 2, label_size = 1)



