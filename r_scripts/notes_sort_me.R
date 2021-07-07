my_id <- 'your spotify id'
my_plists <- get_user_playlists(my_id)

my_plists2 <- my_plists %>%
  filter(playlist_name %in% c('Taiwan Top 50', 'France Top 50', 'Bolivia Top 50', 'U.S. Top 50'))

tracks <- get_playlist_tracks(my_plists2)
features <- get_track_audio_features(tracks)

tracks2 <- tracks%>%
  left_join(features, by="track_uri")

tracks2 <- tracks2%>%
  mutate(difference=speechiness-0.33)

green <- "#1ed760"
yellow <- "#e7e247"
pink <- "#ff6f59"
blue <- "#17bebb"

#Speechiness diff

viz1 <- ggplot(tracks2, aes(x=reorder(track_name, -difference), y=difference, fill=playlist_name, text=(paste("Track:", track_name, "<br>",
                                                                                                              "Artist:", artist_name, "<br>",
                                                                                                              "Speechiness:", speechiness))))+
  geom_col()+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        legend.position="none")+
  ylab("Speechiness Difference")+
  facet_wrap(~ playlist_name)+
  ggtitle("Speechiness Difference")

ggplotly(viz1, tooltip=c("text"))

#Keys

key_country <- tracks2%>%
  select(playlist_name, key)%>%
  group_by(playlist_name, key)%>%
  mutate(n=n())%>%
  unique()%>%
  group_by(key)%>%
  mutate(total=sum(n))%>%
  mutate(percent=round((n/total)*100))

head(key_country, 10)

viz2 <- ggplot(key_country, aes(x=key, fill=playlist_name, y = n, 
                                text = paste("Number of Songs: ", n, "<br>",
                                             "Percent Songs in Key: ", percent, "%")))+
  geom_bar(position="fill", width=0.5, stat = "identity")+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  labs(x="Key", y="Percent of Songs")+
  guides(fill=guide_legend(title="Playlist"))+
  theme_minimal()+
  ggtitle("Musical Key Percentage by Playlist")

ggplotly(viz2, tooltip=c("text"))

#Not a filled version of the same graph

viz3 <- ggplot(key_country, aes(x=key, fill=playlist_name, y = n, 
                                text = paste("Number of Songs: ", n, "<br>",
                                             "Percent Songs in Key: ", percent, "%")))+
  geom_bar(width=0.5, stat = "identity")+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  labs(x="Key", y="Number of Songs") +
  guides(fill=guide_legend(title="Playlist"))+
  theme_minimal()+
  ggtitle("Musical Key Makeup by Playlist")

ggplotly(viz3, tooltip=c("text"))


#Distribution of Dancibility (density)

viz4 <- ggplot(tracks2, aes(x=danceability, fill=playlist_name,
                            text = paste(playlist_name)))+
  geom_density(alpha=0.7, color=NA)+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  labs(x="Danceability", y="Density") +
  guides(fill=guide_legend(title="Playlist"))+
  theme_minimal()+
  ggtitle("Distribution of Danceability Data")

ggplotly(viz4, tooltip=c("text"))


#After looking at this graph, I wanted to know the range between each country’s most and least danceable song.

tracks3 <- tracks2 %>%
  group_by(playlist_name)%>%
  mutate(max=max(danceability))%>%
  mutate(min=min(danceability))%>%
  select(playlist_name, max, min)%>%
  unique()

#I used plotly to make a dumbbell plot showing the range in danceability values for each playlist

viz5 <- plot_ly(tracks3, color = I("gray80"),  
                hoverinfo = 'text') %>%
  add_segments(x = ~max, xend = ~min, y = ~playlist_name, yend = ~playlist_name, showlegend = FALSE) %>%
  add_markers(x = ~max, y = ~playlist_name, name = "Maximum Danceability Value", color = I(pink), text=~paste('Max Danceability: ', max)) %>%
  add_markers(x = ~min, y = ~playlist_name, name = "Minimum Danceability Value", color = I(blue), text=~paste('Min Danceability: ', min))%>%
  layout(
    title = "Playlist Danceability Range",
    xaxis = list(title = "Danceability"),
    yaxis= list(title=""))

ggplotly(viz5)



















