#Text Analysis

library(tidytext)
library(genius)
library(geniusr)

sad_words %>%
  filter(lexicon == "nrc", sentiment == 'sadness') %>% #nrc is a library
  select(word) %>%
  mutate(sad = T)

joy_words %>%
  filter(lexicon == "nrc", sentiment == 'joy') %>%
  select(word) %>%
  mutate(joy = T)

#Next I removed stop words and left-joined the sad and joy word lists into my set of lyrics 
#to calculate the percent of sad words and the percent of joy words that appeared in each song.

with_sentiment %
anti_join(stop_words) %>%
  left_join(sad_words) %>%
  left_join(joy_words) %>%
  summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
            pct_joy = round(sum(joy, na.rm = T) / n(), 4),
            sad_minus_joy = pct_sad - pct_joy)

Downer Index = (distance+(1-pctsadwords))/2
1 == happier




Genius

T3T1rGG3e6CZ5CjpJktDGaVpEgfG68wgKxq2IgQLZ76Vf5pAxp0hxzWF5eWssSBC #client ID
wJxKRyvvR7nfKED3JgkbiEd1vXugm4fUddObdUrXqThfoFubmy0M2ngY-kUiKrU99CZzQeD4gkyMYqJpCsBC_g #client secret
JAMCH8S0xZZJKu4exNufdoX38p4iCLXOZJcA9MyzxfvI1gcHHMQKCi0GCgUkaF6g #client access token

genius_token()

get_song(song_id = 90479)
get_artist(artist_id = 1421)
get_album(album_id = 491200)
search_genius(search_term = "Compton")

get_lyrics_id(song_id = 90479)

get_lyrics_search(artist_name = "Kanye West",
                  song_title = "Good Morning") %>% 
  # get lyric bigrams
  unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
  # look for good morning
  filter(bigram == "good morning") %>% 
  # count bigram frequency
  nrow()


emotions_math <- genius_album(artist = "Margaret Glaspy", album = "Emotions and Math")

# Example with 2 different artists and albums
artist_albums <- tribble(
  ~artist, ~album,
  "J. Cole", "KOD",
  "Sampha", "Process"
)


artist_albums %>%
  add_genius(artist, album)


# Example with 2 different artists and songs
artist_songs <- tribble(
  ~artist, ~track,
  "J. Cole", "Motiv8",
  "Andrew Bird", "Anonanimal"
)

artist_songs %>%
  add_genius(artist, track, type = "lyrics")


memory_street <- genius_lyrics(artist = "Margaret Glaspy", song = "Memory Street")

#################3
#New 

library(tm)


#Check the file you downloaded with radiohead_lyrics-master


#####

library(tidyverse) #can we actually do R without it? I'd say no
library(genius) #we'll need this for the lyrics
library(tidytext) #for text tidying
library(ggtextures) #for glittery plots
library(extrafont) #to add personalizzed fonts to ggplot output
library(scales) #will be needed for percentage scales in ggplot
library(widyr) #to find correlations between songs
library(ggraph) #plotting network maps
library(igraph) #same

#First, downloading each TS album
ts1 <- genius_album(artist = "Taylor Swift", album = "Taylor Swift")%>%
  mutate(album = "Taylor Swift")

ts2 <- genius_album(artist = "Taylor Swift", album = "Fearless")%>%
  mutate(album = "Fearless")%>%
  filter(!track_n %in% 14:16)

ts3 <- genius_album(artist = "Taylor Swift", album = "Speak Now")%>%
  mutate(album = "Speak Now")

ts4 <- genius_album(artist = "Taylor Swift", album = "Red")%>%
  mutate(album = "Red")

ts5 <- genius_album(artist = "Taylor Swift", album =  "1989")%>%
  mutate(album = "1989")

ts6 <- genius_album(artist = "Taylor Swift", album = "Reputation")%>%
  mutate(album = "Reputation")


#Putting all togetherin the same df
tay <- rbind(ts1, ts2, ts3, ts4, ts5, ts6)


#Patterns to be removed from the data
remove.list <- paste(c("Demo Recording", "Voice Memo", "Pop Version", "Acoustic Version"), collapse = '|')


#Applying the changes
tay <- tay%>%
  filter(!grepl(remove.list, track_title)) 


#Just in case: save!
save(tay, file = "taytay.Rdata")


#Tidying our data. Instead of having a line per row, we want a word per row

#Tokenizing our data:
tay_tok <- tay%>%
  #word is the new column, lyric the column to retrieve the information from
  unnest_tokens(word, lyric)

tay_tok %>%
  count(word, sort = TRUE) 

tidy_taylor <- tay_tok %>%
  anti_join(stop_words)

tidy_taylor%>%
  count(word, sort = TRUE)


## Data Viz


#Loading fonts
loadfonts(device = "win")

#The pink pattern I will be using for the plot
img = "pink.jpg"

#Let's plot!
tidy_taylor %>%
  count(word, sort = TRUE) %>%
  #filtering to get only the information we want on the plot
  filter(n > 70,
         word != "di",
         word != "ooh",
         word != "ey")%>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_textured_col(image = img, color = "white", width = 0.8)+
  geom_text(aes(label = reorder(word, n)), 
            hjust = 1.2,vjust = 0.3, color = "white", 
            size = 5,  family="Harlow Solid Italic")+
  labs(y = "Number  of times mentioned", 
       x = NULL,
       title = "Most frequent words in Taylor Swift lyrics",
       caption = "                                                                                                                                    Ariane Aumaitre - @ariamsita")+
  coord_flip()+
  ylim(c(0, 210))+ # I didn't want to have the bars covering the whole plotting area
  theme_minimal()+
  #now making more visually appealing
  theme(plot.title = element_text( hjust = 0.5,vjust = 3, color = "maroon3", size = 14,  family="Forte"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, color = "grey40"),
        axis.title.x = element_text(size = 10, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40"),
        plot.margin=unit(c(2,1,1.5,1.2),"cm"))+
  ggsave("song_count.png")



## Comparing with sentiment analysis


tay_sentiment <- tidy_taylor%>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



#Right order for albums:
tay_order <- c("Taylor Swift", "Fearless", "Speak Now", "Red", "1989", "Reputation")
tay_sentiment$album <- factor(tay_sentiment$album, levels = tay_order)

#Plot:
tay_sentiment%>%
  ggplot(aes(reorder(track_title, sentiment), sentiment, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free")+
  scale_fill_manual(values = c("skyblue1", "lightgoldenrod1", "mediumorchid3", "red2", "plum1", "slategray"))+
  labs(x = NULL,
       y = "Sentiment",
       title = "Taylor Swift's songs ranked by sentiment",
       caption = "Ariane Aumaitre - @ariamsita")+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, hjust = 0.4, face = "bold"),
        axis.title.y = element_text(hjust = 0.05, size = 7, color = "grey40", angle = 0),
        axis.title.x =  element_text(size = 8, color = "grey40"),
        axis.text.x = element_text(size = 6.5, color = "grey40"),
        axis.text.y = element_text(size = 6.5, color = "grey40"), 
        strip.text = element_text(size = 9, color = "grey40", face = "bold"),
        plot.caption = element_text(size = 7.5, color = "grey40"))+
  coord_flip()+
  ggsave("sentiment.png", width = 10, height = 5.5)



##Checking the relationship among albums

#First, factor reordering
tidy_taylor$album <- factor(tidy_taylor$album, levels = tay_order)

#Frequency df
tay_frequency <- tidy_taylor%>%
  count(album, word) %>%
  group_by(album) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(album, proportion)%>%
  select(-c(`Taylor Swift`, `Speak Now`, `Reputation`))%>%
  gather(album, proportion, c(Fearless,Red))



tay_frequency%>%
  ggplot(aes(x = proportion, y = `1989`)) +
  geom_abline(color = "maroon3", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, 
              width = 0.3, height = 0.3, color = "maroon3") +
  geom_text(aes(label = word), check_overlap = TRUE, 
            vjust = 1.5, color = "grey40") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~album, nrow = 1, strip.position = "bottom") +
  coord_equal()+
  theme_minimal()+
  labs(x = "Word frequency",
       y = "Word frequency 1989",
       title = "Comparing Taylor Swift's albums",
       caption = "Ariane Aumaitre - @ariamsita")+
  theme(plot.title = element_text(size = 13, hjust = 0.4, face = "bold"),
        axis.title.y = element_text(hjust = 0.5, size =9 , color = "grey40"),
        axis.title.x =  element_text(size = 8, color = "grey40"),
        axis.text.x = element_text(size = 6.5, color = "grey40"),
        axis.text.y = element_text(size = 6.5, color = "grey40"), 
        strip.text = element_text(size = 9, color = "grey40", face = "bold"),
        plot.caption = element_text(size = 7.5, color = "grey40"))+
  ggsave("frequency.png")



##Drawing a network map of Taylor Swift songs

tay_cors <- tidy_taylor %>%
  pairwise_cor(track_title, word, sort = TRUE)

set.seed(123)

tay_cors %>%
  filter(correlation > .13) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link( show.legend = FALSE, aes(edge_alpha = correlation)) +
  geom_node_point(color = "pink", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5, color = "grey40") +
  theme_void()+
  ggsave("taymap.png", width = 15, height = 11)




##Next Up


library(magrittr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(readr)
library(circlize)
library(reshape2)


lyrics <- read_csv("~/Downloads/taylor_swift_lyrics.csv")

lyrics$length <- str_count(lyrics$lyric,"\\S+")

lyrics_text <- lyrics$lyric



#Removing punctations and alphanumeric content
lyrics_text<- gsub('[[:punct:]]+', "", lyrics_text)
lyrics_text<- gsub("([[:alpha:]])\1+", "", lyrics_text)

#creating a text corpus
docs <- Corpus(VectorSource(lyrics_text))

# Converting the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Removing english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# creating term document matrix
tdm <- TermDocumentMatrix(docs)
# defining tdm as matrix
m <- as.matrix(tdm)
# getting word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# creating a data frame with words and their frequencies
lyrics_wc_df <- data.frame(word=names(word_freqs), freq=word_freqs)

lyrics_wc_df <- lyrics_wc_df[1:300,]

# plotting wordcloud

set.seed(1234)
wordcloud(words = lyrics_wc_df$word, freq = lyrics_wc_df$freq,
          min.freq = 1,scale=c(1.8,.5),
          max.words=200, random.order=FALSE, rot.per=0.15,
          colors=brewer.pal(8, "Dark2"))



visualize_bigrams <- function(bigrams) {
set.seed(2016)
a <- grid::arrow(type = “closed”, length = unit(.15, “inches”))

bigrams %>%
graph_from_data_frame() %>%
ggraph(layout = “fr”) +
geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
geom_node_point(color = “lightblue”, size = 5) +
geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
ggtitle(“Network graph of bigrams”) +
theme_void()
}

lyrics_bigrams %>%
filter(n > 3,
!str_detect(word1, “\\d”),
!str_detect(word2, “\\d”)) %>%
visualize_bigrams()

lyric_sentiment %>%
count(track_title,sentiment,sort=TRUE) %>%
group_by(sentiment) %>%
top_n(n=5) %>%
ggplot(aes(x=reorder(track_title,n),y=n,fill=sentiment)) +
geom_bar(stat=”identity”,show.legend = FALSE) +
facet_wrap(~sentiment,scales=”free”) +
xlab(“Sentiments”) + ylab(“Scores”)+
ggtitle(“Top songs associated with emotions and sentiments”) +
coord_flip()






chordDiagram







