
library(textdata)

#Creating a variable called D1 which contains all of the lyrics from Drake's album "Thank Me Later"
D1 <- genius_album(artist = "Drake", album = "Thank Me Later")

#Using the piping operator to create a count of the most popular words in the album 
D1 %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  dplyr::count(word, sort = TRUE) -> D1Count

#Using more piping operators to create afinn sentiment as well as create a subset of the data that includes the top 20 most popular words 
D1Count %>% 
  inner_join(get_sentiments("afinn")) ->  D1Sentiment

D1Sentiment %>% 
  head(20) -> D1Sentiment2

#Creating a column called color that uses an if else statment to color the sentiment score by red if it is below 0 and green if it is above
D1Sentiment2$color <- ifelse(D1Sentiment2$value < 0, "red", "green")



#Creating a bar graph that shows each sentiment. The "color=color" in the ggplot() and scale_color_identity() are what allow the graph to color by red and green based off of the ifelse statement.
Drake1 <- ggplot(D1Sentiment2, aes(reorder(word, -n), value, color=color)) + geom_col(fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Drake - Thank me later", x="Top 20 Most Popular Words", y="Sentiment Score") +
  theme(plot.title = element_text(size=15,hjust = 0.5)) +
  scale_color_identity()

Drake1

#gridExtra to seperate each artist by genre


###From Metallica to Adele — Text Analysis of successful song lyrics with R

#create stop words you will want to remove
custom_stop_words <- bind_rows(tibble(word = c("ma","se", "sa", "coo", "yeah", "na", "whoo", "ow", "aw", "ooh",
                                               "yea", "aaaaahaaaahaaaah", "oooh", "ooo", "ha", "vrrm", "dah",
                                               "da", "um", "la", "yo", "huh", "ya", "eh", "ou", "woe", "yoe"), 
                                      lexicon = c("custom")), 
                               stop_words)


df$lyrics <- str_remove_all(df$lyrics, pattern = regex("\\[(.*?)\\]"))

#removing stop words
tidy_df <- df %>% 
  unnest_tokens(word, lyrics) %>% 
  anti_join(custom_stop_words, by = 'word')

tidy_df <- subset(tidy_df, !grepl("[0-9]", word)) #remove numbers


#find most common words
tidy_df %>%
  count(word, sort = TRUE) %>% 
  filter(n > 50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  ggtitle("Figure 2: Most used words") +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#how artists use words differently
frequency <- bind_rows(mutate(tidy_df %>% 
                                filter(artist == 'Eminem'), author = 'Eminem'),
                       mutate(tidy_df %>% 
                                filter(artist == 'Adele'), author = 'Adele'),
                       mutate(tidy_df %>% 
                                filter(artist == 'Metallica'), author = 'Metallica')) %>%
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n/ sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Eminem`:`Adele`)

library(scales)
ggplot(frequency, aes(x = proportion, y = `Metallica`, color = abs(`Metallica` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  ggtitle("Figure 3: Different word choices") +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = 'Metallica', x = NULL)


cor.test(data = [frequency$author == 'Adele', ], ~ proportion + 'Metallica')


#using sentiment scores
sentiments <- tidy_df %>%
  count(genre, word, sort = TRUE) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(genre) %>%
  summarize(score = sum(score * n) / sum(n))

sentiments %>%
  mutate(genre = reorder(genre, score)) %>%
  ggplot(aes(genre, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment score") +
  ggtitle('Figure 5: Sentiments per Genre')

#most influential words contributing to this average sentiment
tf_idf <- tidy_df %>% 
  count(genre, word) %>% 
  ungroup() %>% 
  bind_tf_idf(word, genre, n) %>% 
  arrange(desc(tf_idf))

tf_idf %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(genre) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = genre)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ genre, ncol = 3, scales = 'free') +
  coord_flip() +
  ggtitle("Figure 7: Highest tf-idf words per Genre")



#Network Plot

# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
head(data)

# short names
colnames(data) <- c("Africa", "East Asia", "Europe", "Latin Ame.",   "North Ame.",   "Oceania", "South Asia", "South East Asia", "Soviet Union", "West.Asia")
rownames(data) <- colnames(data)

# I need a long format
data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:10)]

# Base plot
chordDiagram(
  x = data_long, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE)
  }
)