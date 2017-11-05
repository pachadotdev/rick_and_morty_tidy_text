# Rick and Morty and Tidy Data Principles

# Rick and Morty Can Be So Tidy

if (!require("pacman")) install.packages("pacman")
p_load(data.table,tidyr,stringr,tidytext,dplyr,janitor,ggplot2,viridis,ggstance,igraph)
p_load_gh("thomasp85/ggraph","dgrtwo/widyr")

rick_and_morty_subs = as_tibble(fread("01_rick_and_morty/rick_and_morty_subs.csv"))

rick_and_morty_subs_tidy = rick_and_morty_subs %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

rick_and_morty_subs_tidy %>%
  count(word, sort = TRUE)

bing = sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

bing

rick_and_morty_sentiment = rick_and_morty_subs_tidy %>%
  inner_join(bing) %>% 
  count(episode_name, index = linenumber %/% 50, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  left_join(rick_and_morty_subs_tidy[,c("episode_name","season","episode")] %>% distinct()) %>% 
  arrange(season,episode) %>% 
  mutate(episode_name = paste(season,episode,"-",episode_name),
         season = factor(season, labels = c("Season 1", "Season 2", "Season 3"))) %>% 
  select(episode_name, season, everything(), -episode)

rick_and_morty_sentiment

ggplot(rick_and_morty_sentiment, aes(index, sentiment, fill = season)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~season, nrow = 3, scales = "free_x", dir = "v") +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Rick and Morty",
       y = "Sentiment") +
  scale_fill_viridis(end = 0.75, discrete=TRUE) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())

rick_and_morty_sentiment_favourites = rick_and_morty_sentiment %>% 
  filter(grepl("S03 E03|S03 E07|S01 E06|S02 E03|S02 E07", episode_name))
  
ggplot(rick_and_morty_sentiment_favourites, aes(index, sentiment, fill = season)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~episode_name, ncol = 5, scales = "free_x", dir = "h") +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment in Rick and Morty (Creator's favourite episodes)",
       y = "Sentiment") +
  scale_fill_viridis(end = 0.75, discrete=TRUE) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())

# Looking at Units Beyond Words

rick_and_morty_sentences = rick_and_morty_subs %>% 
  group_by(season) %>% 
  unnest_tokens(sentence, text, token = "sentences") %>% 
  ungroup()

rick_and_morty_sentences$sentence[50]

bingnegative = sentiments %>%
  filter(lexicon == "bing", sentiment == "negative")

wordcounts = rick_and_morty_subs_tidy %>%
  group_by(season, episode) %>%
  summarize(words = n())

rick_and_morty_subs_tidy %>%
  semi_join(bingnegative) %>%
  group_by(season, episode) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("season", "episode")) %>%
  mutate(ratio = negativewords/words) %>%
  top_n(1)

# Networks of Words

rick_and_morty_words = rick_and_morty_subs_tidy %>%
  filter(season == "S01")

word_cooccurences = rick_and_morty_words %>%
  pairwise_count(word, linenumber, sort = TRUE)

word_cooccurences

set.seed(1717)

word_cooccurences %>%
  filter(n >= 25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle(expression(paste("Word Network in Rick and Morty's ", 
                           italic("Season One")))) +
  theme_void()
