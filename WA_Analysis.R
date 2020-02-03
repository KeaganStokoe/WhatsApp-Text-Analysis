#_____________________________________________WhatsApp Message Analysis _____________________________________
#____________________________________________________________________________________________________________

library("tidyr")
library("dplyr")
library("ggplot2"); #theme_set(theme_minimal())
library("lubridate")
library("rwhatsapp")
library("tidytext")
library("stopwords")

history <- "/Users/keaganstokoe/Documents/Quanta-CS/R /WhatsApp/Bullring/bullring.txt"
chat <- rwa_read(history, format = "[yyyy/MM/dd, HH:mm:ss]")

to_remove <- c((stopwords),
               "media",
               "omitted",
               "image",
               "i'm",
               "i'll",
               "audio",
               "video",
               "gif",
               "fuck", 
               "fucking",
               "shit")

chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)


#__________________________________________ Messages per day ________________________________________

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill = "#52854C") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")

#__________________________________________ Messages per day ________________________________________

#_______________________________________Number of messages sent ________________________________________

chat_clean %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity", fill = "#52854C") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")

#_________________________________________Number of messages sent ________________________________________

#__________________________________________ Remove stop words ________________________________________


chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)
  

#__________________________________________ Unique Words - author ________________________________________

chat_clean%>%
  filter(author == "Blake Rautenbach") %>% 
  count(word, sort = TRUE) %>% 
  #filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 7, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE, fill = 'steelblue') +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of John")

#__________________________________________ Emojis __________________________________________________


library("ggimage")
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)

chat_clean%>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#__________________________________________ Emojis ___________________________________________________


#__________________________________________ Lexical Diversity ________________________________________

chat_clean%>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()

#__________________________________________ Important words used ________________________________________

chat_clean%>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 2) %>%
  group_by(author) %>%
  top_n(n = 7, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words using weighting & frequency")

#__________________________________________ Most used words ________________________________________

chat_clean%>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 4, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most commonly used words")

#__________________________________________ Most used words ________________________________________

#__________________________________________ Sentiment Analysis ________________________________________

library(tidytext)


chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)


chat_clean %>%
  group_by(author) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

chat_clean %>%
  filter(author == "Keagan Stokoe") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

library(tidyr)

#__________________________________________ Bing Analysis ________________________________________


bullring_sentiment_bing <- chat_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(author, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#__________________________________________ Bing Analysis ________________________________________

#__________________________________________ AFINN Analysis ________________________________________


bullring_sentiment_afinn <- chat_clean %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(author) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "Group Chat Sentiment")

bullring_sentiment_afinn%>%
  ggplot(aes(author, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 3, scales = "free_y")

#__________________________________________ AFINN Analysis ________________________________________

bing_and_nrc <- bind_rows(chat_clean %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          chat_clean %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#__________________________________________ Bing Wordcounts ________________________________________


bing_word_counts <- chat_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#__________________________________________ Bing Wordcounts ________________________________________

#__________________________________________ Wordcloud ______________________________________________


library(wordcloud)

chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)

chat_clean%>%
  count(word) %>%
  with(wordcloud(word, n,colors = c("#D55E00", "#009E73"), max.words = 100))

#__________________________________________ Wordcloud _______________________________________________

chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)

library(reshape2)

chat_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#D55E00", "#009E73"),
                   max.words = 100)

