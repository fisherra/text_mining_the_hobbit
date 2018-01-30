###################################
##                               ##
##   The Hobbit Text Analysis    ##
##                               ##
###################################

## Sentiment Analysis

# Fisher Ankney
# January 29, 2018

## Libraries 
library('tidyverse')
library('stringr')
library('tidytext')
library('wordcloud') 

## Source > https://archive.org/stream/TheHobbitByJ.R.RTolkien/The%20Hobbit%20by%20J.R.R%20Tolkien_djvu.txt
the_hobbit <- as.tibble(read_file("the_hobbit/input/hobbit_body.txt"))

tidy_hobbit <- the_hobbit %>%
  unnest_tokens(word, value)      # split words into elements of a df using tidytext

tidy_hobbit <- tidy_hobbit %>%
  anti_join(stop_words)           # remove words like to, it, and the

tidy_hobbit$linenumber <- 1:nrow(tidy_hobbit)

## Adjective Analysis

# Joyful Words 
nrcjoy <- get_sentiments("nrc") %>%    # from the tidytext
  filter(sentiment == "joy")

happy_cloud <- tidy_hobbit %>%
  inner_join(nrcjoy) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 color = brewer.pal(n = 8,"Dark2")))
happy_cloud

# Frequency of joy throught the book
joy_meter <- tidy_hobbit %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "joy") %>%
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = n()) %>% 
  mutate(method = "NRC")

joy_meter %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE,
           fill = "orange") + 
  xlab("Book Progression") + 
  ylab("Joy Meter") + 
  labs(title = "Words with Joyous Sentiment in 'The Hobbit'") +
  theme_minimal()




# Sad Words 
nrcsad <- get_sentiments("nrc") %>%    # from the tidytext
  filter(sentiment == "sadness")

sad_cloud <- tidy_hobbit %>%
  inner_join(nrcsad) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 color = brewer.pal(n = 8,"Dark2")))


# Frequency of sadness throught the book
sad_meter <- tidy_hobbit %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "sadness") %>%
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = n()) %>% 
  mutate(method = "NRC")

sad_meter %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE, 
           fill = "lightsteelblue") + 
  xlab("Book Progression") + 
  ylab("Sad Meter") + 
  labs(title = "Words with Sad Sentiment in 'The Hobbit'") +
  theme_minimal() + 

  
## Fearful Words
nrcfear <- get_sentiments("nrc") %>%   
  filter(sentiment == "fear")

fear_cloud <- tidy_hobbit %>%
  inner_join(nrcfear) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 color = brewer.pal(n = 8,"Dark2")))


# Frequency of sadness throught the book
fear_meter <- tidy_hobbit %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "fear") %>%
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = n()) %>% 
  mutate(method = "NRC")

fear_meter %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE, 
           fill = "Red4") + 
  xlab("Book Progression") + 
  ylab("Fear Meter") + 
  labs(title = "Words with Fear Sentiment in 'The Hobbit'") +
  theme_minimal()


### Happy, Sad, and Fear all together now
ggplot() + 
  geom_smooth(aes(fear_meter$index,
                  fear_meter$sentiment, 
                  color = "Fear"),
              se = FALSE) + 
  geom_smooth(aes(joy_meter$index, 
                  joy_meter$sentiment, 
                  color = "Joy"),
              se = FALSE) + 
  geom_smooth(aes(sad_meter$index, 
                  sad_meter$sentiment, 
                  color = "Sadness"),
              se = FALSE) + 
  xlab("Book Progression Index") + 
  ylab("Sentiment Index") +
  labs(
    title = "Specific Sentiment in 'The Hobbit'"
  ) + 
  theme_minimal() + 
  scale_color_manual(
    name = "Sentiment",
    values = c("red4", "orange", "lightsteelblue")
  )



# Overall Sentiment
hobbit_sent <- tidy_hobbit %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

hobbit_sent[["sign"]] = ifelse(hobbit_sent[["sentiment"]] >=0, "positive", "negative")

hobbit_sent %>%
  ggplot() + 
  geom_col(aes(index, 
               sentiment,
               fill = sign)
           ) + 
  scale_fill_manual(name = "Sentiment",
                    values = c("positive" = "royalblue4", "negative" = "red4"),
                    guide = guide_legend(reverse=TRUE)) + 
  xlab("Book Progression Index") + 
  ylab("Sentiment Index") + 
  ggtitle("General AFINN Method Sentiment in 'The Hobbit'") + 
  theme_minimal()
