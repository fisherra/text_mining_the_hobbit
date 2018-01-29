###################################
##                               ##
##   The Hobbit Text Analysis    ##
##                               ##
###################################

## Sentiment Analysis

# Fisher Ankney
# January 25, 2018


#### Preparation

# load libraries
library('tidyverse')
library('stringr')
library('tidytext')

# read in the hobbit main text (chapter 1 - chapter 15) as a tibble
the_hobbit <- as.tibble(read_file("the_hobbit/the_hobbit.txt"))

# split each word into an element via tidytext library
tidy_hobbit <- the_hobbit %>%
  unnest_tokens(word, value)

# take out common words like "to", "it" and "the"
tidy_hobbit <- tidy_hobbit %>%
  anti_join(stop_words)





## sentiment ananylsis

# joyful words
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

happy_hobbit <- tidy_hobbit %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)



joy_meter <- tidy_hobbit %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "joy") %>%
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = n()) %>% 
  mutate(method = "NRC")

joy_meter %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE)



# sad words
nrcsad <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")

sad_hobbit <- tidy_hobbit %>%
  inner_join(nrcsad) %>% 
  count(word, sort = TRUE)




# fearful words 
nrcfear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

scared_hobbit <- tidy_hobbit %>%
  inner_join(nrcfear) %>%
  count(word, sort = TRUE)
scared_hobbit

# fear meter
fear_meter <- tidy_hobbit %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "fear") %>%
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = n()) %>% 
  mutate(method = "NRC")

fear_meter %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

# overall sentiment of the hobbit

# create word index numbers
tidy_hobbit$linenumber <- 1:nrow(tidy_hobbit)


the_hobbit$linenumber <- 1:nrow(the_hobbit)

# sentiment analysis positive v negative
afinn <- tidy_hobbit %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 400) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

afinn %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE)





