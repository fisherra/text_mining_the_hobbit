###################################
##                               ##
##   The Hobbit Text Analysis    ##
##                               ##
###################################

## Word Counts

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




#### Book Statistics
# how many characters?
str_length(the_hobbit) # 514,751 characters 

# how many words?
str_count(the_hobbit, " ") + 1 # 95,87

# how many sentences?
str_count(the_hobbit, "[.!?]") # 5,967

# how long to read the hobbit?
((str_count(the_hobbit, " ") + 1) / 200) / 60 # 8 hours 
# 200 word per minute ~average adult reading speed, 60 min / hour


# visualize the most common words in the novel
tidy_hobbit %>%
  count(word, sort = TRUE) %>%
  filter(n > 80) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## Hobbit word cloud
library(wordcloud)

tidy_hobbit %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 color = brewer.pal(n = 8,"Set1")))