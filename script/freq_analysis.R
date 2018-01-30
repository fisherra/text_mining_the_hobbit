###################################
##                               ##
##   The Hobbit Text Analysis    ##
##                               ##
###################################

## Word Frequency Analysis

# Fisher Ankney
# January 29, 2018

## Libraries 
library('tidyverse')
library('stringr')
library('tidytext')
library('wordcloud') 

## Source > https://archive.org/stream/TheHobbitByJ.R.RTolkien/The%20Hobbit%20by%20J.R.R%20Tolkien_djvu.txt
the_hobbit <- as.tibble(read_file("the_hobbit/input/hobbit_body.txt"))

#### Book Statistics
# How many words?
str_count(the_hobbit, " ") + 1 # 95,874 words

# How many sentences?
str_count(the_hobbit, "[.!?]") # ~5,967 sentences

# How long to read the hobbit? (200 wpm reading speed)
((str_count(the_hobbit, " ") + 1) / 200) / 60 # ~8 hours 


#### Common Words
tidy_hobbit <- the_hobbit %>%
  unnest_tokens(word, value)      # split words into elements of a df using tidytext

tidy_hobbit <- tidy_hobbit %>%
  anti_join(stop_words)           # remove words like to, it, and the


## A plot of the most common words
tidy_hobbit %>%
  count(word, sort = TRUE) %>%        # count words, sort by most highest occurance
  filter(n > 80) %>%                  # only show words that have been used 80+ times
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +              # plot the words by number of occurance
    geom_col(fill = "darkslateblue") + 
  xlab("Word") + 
  ylab("Number of Occurances") + 
  labs(title = "Most Common Meaningful Words in 'The Hobbit'") +
  coord_flip() +                      
  theme_minimal()

## A word cloud of the 100 most common words
hobbit_cloud <- tidy_hobbit %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 color = brewer.pal(n = 8,"Set1")))
hobbit_cloud
