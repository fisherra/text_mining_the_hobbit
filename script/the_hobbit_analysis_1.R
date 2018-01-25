###################################
##                               ##
##   The Hobbit Text Analysis    ##
##                               ##
###################################

# Fisher Ankney
# January 23, 2018



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


# gandalf analysis 

gandalf_meter <- tidy_hobbit %>% 
  
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "fear") %>%
  group_by(index = linenumber %/% 500) %>% 
  summarise(sentiment = n()) %>% 
  mutate(method = "NRC")

fear_meter %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE)













### Comparing Dwarves
# string count the number of times each dwarf is mentioned

dwalin <- str_count(tidy_hobbit, "dwalin")
balin <- str_count(tidy_hobbit, "balin")
kili <- str_count(tidy_hobbit, "kili")
fili <- str_count(tidy_hobbit, "fili")
dori <- str_count(tidy_hobbit, "dori")
nori <- str_count(tidy_hobbit, "nori")
ori <- str_count(tidy_hobbit, "ori")
oin <- str_count(tidy_hobbit, "oin")
gloin <- str_count(tidy_hobbit, "gloin")
bifur <- str_count(tidy_hobbit, "bifur")
bofur <- str_count(tidy_hobbit, "bofur")
bombur <- str_count(tidy_hobbit, "bombur")
thorin <- str_count(tidy_hobbit, "thorin")

# plot number of mentions per dwarf in bar chart

# not working
dwarves <- c(dwalin, balin, kili, fili, dori,
             nori, ori, oin, gloin, bifur,
             bofur, bombur, thorin)

dwarves <- names(c("dwalin", "balin", "kili", "fili", "dori",
                   "nori", "ori", "oin", "gloin", "bifur",
                   "bofur", "bombur", "thorin")
)



# dot or frequency plot of where in the book each dwarf is mentioned






#### Main Character Analysis

# string count main character mentions
bilbo <- str_count(the_hobbit, "Bilbo")
gandalf <- str_count(the_hobbit, "Gandalf")
smaug <- str_count(the_hobbit, "Smaug")
gollum <- str_count(the_hobbit, "Gollum")

# bar plot of mentions

# frequency plot throughout book sep by words




