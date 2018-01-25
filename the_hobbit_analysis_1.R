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

# overall sentiment of the hobbit

# create word index numbers
tidy_hobbit$linenumber <- 1:nrow(tidy_hobbit)


the_hobbit$linenumber <- 1:nrow(the_hobbit)

# change me for the hobbit ### 
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




### Comparing Dwarves
# string count the number of times each dwarf is mentioned

dwalin <- str_count(the_hobbit, "Dwalin")
balin <- str_count(the_hobbit, "Balin")
kili <- str_count(the_hobbit, "Kili")
fili <- str_count(the_hobbit, "Fili")
dori <- str_count(the_hobbit, "Dori")
nori <- str_count(the_hobbit, "Nori")
ori <- str_count(the_hobbit, "Ori")
oin <- str_count(the_hobbit, "Oin")
gloin <- str_count(the_hobbit, "Gloin")
bifur <- str_count(the_hobbit, "Bifur")
bofur <- str_count(the_hobbit, "Bofur")
bombur <- str_count(the_hobbit, "Bombur")
thorin <- str_count(the_hobbit, "Thorin")

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




