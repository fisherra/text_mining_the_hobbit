###################################
##                               ##
##   The Hobbit Text Analysis    ##
##                               ##
###################################

## Character Counts

# Fisher Ankney
# January 25, 2018

## Libraries 
library('tidyverse')
library('stringr')
library('tidytext')


## Source > https://archive.org/stream/TheHobbitByJ.R.RTolkien/The%20Hobbit%20by%20J.R.R%20Tolkien_djvu.txt
the_hobbit <- as.tibble(read_file("the_hobbit/input/hobbit_body.txt"))

tidy_hobbit <- the_hobbit %>%
  unnest_tokens(word, value)      # split words into elements of a df using tidytext

tidy_hobbit <- tidy_hobbit %>%
  anti_join(stop_words)           # remove words like to, it, and the

tidy_hobbit$linenumber <- 1:nrow(tidy_hobbit)

# Main Character Frequency
bilbo_count <- str_count(the_hobbit, "Bilbo")
bilbo_count

thorin_count <- str_count(the_hobbit, "Thorin")
thorin_count

gandalf_count <- str_count(the_hobbit, "Gandalf")
gandalf_count

smaug_count <- str_count(the_hobbit, "Smaug")
smaug_count

gollum_count <- str_count(the_hobbit, "Gollum")
gollum_count

bard_count <- str_count(the_hobbit, "Bard")
bard_count



# Character Frequency Timeline

bilbo_meter <- tidy_hobbit %>%
  filter(word == "bilbo")  %>%
  group_by(index = linenumber %% 100) %>%
  summarise(count = n())

bilbo_meter %>%
  ggplot(aes(index, count)) +
  geom_col(show.legend = FALSE,
           fill = "forestgreen") + 
  xlab("Book Progression") + 
  ylab("Bilbo Presence") + 
  labs(title = "How often Bilbo is mentioned in 'The Hobbit'") +
  theme_minimal()



gandalf_meter <- tidy_hobbit %>%
  filter(word == "gollum")  %>%
  group_by(index = linenumber %% 100) %>%
  summarise(count = n())

gandalf_meter %>%
  ggplot(aes(index, count)) +
  geom_col(show.legend = FALSE,
           color = "forestgreen") + 
  xlab("Book Progression") + 
  ylab("Bilbo Presence") + 
  labs(title = "How often Bilbo is mentioned in 'The Hobbit'") +
  theme_minimal()
  

# The 13 Dwarves
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

# plot dwarf mentions most popular to least popular

# not working
dwarves <- c(dwalin, balin, kili, fili, dori,
             nori, ori, oin, gloin, bifur,
             bofur, bombur, thorin)

dwarves <- names(c("dwalin", "balin", "kili", "fili", "dori",
                   "nori", "ori", "oin", "gloin", "bifur",
                   "bofur", "bombur", "thorin")
)


