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


# The 13 Dwarves
dwalin <- sum(str_count(tidy_hobbit$word, "dwalin"))
balin <- sum(str_count(tidy_hobbit$word, "balin"))
kili <- sum(str_count(tidy_hobbit$word, "kili"))
fili <- sum(str_count(tidy_hobbit$word, "fili"))
dori <- sum(str_count(tidy_hobbit$word, "dori"))
nori <- sum(str_count(tidy_hobbit$word, "nori"))
ori <- sum(str_count(tidy_hobbit$word, "ori"))
oin <- sum(str_count(tidy_hobbit$word, "oin"))
gloin <- sum(str_count(tidy_hobbit$word, "gloin"))
bifur <- sum(str_count(tidy_hobbit$word, "bifur"))
bofur <- sum(str_count(tidy_hobbit$word, "bofur"))
bombur <- sum(str_count(tidy_hobbit$word, "bombur"))
thorin <- sum(str_count(tidy_hobbit$word, "thorin"))

# plot dwarf mentions most popular to least popular

dwarves <- as.tibble(c(dwalin, balin, kili, fili, dori,
             nori, ori, oin, gloin, bifur,
             bofur, bombur, thorin))

dwarves$names <- c("dwalin", "balin", "kili", "fili", "dori",
                   "nori", "ori", "oin", "gloin", "bifur",
                   "bofur", "bombur", "thorin")
dwarves
