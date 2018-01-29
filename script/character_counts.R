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



# Main Character Frequency
bilbo <- str_count(the_hobbit, "Bilbo")
bilbo

thorin <- str_count(the_hobbit, "Thorin")
thorin

gandalf <- str_count(the_hobbit, "Gandalf")
gandalf

smaug <- str_count(the_hobbit, "Smaug")
smaug

gollum <- str_count(the_hobbit, "Gollum")
gollum

bard <- str_count(the_hobbit, "Bard")
bard



# Character Frequency Timeline
bilbo
thorin
gandalf
smaug
gollum
bard

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


