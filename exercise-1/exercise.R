# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
install.packages(janeaustenr)
install.packages("tidytext")
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# Load booksinto a dataframe using the austen_books() function
df <- austen_books()

# How many books are in the dataset?
num.books <- df %>% select(book) %>% group_by(book) %>% unique() %>% nrow()

# Which book has the most lines?
most.lines <- df %>% group_by(book) %>% summarise(lines = n()) %>% filter(lines == max(lines)) %>% select(book)

# Use the unnest_tokens function to generate the full list of words
words <- unnest_tokens(df, 'words', 'text')

# Which words are most common (regardless of which book them come from)?
most.common <- words %>% group_by(words) %>% summarise(num = n()) %>% arrange(-num)

# Remove stop words by performing an anti_join with the stop_words dataframe
no.stop.words <- words %>% mutate(word = words) %>% select(word) %>% anti_join(stop_words, by = 'word')

# Which non stop-words are most common?
non.stop.count <- no.stop.words %>% group_by(word) %>% summarise(count = n()) %>% arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
no.stop.words %>% 
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()