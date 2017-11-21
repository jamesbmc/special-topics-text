# Exercise-2
# What are informatics courses about?

# Set up
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)

# Use the `read_html` function to load this webpage:
# https://www.washington.edu/students/crscat/info.html
info.page <- read_html("https://www.washington.edu/students/crscat/info.html", stringsas)

# Extract the text of each course title from the page by using the `html_nodes`
# function to identify the *bold paragraphs*. 
# Extract the text by passing those element to the `html_text` function
titles <- html_nodes(info.page, "b") %>% 
  html_text()

# Extract the *descriptions* of each course in the same process as above, searching for 
# paragraphs (p)
descriptions <- html_nodes(info.page, "p") %>% 
  html_text()

# Create a dataframe by combinding your course titles and descriptions (skip the first description)
data <- data.frame(title = titles, description = descriptions[-1], stringsAsFactors = F)

# How many courses are in the catalogue?
num.courses <- nrow(data)

# Create a tidytext sturcture of all words
words <- data %>% 
  unnest_tokens(word, description)

# Which words do we use to describe our courses?
word.count <- words %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

# Create a set of stop words by adding (more) irrelevant words to the stop_words dataframe
more.words <- data.frame(
  word=c("myplan", "info", "course"),
  lexicon="custom"
)

all.stop.words <- rbind(stop_words, more.words)
# Remove stop words by performing an anti_join with the stop_words dataframe
no.stop.words <- word.count %>% 
  anti_join(all.stop.words, by = "word") 

# Which non stop-words are most common?


# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
no.stop.words %>% 
  filter(count > 10) %>% 
  mutate(word = reorder(word, count)) %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  coord_flip()
