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


# Extract the text of each course title from the page by using the `html_nodes`
# function to identify the *bold paragraphs*. 
# Extract the text by passing those element to the `html_text` function

# Extract the *descriptions* of each course in the same process as above, searching for 
# paragraphs (p)


# Create a dataframe by combinding your course titles and descriptions (skip the first description)


# How many courses are in the catalogue?


# Create a tidytext sturcture of all words


# Which words do we use to describe our courses?


# Create a set of stop words by adding (more) irrelevant words to the stop_words dataframe


# Remove stop words by performing an anti_join with the stop_words dataframe


# Which non stop-words are most common?


# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words

