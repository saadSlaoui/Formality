# Wiki vs Simple Wiki comparison

library(WikipediR)
library(rvest)
library(RCurl)
library(dplyr)

#' Returns a sentence decomposition of the text content of a wikipedia page
#' given any topic argument. Adapted to also look into the simple wikipedia 
#' instead of the standard wikipedia.
#' 
#' @param topic the query whose data we want to extract
#' @param simple boolean indicating whether the query shiuld be made into 
#' the simple wikipedia. Defaults to false
#' @return A vector of the sentences appearing in the corresponding article
#' @examples extract_wiki("Abnormal_psychology")
#' @examples extract_wiki("Psychology, simple = T)
#' 
extract_wiki <- function(topic, simple = F) {
  
  # Get all the paragraphs on the wikipedia page
  url <- ifelse(simple,
                paste("https://simple.wikipedia.org/wiki/", topic, sep = ""),
                paste("https://en.wikipedia.org/wiki/", topic, sep = ""))
  wiki_html <- read_html(url)
  
  # Clean up the text
  text_vector <- paste(read_html(url) %>% html_nodes(xpath = "//p") %>% html_text(), collapse = "")
  text_vector <- gsub(text_vector, pattern = ".*0{8,}([0-9,]+).*", replacement = "")
  text_vector <- text_vector %>% gsub(pattern = "\\[", replacement = "") %>% 
                 gsub(pattern = "\\]", replacement = "") %>% gsub(pattern = "[0-9]", replacement = "")
  
  # divide in sentences by looking for . 
  text_vector <- str_split(text_vector, pattern = "\\.")[[1]]
  text_vector <- str_trim(text_vector)
  
  # account for sentence detections on Dr., Mr., ... by removing sentences of less than 10 characters
  text_vector <- text_vector[-which(sapply(text_vector, function (x) { nchar(x) < 10 }))] 
  
  return(text_vector)
}

####################################################

# The function is now operational and ready to extract sentences about any given topic from both
# the simple wiki and the standard wiki

simple_sample <- extract_wiki("Psychology", simple = T)
standard_sample <- extract_wiki("Psychology")

# Let's measure a bunch of stuff

# Average frequency of the (percentage) 
word_freq("the", simple_sample)    # ~ 9.5%
word_freq("the", standard_sample)  # ~ 8%

# Average sentence length
avg_sent_length(simple_sample)    # 17.15
avg_sent_length(standard_sample)  # 21.04

# Location of the (1 = beginning, 3 = end)
mean(locate_word("the", simple_sample))    # 1.065
mean(locate_word("the", standard_sample))  # 1.034

# Average word Length
mean(as.vector(sapply(simple_sample, function(sent) { avg_char_length(sent) })))   # 5.2
mean(as.vector(sapply(standard_sample, function(sent) { avg_char_length(sent) }))) # 6


# Comparing these values with the formality curves obtained with the answers dataset gives the
# impression that both of those texts are extremely formal. However, since the nature of the
# answers dataset makes the "formal" level much lower than it would be by other standards,
# this is not necessarily conclusive.

# Main point here: 'the' appears MORE frequently in the simple wiki than it does in the
# standard wiki. In every other measure, the standard wiki sentences correlate with slightly
# more formal language (longer sentences, 'the' appears earlier in the sentence, larger 
# average word length).

# It thus seems that we have found a good candidate for the motivation of using the: context!
# Simple wiki mainly aims to ensure the understanding of the reader, and thus the text is
# extremely self contained and attempts to clarify any confusion. 
# This strips away the informality that normally comes along with decrease of context-emphasis.
