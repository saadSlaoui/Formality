# Wiki vs Simple Wiki comparison

library(WikipediR)
library(rvest)
library(RCurl)
library(dplyr)
library(reshape2)

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
  text_vector <- paste(wiki_html %>% html_nodes(xpath = "//p") %>% html_text(), collapse = "")
  text_vector <- gsub(text_vector, pattern = ".*0{8,}([0-9,]+).*", replacement = "")
  text_vector <- text_vector %>% gsub(pattern = "\\[", replacement = "") %>% 
                 gsub(pattern = "\\]", replacement = "") %>% gsub(pattern = "[0-9]", replacement = "")
  
  # divide in sentences by looking for . 
  text_vector <- str_split(text_vector, pattern = "\\.")[[1]]
  text_vector <- as.vector(str_trim(text_vector))
  
  # account for sentence detections on Dr., Mr., ... by removing sentences of less than 10 characters
  if (length(which(nchar(text_vector) < 10)) != 0) {
         text_vector <- text_vector[-which(nchar(text_vector) < 10)]
  }
  
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


################################################

# We set out to expand our analysis to multiple topics in order to map the differences in a more
# tangible way

# Note for later: most articles on simple wiki are relatively short, so special attention should be paid
# to ensure that the content for a given topic is sufficient to yield sensible results

# We start by creating a database that will store measures of the four variables per topic and per source
# We initialize it with the psychology results and expand it with new topics

# Note for later: this database can be normalized and stored in an sql server

wiki_database <- 
  data.frame(topic = rep("psychology", 2),
             source = as.factor(c("wiki", "simple_wiki")),
             num_sentences = c(length(standard_sample), length(simple_sample)),
             the_freq = c(word_freq("the", standard_sample), 
                           word_freq("the", simple_sample)),
             avg_sentence_length = c(avg_sent_length(standard_sample),
                                      avg_sent_length(simple_sample)),
             avg_word_length = 
               c(mean(as.vector(sapply(standard_sample, function(sent) { avg_char_length(sent) }))),   
                 mean(as.vector(sapply(simple_sample, function(sent) { avg_char_length(sent) })))),
             the_location = c(mean(locate_word("the", standard_sample)), 
                               mean(locate_word("the", simple_sample)))
  )


#' Given a wiki database in data frame format, returns an extended dataframe 
#' containing the measures made for a new topic specified as argument
#' 
#' @param dat the wiki_database dataframe that should be extended
#' @param topic the topic on which we wish to recolt measures
#' @return A new dataframe containing the information for the topic argument
#' @examples add_topic(wiki_database, "Philosophy")
#' 
add_topic <- function(dat, topic) {
  simple_dat <- extract_wiki(topic, simple = T)
  standard_dat <- extract_wiki(topic, simple = F)
  
  dat <- rbind(dat,
       data.frame(
         topic = rep(topic, 2),
         source = c("wiki", "simple_wiki"),
         num_sentences = c(length(standard_dat), length(simple_dat)),
         the_freq = c(word_freq("the", standard_dat), 
                       word_freq("the", simple_dat)),
         avg_sentence_length = c(avg_sent_length(standard_dat),
                                 avg_sent_length(simple_dat)),
         avg_word_length = 
           c(mean(as.vector(sapply(standard_dat, function(sent) { avg_char_length(sent) }))),   
             mean(as.vector(sapply(simple_dat, function(sent) { avg_char_length(sent) })))),
         the_location = c(mean(locate_word("the", standard_dat)), 
                          mean(locate_word("the", simple_dat)))
       ))
  
  return(dat)
}

# We're ready to add a bunch of topics!!! We draw from the list of suggested topics from the simple
# wiki main page to ensure that the topics have been reasonably documented

topic_vector <- c("physics", "internet", "philosophy", "linguistics", "government", "buddhism",
                  "futurism", "renaissance", "art", "architecture"
                  )

for (topic in topic_vector) {
  wiki_database <- wiki_database %>% add_topic(topic)
}


#####################################################

# It's time to plot the results!
# for each variable, we create the following graph: 
# standard wiki results on x-axis, corresponding simple wiki results on y axis
# a cluster along the x = y line would mean that the measure is equal, a cluster above the
# x = y line would mean that simple wiki scores higher on that measure, and a cluster 
# below the x = y line would mean that the standard wiki scores higher


# the frequency: higher for the simple wiki in 9 out of 11 topics!!!

wiki_the_freq <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_freq = wiki_database$the_freq[which(wiki_database$source == "simple_wiki")],
    standard_freq = wiki_database$the_freq[which(wiki_database$source == "wiki")]
    )

ggplot(data = wiki_the_freq, 
       aes(x = simple_freq, y = standard_freq)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Repartition of 'the' Frequency") +
  xlab("Frequency in Simple Wiki") +
  ylab("Frequency in Standard Wiki") +
  geom_text(aes(label = topic),
            color = "gray20",
            data = subset(wiki_the_freq, topic %in% c("government", "art", "psychology")), 
            hjust = "inward", vjust = "inward") +
  geom_abline(slope = 1) +
  geom_point(aes(color = topic))

write.csv(file = "wiki_database.csv", wiki_database)

# sentence length: much longer sentences for standard wiki

wiki_sentence_length <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_sentence_length = wiki_database$avg_sentence_length[which(wiki_database$source == "simple_wiki")],
    standard_sentence_length = wiki_database$avg_sentence_length[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_sentence_length, 
       aes(x = simple_sentence_length, y = standard_sentence_length)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Repartition of Sentence Length (in Words)") +
  xlab("Sentence Length in Simple Wiki") +
  ylab("Sentence Length in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point(aes(color = topic))


# word length: extremely clear distinction: much longer words for standard wiki

wiki_word_length <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_word_length = wiki_database$avg_word_length[which(wiki_database$source == "simple_wiki")],
    standard_word_length = wiki_database$avg_word_length[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_word_length, 
       aes(x = simple_word_length, y = standard_word_length)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Repartition of word Length (in characters)") +
  xlab("Word Length in Simple Wiki") +
  ylab("Word Length in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point(aes(color = topic))

# 'the' location: no major difference

wiki_the_location <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_location = wiki_database$the_location[which(wiki_database$source == "simple_wiki")],
    standard_location = wiki_database$the_location[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_the_location, 
       aes(x = simple_location, y = standard_location)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Location of 'the' in the Sentence (1: start, 3: end)") +
  xlab("Location of 'the' in Simple Wiki") +
  ylab("Location of 'the' in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point(aes(color = topic))


acc_wiki_database <-
  wiki_database %>% group_by(source) %>% summarize(
    num_sentences = sum(num_sentences),
    the_freq = mean(the_freq),
    avg_sentence_length = mean(avg_sentence_length),
    avg_word_length = mean(avg_word_length),
    the_location = mean(the_location, na.rm = T)
  )
