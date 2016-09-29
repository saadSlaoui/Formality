###########################################################

# Let's accumulate a lot of sentences from identical topics in both simple wiki and standard
# wiki. We use the table of suggestions in the simple wiki main page, since every topic
# listed on there should have a reasonable coverage within the simple wiki website

# Extract the table containing every single topic suggestion in the simple wiki main page
simple_html <- read_html("https://simple.wikipedia.org/wiki/Main_Page") 
simple_table <- simple_html %>% html_nodes("table") %>% .[[9]] %>% html_table(fill = T) 

# clean the table and concatenate every topic into one vector
simple_vector = c()
for (i in 1:14) {
  col = simple_table[, i]
  simple_vector <- c(simple_vector, col)
}

simple_vector <- simple_vector[-which(is.na(simple_vector))] 
simple_vector <- simple_vector[-which(as.vector(sapply(simple_vector, function(x) { x == ""})))]
simple_vector <- simple_vector[1:4] %>% str_split(pattern = "•")

# bring all the cleaned up values together in one vector
simple_vector <- c(simple_vector[[1]], simple_vector[[2]], simple_vector[[3]], simple_vector[[4]])
simple_vector <- simple_vector %>% str_trim()
# remove exceedingly long topic names
simple_vector <- simple_vector[-which(as.vector(sapply(simple_vector, function(x) { nchar(x) > 18})))]
simple_vector[24] <- "Movies"

# format 2-word topic names by inserting underscore
simple_vector <- gsub(simple_vector, pattern = " ", replacement = "_")


#################################################


# Time to run the add_topic function on every single topic in simple_vector!

# Reinitialize the wiki database
wiki_database <- 
  data.frame(topic = NA,
             source = NA,
             num_sentences = NA,
             the_freq = NA,
             avg_sentence_length = NA,
             avg_word_length = NA,
             the_location = NA
  )

# Run add_topic on the 59 topics in normal_vector
for (topic in simple_vector) {
  wiki_database <- add_topic(wiki_database, topic)
}
wiki_database <- wiki_database[-1, ]
row.names(wiki_database) <- 1:118

# suppress topics where the corresponding article has less than 15 sentences (value can be modified)
index <- which(wiki_database$num_sentences < 15)
wiki_database <- wiki_database[-index, ]
# careful: # must suppress associated article every time
singleton_topics <-
  wiki_database %>% group_by(topic) %>% summarize(
    num_appearances = length(num_sentences)
  )

singleton_topics <- singleton_topics$topic[which(singleton_topics$num_appearances == 1)]
index <- which(wiki_database$topic %in% singleton_topics)
wiki_database <- wiki_database[-index, ]
row.names(wiki_database) <- 1:nrow(wiki_database)

# We end up with 44 topics whose simple wiki and standard wiki were both analyzed for the frequency,
# word length, sentence length and the location.

write_csv(path = "wiki_database_V2.csv", wiki_database)

# Main result: concatenate every sentence of every topic, grouped by source

acc_wiki_database_V2 <-
  wiki_database %>% group_by(source) %>% summarize(
    num_sentences = sum(num_sentences),
    the_freq = mean(the_freq),
    avg_sentence_length = mean(avg_sentence_length),
    avg_word_length = mean(avg_word_length),
    the_location = mean(the_location, na.rm = T)
  )


##################################################


# graphs

# the frequency: higher for the simple wiki in 9 out of 11 topics!!!

wiki_the_freq_V2 <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_freq = wiki_database$the_freq[which(wiki_database$source == "simple_wiki")],
    standard_freq = wiki_database$the_freq[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_the_freq_V2, 
       aes(x = simple_freq, y = standard_freq)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Repartition of 'the' Frequency") +
  xlab("Frequency in Simple Wiki") +
  ylab("Frequency in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point()


# sentence length: much longer sentences for standard wiki

wiki_sentence_length_V2 <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_sentence_length = wiki_database$avg_sentence_length[which(wiki_database$source == "simple_wiki")],
    standard_sentence_length = wiki_database$avg_sentence_length[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_sentence_length_V2, 
       aes(x = simple_sentence_length, y = standard_sentence_length)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Repartition of Sentence Length (in Words)") +
  xlab("Sentence Length in Simple Wiki") +
  ylab("Sentence Length in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point()


# word length: extremely clear distinction: much longer words for standard wiki

wiki_word_length_V2 <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_word_length = wiki_database$avg_word_length[which(wiki_database$source == "simple_wiki")],
    standard_word_length = wiki_database$avg_word_length[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_word_length_V2, 
       aes(x = simple_word_length, y = standard_word_length)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Repartition of word Length (in characters)") +
  xlab("Word Length in Simple Wiki") +
  ylab("Word Length in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point()

# 'the' location: no major difference

wiki_the_location_V2 <-
  data.frame(
    topic = unique(wiki_database$topic),
    simple_location = wiki_database$the_location[which(wiki_database$source == "simple_wiki")],
    standard_location = wiki_database$the_location[which(wiki_database$source == "wiki")]
  )

ggplot(data = wiki_the_location_V2, 
       aes(x = simple_location, y = standard_location)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Location of 'the' in the Sentence (1: start, 3: end)") +
  xlab("Location of 'the' in Simple Wiki") +
  ylab("Location of 'the' in Standard Wiki") +
  geom_abline(slope = 1) +
  geom_point()
