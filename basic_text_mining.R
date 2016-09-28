library(readr)
library(dplyr)
library(stringr)
library(rvest)
library(ggplot2)
library(grid)
library(RCurl)
library(outliers)
library(mice)
library(corrplot)

dat <- read_csv("answers_v2.csv")

####################################################


# Frequency measure of "the" per sentence:
# We create a new variable the following way: we collapse the avg_rating variable
# by rounding every value to the nearest int. Each sentence then belongs to one of
# 7 categories: -3 (least formal) to 3 (most formal).
# We thus have a meaningful body of sentences linked to one another by formality only, upon which
# we can measure the frequency of appearances of the word "the"

# Creation of the feature engineered grouping variable
dat <- cbind(dat$avg_rating, round(dat$avg_rating), dat[, -1])
colnames(dat)[1:2] <- c("avg_rating", "rating_category")

# Computation of the frequency of "the" per formality group: number of appearances divided
# by number of words of collapsed sentence string 

#' Returns the frequency of appearances of a word in a text. Note: assumes all words are
#' separated by a whitespace; can be optimized
#' 
#' @param word a word string
#' @param text a text string
#' @return The number of appearances of the word divided by the numer of words in the text
#' @examples
#' word_freq("the", "This is a sentence with 2 mentions of the. The uppercases are put in lowercase")
word_freq <- function(word, text) {
  text <- text %>% tolower() %>% paste(collapse = " ")
  
  word.count <- length(str_locate_all(string = text, pattern = word)[[1]]) / 2
  text.count <- length(str_split(text, pattern = " ")[[1]])
  return(word.count / text.count)
}

# We now have a basic "percentage" measure of the appearances of "the" in any given sentence
# Let us compute this measure for all seven formality categories
the_freq_vs_formality <-
  dat %>% group_by(rating_category) %>% summarise(
    frequency = word_freq(word = "the", text = sentence)
  )


# Plotting of the resulting dataframe

ggplot(data = the_freq_vs_formality, 
       aes(x = rating_category, y = frequency)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Frequency of Appearance of 'the' vs Formality Level") +
  xlab("Formality Level (-3: most informal; 3: most formal)") +
  ylab("Frequency of 'the'") +
  geom_smooth(color = "red", span = 1.2) +
  geom_point(size = 1, shape = 7)

grid.text("r = 0.94, p < 0.002",
          x = .70, y = .20,
          just = "left",
          draw = TRUE)

write_csv(x = dat, path = "answers_v2.csv")


####################################################


# We reiterate the process, this time linking "the" with the average word length
# of the sentence surrounding it. 
# In order to make the data more telling, we shall once again categorize sentences 
# by average word length (in characters)

# First step: writing a function that determines the average word length of a sentence,
# essential to feature engineering the avg_word_length variable

#' Returns the average character length of the words in a given sentence
#' Assumes that words are always separated by whitespaces; can be optimized
#' 
#' @param sentence a sentence string
#' @return The averaged character length of all words in the sentence
#' @examples
#' avg_char_length("This phrase has a short avg word length")
#' avg_char_length("The subsequent sentence possesses quite elaborate expressions")
avg_char_length <- function(sentence) { 
  word.vector <- (sentence %>% str_trim() %>% str_split(pattern = " "))[[1]]
  total.char <- sum(nchar(word.vector))
  
  return(total.char / length(word.vector))
}

# We can now associate the average word length to each sentence
dat$avg_word_length <- sapply(dat$sentence, function(sent) { avg_char_length(sent) })

# In order to remain consistent with the first divide in formality measure, we group the 
# average word lengths of the sentences into 7 equal parts

# It is also safe to ignore outliers - anything with an extraordinary departure from the 
# average is probably caused by very poor typing of some chain of characters
# We choose to ignore the elements lying on the 1% extreme of a normalized distribution
# of average word lengths
word_avg_dat = dat$avg_word_length[-which(scores(dat$avg_word_length, prob = 0.99))]

# Now that we are dealing with a reduced vector, it is preferable to generate a word length
# dataframe to support the visualisation. We add to this dataframe the necessary rows from the main
# one, corresponding to the rows we have not ruled out as outliers

index <- which(dat$avg_word_length %in% 
                 dat$avg_word_length[-which(scores(dat$avg_word_length, prob = 0.99))])


word_length <- 
  data.frame(
    sentence = dat$sentence[index],
    avg_word_length = word_avg_dat,
    category = ntile(word_avg_dat, 7)
  )

# We are still interested in the actual values corresponding to the 7 categories

the_freq_vs_word_length <-
  word_length %>% group_by(category) %>% summarise(
    avg_length = mean(avg_word_length),
    frequency = word_freq(word = "the", text = sentence)
  )

# We are now ready to plot average word length vs frequency of the

ggplot(data = the_freq_vs_word_length, 
       aes(x = avg_length, y = frequency)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Frequency of Appearance of 'the' vs Sentence Average Word Length") +
  xlab("Average Word Length (in characters)") +
  ylab("Frequency of 'the'") +
  geom_smooth(color = "green", span = 1.2) +
  geom_point(size = 1, shape = 7)

grid.text("r = 0.86, p < 0.0012",
          x = .70, y = .20,
          just = "left",
          draw = TRUE)

# A natural extension of this exercise is to verify to what extent word length 
# and formality are correlated - intuition: a lot
formality_vs_word_length <-
  dat[index, ] %>% group_by(rating_category) %>% summarize(
    avg_length = mean(avg_word_length)
  )

ggplot(data = formality_vs_word_length, 
       aes(x = avg_length, y = rating_category)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Formality Level vs Sentence Average Word Length") +
  xlab("Average Word Length (in characters)") +
  ylab("Formality Level (-3: most informal; 3: most formal)") +
  geom_smooth(color = "blue", span = 1.2) +
  geom_point(size = 1, shape = 7)

grid.text("r = 0.96, p < 1e-3",
          x = .70, y = .20,
          just = "left",
          draw = TRUE)

# The assumption couldn't have been more true: r > 0.96, p < 1e-3. The two measures are 
# basically equivalent.

####################################################


# Another point of interest would be where in the sentence does "the" appear, depending on the 
# formality of the sentence. We re-use our data clusters by formality and within each of them
# filter the sentences which contain "the"
# We then compute the percentage of appearances of "the" in 3 categories: beginning, middle or end of
# the sentence, by dividing the sentence into 3 quantiles

#' Returns the location of an input word in an input sentence containing it
#' Assumes that the sentence contains the word. Can be optimized.
#' 
#' @param word the word we are looking for
#' @param sentence a sentence string containing "the"
#' @return The location of the first appearance of word in the sentence 
#'         (1 = start, 2 = middle, 3 = end)
#' @examples
#' locate_word("the", "The fox is red)
locate_word <- function(word, sentence) {
  sentence <- sentence %>% tolower() %>% 
                gsub(pattern = "[?!.;,#:']", replacement = "")
  location <- str_locate(sentence, word)[1]
  relative_location <- location / nchar(sentence)
  
  return(ifelse(relative_location < .33, 1,
                ifelse(relative_location < .66, 2, 3)))
}

# We are ready to map every sentence with an occuring the to its corresponding location
index <- which(str_detect(tolower(dat$sentence), pattern = "the"))

the_location <- 
  data.frame(sentence = dat$sentence[index],
             avg_rating = dat$avg_rating[index],
             rating_category = dat$rating_category[index],
             location = sapply(dat$sentence[index], 
                               function(sent) { locate_word("the", sentence = sent)})
            )  
row.names(the_location) = NULL

# collapsing the location data into formality clusters, this time with more values:
# we round the average formality rating to the first decimal place

grouped_the_location <- 
  the_location %>% group_by(avg_rating) %>% summarize(
    avg_location = mean(location)
  )

# We also create a dataframe using the recurrent 7-parts discrete spectrum,
# in order to later develop a correlation matrix

standard_the_location <-
  the_location %>% group_by(rating_category) %>% summarize(
    avg_location = mean(location)
  )

ggplot(data = grouped_the_location, 
       aes(x = avg_rating, y = avg_location)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Location of 'the' in Sentence vs Average Formality Rating") +
  xlab("Formality Level (-3: most informal; 3: most formal)") +
  ylab("Location of 'the' (1 = start, 2 = middle, 3 = end)") +
  geom_smooth(color = "orange", span = 1.5) +
  geom_point(size = 1, shape = 7)

grid.text("r = -0.93, p < 1.7e-5",
          x = .15, y = .20,
          just = "left",
          draw = TRUE)

##################################################

# Let us look at the averaged sentence length word-wise over the formality spectrum

# Same process as always: define a function, run it

#' Returns the average sentence length of a given vector
#' Assumes that words are always separated by whitespaces; can be optimized
#' 
#' @param sentences a vector of sentences
#' @return The averaged number of words over all sentences
#' @examples
#' avg_sent_length(c("Very short", "The sentences are short"))
avg_sent_length <- function(sentences) { 
  word_count <- sum(
    as.vector(sapply(sentences, function (sentence) { 
      sentence <- length(str_split(sentence, pattern = " ")[[1]]) }
    )))
  
  return(word_count / length(sentences))
}

# Formality and sentence length
sent_length_vs_formality <- dat %>% group_by(rating_category) %>% summarize(
  sentence_length = avg_sent_length(sentence)
)

# The frequency and sentence length:
# we measure the length of every individual sentence in the sample and divide them into
# 7 categories according to their length

dat$sent_length_category <- 
  ntile(as.vector(sapply(dat$sentence, function (x) { avg_sent_length(x) })), 7)

sent_length_vs_the_freq <- dat %>% group_by(sent_length_category) %>% summarize(
  sentence_length = avg_sent_length(sentence),
  the_freq = word_freq("the", sentence)
)

# We are ready to plot the correlations:

# Sentence length vs formality
ggplot(data = sent_length_vs_formality, 
       aes(x = rating_category, y = sentence_length)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Average Sentence length vs Formality Level") +
  xlab("Formality Level (-3: most informal; 3: most formal)") +
  ylab("Average Length of the Sentence (in Words)") +
  geom_smooth(color = "purple", span = 1.5) +
  geom_point(size = 1, shape = 7)

grid.text("r = 0.95, p < 1.e-4",
          x = .15, y = .80,
          just = "left",
          draw = TRUE)

# Sentence Length vs the frequency (percentage)
ggplot(data = sent_length_vs_the_freq, 
       aes(x = sentence_length, y = the_freq)) +
  theme_light(base_size = 9) +
  ggtitle(label = "Frequency of 'the' vs Average Sentence Length") +
  xlab("Average Sentence Length (in Words)") +
  ylab("Frequency of 'the'") +
  geom_smooth(color = "cyan", span = 3) +
  geom_point(size = 1, shape = 7)

grid.text("r = 0.89, p < 0.007",
          x = .65, y = .20,
          just = "left",
          draw = TRUE)


####################################################


# We create a correlation plot which displays the correlation between each of the variables we've
# analyzed so far with the average formality rating
# We form a 4*4 matrix which brings together the 4 measures we've been dealing with so far:
# formality level, frequency of the, location of the, average word length
merged <- merge(merge(the_freq, merge(formality_vs_word_length, sent_length_vs_formality)), standard_the_location)
colnames(merged) <- c("formality", "the_frequency", "word_length", "sentence_length", "the_location")
correlation_matrix <- cor(merged)
corrplot.mixed(correlation_matrix)
