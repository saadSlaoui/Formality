library(readr)
library(dplyr)
library(stringr)
library(rvest)
library(ggplot2)
library(grid)
library(RCurl)
library(outliers)
library(mice)

dat <- read_csv("answers_clean.csv")


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
