library(readr)
library(dplyr)
library(stringr)
library(rvest)
library(ggplot2)
library(grid)
# Change the file argument to the path of the file
dat <- read_csv(file = "answers_clean.csv")

##############################################

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
the_freq <-
  dat %>% group_by(rating_category) %>% summarise(
    frequency = word_freq(word = "the", text = sentence)
  )


# Plotting of the resulting dataframe

ggplot(data = the_freq, 
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
