library(readr)
library(dplyr)
library(stringr)
library(rvest)

# Change the file argument to the path of the file
dat <- read_file(file = "answers")

# This code assumes that the data is in the form
# ratings  \t  rating1, rating2, rating3, rating4, rating5  <obscure chain of numbers> sentence \n
# And it reduces it to a clean csv file of the form visible below

dat.vectors <- (dat %>% str_split("\\\n"))[[1]]
dat.vectors <- dat.vectors %>% str_split("\\\t")
dat.vectors <- lapply(dat.vectors, function (vec) { vec <- vec[-3] })
dat.cleaned <- lapply(dat.vectors, function (vec) { vec <- c(vec[1], str_split(vec[2], ",")[[1]], vec[3])})

dat.dataframe <-
  data.frame(avg_rating = sapply(dat.cleaned, function (vec) { vec <- vec[1] }),
             rating_1   = sapply(dat.cleaned, function (vec) { vec <- vec[2] }),
             rating_2   = sapply(dat.cleaned, function (vec) { vec <- vec[3] }),
             rating_3   = sapply(dat.cleaned, function (vec) { vec <- vec[4] }),
             rating_4   = sapply(dat.cleaned, function (vec) { vec <- vec[5] }),
             rating_5   = sapply(dat.cleaned, function (vec) { vec <- vec[6] }),
             sentence   = sapply(dat.cleaned, function (vec) { vec <- vec[7] })
            )

dat.dataframe <- dat.dataframe %>% filter(!is.na(sentence))

# change the path to a name corresponding to the file
write_csv(path = "answers_clean.csv", dat.dataframe)
