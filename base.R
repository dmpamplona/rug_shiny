library(dplyr)
library(ggplot2)
library(ggplot2movies)

thisyear <- 2002

movieyear <- movies %>%
  filter(
    year == thisyear
  )

ggplot(movieyear, aes(x = rating)) +
  geom_histogram()


mpaarating <- c('', 'PG-13')

moviesmpaa <- movieyear %>%
  filter(
    mpaa %in% mpaarating
  )

ggplot(moviesmpaa, aes(x = length, y = rating, color = mpaa)) +
  geom_smooth()

minrating <- 9.5

movielist <- moviesmpaa %>%
  filter(
    rating >= minrating
  )

head(movielist)