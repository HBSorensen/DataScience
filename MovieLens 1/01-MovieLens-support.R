library(dplyr)
library(stringr)

zero_ratings <- edx %>% filter(rating == 0)
nrow(zero_ratings)

zero_ratings

three_ratings <- edx %>% filter(rating == 3)
nrow(three_ratings)

distinct_data <- n_distinct(edx$movieId)
nrow(distinct_movies)

distinct_movies <- edx %>% summarize(num_movies = n_distinct(movieId))
distinct_movies

distinct_users <- edx %>% summarize(num_users = n_distinct(userId))
distinct_users

drama_movies <- edx %>% filter(str_detect(genres, "Drama"))
comedy_movies <- edx %>% filter(str_detect(genres, "Comedy"))
thriller_movies <- edx %>% filter(str_detect(genres, "Thriller"))
romance_movies <- edx %>% filter(str_detect(genres, "Romance"))

nrow(drama_movies)
nrow(comedy_movies)
nrow(thriller_movies)
nrow(romance_movies )


ratings_per_movie <- edx %>% group_by(movieId) %>% summarise(count = n())
ratings_per_movie

movies <- edx
top10 <- edx %>% group_by(movieId) %>% summarise(n_ratings = n(), .groups = "drop") %>%
  arrange(desc(n_ratings)) %>% slice_head(n = 10) %>% left_join(movies, by = "movieId") %>%
  select(title, n_ratings)

top5_ratings <- edx %>%
  count(rating) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)
top5_ratings