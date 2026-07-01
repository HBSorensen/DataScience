# File        : 00-MovieLens.R
# Autor       : EDX + Henrik B. Sørensen
# Date        : 2026-07-01
# Description : Condensed R code for the MovieLens project assignment.
#               Own changes have been added to the example file where needed.


####################################################################################################################
# Create edx and final_holdout_test sets 
####################################################################################################################

# Note: this process could take a couple of minutes

####################################################################################################################
# Install necessary libraries if not already present.
# Additional libraries (besides the example) are loaded.
####################################################################################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

####################################################################################################################
# Load the libraries to session
####################################################################################################################
library(tidyverse)
library(tidyverse)
library(caret)
library(dplyr)
library(stringr)
library(ggplot2)
library(caret)

####################################################################################################################
# MovieLens 10M dataset are loaded from:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
####################################################################################################################

# Set timeout
options(timeout = 120)

####################################################################################################################
# Download data set and uncompress files
####################################################################################################################
# Download zipped data set if missing
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Uncompress ratings.dat file from downloaded zip file
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

# Uncompress movies.dat file from downloaded zip file
movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

####################################################################################################################
# Load data from files into data.frames
####################################################################################################################
# Load ratings from ratings.dat file
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)

# Name the columns
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

# Convert the columns from test into integers (3 columns) and floating point (1 column)
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# Load movies from movies.dat file
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

# Name the columns
colnames(movies) <- c("movieId", "title", "genres")

# Convert single column from text to integer
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

# Create the movielens data set joining the ratings table making the result larger.
# Be aware, that there'll be multiple instances of the same movie in the result.
movielens <- left_join(ratings, movies, by = "movieId")


####################################################################################################################
# Splitting the MovieLens dataset into training set and validation set. 
# Training set being the 90% part of the data set
####################################################################################################################
# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
# Create the split, by retrieving the indexes of the 10% partition
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

# Take all entries 
# edx will contain 90% of the data (data not indexed by the test_index list)
edx <- movielens[-test_index,]
# temp will contain 10% of the data (data indexed by the test_index list)
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
# By using semi_join, we check that both moveId and userId for any entry are present
# in the resulting data set.
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
# Find all entries not present in final_holdout_test but present in temp
removed <- anti_join(temp, final_holdout_test)
# Add the found entries back into edx data set
edx <- rbind(edx, removed)

# Get an overview of the data sizes
cat("Movies rows    : ", format(nrow(movies), big.mark=","), "\n")
cat("Ratings rows   : ", format(nrow(ratings), big.mark=","), "\n")
cat("MovieLens rows : ", format(nrow(movielens), big.mark=","), "\n")

# Clean up temporary variables to free up memory
rm(dl, ratings, movies, test_index, temp, movielens, removed)

####################################################################################################################
# Initial data investigation
####################################################################################################################
# Initial overview of the edx data set
# Count the data entries:
cat("edx rows    :", nrow(edx), "\n")
cat("edx columns :", ncol(edx), "\n")
cat("Unique users :", n_distinct(edx$userId),  "\n")
cat("Unique movies:", n_distinct(edx$movieId), "\n")

# Show the top 10 entries of the data set 
edx_display <- head(edx, n=10)
edx_display$genres <- gsub("\\|", ", ", edx_display$genres)
edx_display

####################################################################################################################
# Data visualization
# We visualize the data in order to be able to show trends in the ratings
####################################################################################################################
# Show distribution of ratings
edx %>%
  ggplot(aes(x = rating)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  labs(title = "Rating Distribution", x = "Rating", y = "Number of Ratings")

# Show ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Number of Ratings per Movie",
       x = "Number of Ratings", y = "Number of Movies")

# Show ratings per users
edx %>%
  count(userId) %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Number of Ratings per User", x = "Number of Ratings", y = "Number of Users")

#Show average rating per movie
edx %>%
  group_by(movieId) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(x = avg)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Average Rating per Movie",
       x = "Average Rating", y = "Number of Movies")

# Show average rating per user
edx %>%
  group_by(userId) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(x = avg)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Average Rating per User",
       x = "Average Rating", y = "Number of Users")

# Show average rating per Genre
edx %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(avg_rating = mean(rating), .groups = "drop") %>%
  arrange(genres) %>%
  ggplot(aes(x = genres, y = avg_rating)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Rating by Genre", x = "Genre", y = "Average Rating")

####################################################################################################################
# Model development
####################################################################################################################
# Splitting the training set (edx) further into a training and validation set.
set.seed(2, sample.kind = "Rounding")
val_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-val_index, ]
val_set   <- edx[val_index, ]

# Ensure val_set only contains movies and users seen in train_set
val_set <- val_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Define the RMSE function
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Set our target:
target_rmse <- 0.86490
####################################################################################################################
# Model 1 — Baseline: Overall Mean
####################################################################################################################
# Take the average of the ratings
mu <- mean(train_set$rating)
cat("Overall mean rating:", mu, "\n")

# Calculate the RMSE of these
rmse_m1 <- RMSE(val_set$rating, mu)
cat("Model 1 RMSE (baseline):", rmse_m1, "\n")

# Test towards the target values
if (rmse_m1 > target_rmse)
  cat ("Model 1 RMSE does not meet target")
if (rmse_m1 <= target_rmse)
  cat ("Model 1 RMSE meets target")

####################################################################################################################
# Model 2 — Regularized Movie Bias
# Now, we regularize the movies in order to compensate for the distribution of ratings per movie
####################################################################################################################
lambdas <- seq(0, 10, by = 0.25)

# Calculation of the RMSEs for different lambda values
rmse_m2_vec <- sapply(lambdas, function(lambda) {
  # Calculate the adjustment values in order to regularize the movie ratings per movie
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + lambda), .groups = "drop")
  
  # Calculate the predicted rating using the bias
  pred <- val_set %>%
    left_join(b_i, by = "movieId") %>%
    mutate(pred = pmin(pmax(mu + b_i, 0.5), 5)) %>%
    pull(pred)
  
  RMSE(val_set$rating, pred)
})

# Plot the Lambda vs. RMSE
qplot(lambdas, rmse_m2_vec, geom = "line",
      xlab = "Lambda", ylab = "RMSE",
      main = "Model 2: RMSE vs Lambda (movie bias)")

# Print out the lambda value which gives the smallest RMSE and the value itself
lambda_m2 <- lambdas[which.min(rmse_m2_vec)]
rmse_m2   <- min(rmse_m2_vec)
cat("Model 2 best lambda:", lambda_m2, "\n")
cat("Model 2 RMSE (movie bias):", rmse_m2, "\n")

# Test towards the target values
if (rmse_m2 > target_rmse)
  cat ("Model 2 RMSE does not meet target")
if (rmse_m2 <= target_rmse)
  cat ("Model 2 RMSE meets target")

####################################################################################################################
# Model 3 — Regularized Movie Bias + User Bias
# Last but not least, we add the User Bias in the model
####################################################################################################################
# Calculation of the RMSEs for different lambda values (reusing the lambdas vector previously created)
rmse_m3_vec <- sapply(lambdas, function(lambda) {
  # Calculate the adjustment values in order to regularize the movie ratings per movie
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + lambda), .groups = "drop")
  
  # Calculate the adjustment values in other to regularize the user ratings  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i) / (n() + lambda), .groups = "drop")
  
  # Calculate the predicted rating using the biases
  pred <- val_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(
      b_i  = ifelse(is.na(b_i), 0, b_i),
      b_u  = ifelse(is.na(b_u), 0, b_u),
      pred = pmin(pmax(mu + b_i + b_u, 0.5), 5)
    ) %>%
    pull(pred)
  
  RMSE(val_set$rating, pred)
})

qplot(lambdas, rmse_m3_vec, geom = "line",
      xlab = "Lambda", ylab = "RMSE",
      main = "Model 3: RMSE vs Lambda (movie + user bias)")

# Print out the lambda value which gives the smallest RMSE and the value itself
lambda_m3 <- lambdas[which.min(rmse_m3_vec)]
rmse_m3   <- min(rmse_m3_vec)
cat("Model 3 best lambda:", lambda_m3, "\n")
cat("Model 3 RMSE (movie + user bias):", rmse_m3, "\n")

# Test towards the target values
if (rmse_m3 > target_rmse)
  cat ("Model 3 RMSE does not meet target")
if (rmse_m3 <= target_rmse)
  cat ("Model 3 RMSE meets target")

####################################################################################################################
# Final Evaluation on final_holdout_test
####################################################################################################################
# Refit overall mean on full edx data set
mu_final <- mean(edx$rating)

# Refit regularized movie bias on full edx data set
b_i_final <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_final) / (n() + lambda_m3), .groups = "drop")

# Refit regularized user bias on full edx (conditioned on movie bias)
b_u_final <- edx %>%
  left_join(b_i_final, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_final - b_i) / (n() + lambda_m3), .groups = "drop")

# Predict on final_holdout_test
pred_final <- final_holdout_test %>%
  left_join(b_i_final, by = "movieId") %>%
  left_join(b_u_final, by = "userId") %>%
  mutate(
    b_i  = ifelse(is.na(b_i), 0, b_i),
    b_u  = ifelse(is.na(b_u), 0, b_u),
    pred = pmin(pmax(mu_final + b_i + b_u, 0.5), 5)
  ) %>%
  pull(pred)

# Print out the RMSE when testing against the final_holdout_test data set
rmse_final <- RMSE(final_holdout_test$rating, pred_final)
cat("Final RMSE on final_holdout_test:", rmse_final, "\n")

# Test towards the target values
if (rmse_final > target_rmse)
  cat ("Model 3 RMSE does not meet target on the final_holdout_test data set")
if (rmse_final <= target_rmse)
  cat ("Model 3 RMSE meets target on the final_holdout_test data set")


