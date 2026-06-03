# Filename     : 00-MovieLens1.R
# Author       : Henrik B. Sørensen
# Created Date : 2026-06-03
# Description  : R code for MovieLens 1 project for Data Science: Capstone, HarvardX PH125.9x
#                Basis for Rmd file and based on code example provided
#                Code can be downloaded using https://github.com/HBSorensen/DataScience.git and is located inside
#                'MovieLens 1/Final Report and Support Files' folder

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

##########################################################
# Install required libraries if missing and load them
##########################################################
# Note: this process could take a couple of minutes

# Check for presence of tidyverse library and download/install if missing
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# Check for presence of caret library and download/install if missing
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")

# Check for presence of dplyr library and download/install if missing
if(!require(dplyr)) 
  install.packages("dplyr", repos = "http://cran.us.r-project.org")

# Check for presence of stringr library and download/install if missing
if(!require(stringr)) 
  install.packages("stringr", repos = "http://cran.us.r-project.org")

# Load the libraries for use in this session
library(tidyverse)
library(caret)
library(dplyr)
library(stringr)


##########################################################
# Download, unpack and load dataset files
# In case of data already has been downloaded and 
#  uncompressed, the existing files are loaded. 
##########################################################
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
# Set default timeout to 2 minutes
options(timeout = 120)

# Download main data file (zipped)
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Extract ratings data file if not already done.
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

# Extract movies data file if not already done.
movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

# Extract tags data file if not already done.
tags_file <- "ml-10M100K/tags.dat"
if(!file.exists(tags_file))
  unzip(dl, tags_file)


##########################################################
# Convert the data files into frames
##########################################################
# Load ratings from ratings file and convert into frame
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
# Set up column names to userId, movieId, rating, timestamp
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
# Change the data types from text to numbers where needed
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# Load ratings from movies file and convert into frame
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE), 
                        stringsAsFactors = FALSE)
# Set up column names to movieId, title, genres
colnames(movies) <- c("movieId", "title", "genres")
# Change the movieId from text to number
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

# Connect (join) the movies and ratings by the movieId
movielens <- left_join(ratings, movies, by = "movieId")


##########################################################
# Split the data sets up for training and testing
##########################################################
# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

# Cleanup temporary tables/frames to release memory
rm(dl, ratings, movies, test_index, temp, movielens, removed)


##########################################################
# Initial analysis of the data
##########################################################
# Extract list of genres
movie_genres <- unique(unlist(strsplit(edx$genres, "\\|")))
movie_genres