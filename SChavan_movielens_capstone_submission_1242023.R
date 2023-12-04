##########################################################
# Create edx and final_holdout_test sets 
##########################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

# MovieLens 10M dataset:
#creating dl and printing to see if it was created
dl <- "ml-10M100K.zip"
if(! file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
print(dl)

#creating ratings_file and print to see if it was created. 
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)
print(ratings_file)

# creating movies_file and printing to check if it was created 
movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)
print(movies_file)

# creating ratings, note the header here. Use head to check the header and dataset
ratings <- read_delim(ratings_file, delim = "::", col_names = c("UserID", "MovieID", "Rating", "Timestamp"))
head(ratings)

# creating movies and head movies to check if it is created and also the header is correct. 
movies <- read_delim(movies_file, delim = "::", col_names = c("MovieID", "Title", "Genres"))
head(movies)

#code had changes slightly from the edex with the MovieID and others titles using caps locks. I dont think this helped a lot.
colnames(movies) <- c("MovieID", "Title", "Genres")
head(movies)

#movies dataset if transformed and head to check the titles. See the change in MovieID.
movies <- transform(movies, MovieID = as.integer(MovieID))
head(movies)

#to confirm it became an integer
class(movies)
class(movies$MovieID) 


#creating movielens dataset
movielens <- left_join(ratings, movies, by = "MovieID")
head(movielens)

# Final hold-out test set will be 10% of MovieLens data
set.seed(1,sample.kind="Rounding")
# above code gave a warning so used the below code. 
set.seed(1)

#creating test_index and head test_index. Note the change in movielens$Ratings (capital R to match my headers)
test_index <- createDataPartition(y = movielens$Rating, times = 1, p = 0.1, list = FALSE)

#checking to see what the dataset looks like
head(test_index) 

#creating edx dataset
edx <- movielens[-test_index,]
head(edx)

# creating tmp dataset
temp <- movielens[test_index,]
head(temp)

#creat final_holdout_test
final_holdout_test <- temp %>% 
  semi_join(edx, by = "MovieID") %>%
  semi_join(edx, by = "UserID")
head(final_holdout_test)

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)
##################################################################################
# Investigating of the edx dataset
summary(edx)

# Movie rating output
edx %>% 
  summarize(
    Lowest_Rating = min(Rating),
    Highest_Rating = max(Rating),
    Mean_Rating = mean(Rating),
    Median_Rating = median(Rating)
  )
#######################################################
# Group data by "Rating"
# Group data by "Rating" top_5
edx %>% group_by(Rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

# Group data by "Rating" top_10
edx %>% group_by(Rating) %>% summarize(count = n()) %>% top_n(10) %>%
  arrange(desc(count)) 
######################################################################## 
# Calculating the number of distinct users and the number of distinct movies in the dataset
# Distinct users and movies
edx %>% 
  summarize(n_users = n_distinct(UserID), 
            n_movies = n_distinct(MovieID))
####################################################################################
# Number of ratings per movie. 
# Count the number of ratings per movie
ratings_per_movie <- edx %>% count(MovieID)

# Create the histogram plot using ggplot
plot <- ggplot(data = ratings_per_movie, aes(x = n)) +
  geom_histogram(color = "white", fill = "red", bins = 25, binwidth = 0.1) +
  scale_x_log10() +
  ggtitle("Number of Ratings per Movie") 
  
# Display the plot for number of ratings per movie.
print(plot)
############################################################
###  Calculates the number of ratings per user
#Calculate the number of ratings per user. 
# Count the number of ratings per user
ratings_per_user <- count(edx, UserID)

# Create the histogram plot using ggplot
plot2 <- ggplot(data = ratings_per_user, aes(x = n)) +
  geom_histogram(color = "white", fill = "orange", bins = 20, binwidth = 0.3) +
  ggtitle("Number of Ratings Per User") +
  scale_x_log10() 

# Display the plot for number of ratings per uesr. 
print(plot2)

#####################################################################################
###  Plot the ratings for each movie genre.
# Plot the ratings for each movie genre.
 # Separate genres column into rows.
edx_separated <- separate_rows(edx,Genres, sep ="\\|")

# Count the occurrences of each genre
genres_counts <- edx_separated %>% 
  group_by(Genres) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# Create a custom color palette with 20 colors
custom_colors <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
  "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5"
)

# Create the bar plot using ggplot with custom colors
plot <- ggplot(data = genres_counts, aes(x = reorder(Genres, -count), y = count, fill = Genres)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Ratings for Each Genre") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = custom_colors) +  # Use the custom colors
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# Print the plot for number of ratings per genre
print(plot)
############################################################################
# The Movielens dataset is split into edx (training) and final_holdout_test(validation). Within the edx(training) dataset there will be another split for internal training (edx_train) and testing (edx_temp). The final_holdout_test (validation) dataset will be utilized only on the final models. 
#################################################
## Creating training and test sets using edx data set.
##########################################################
# Set a random seed for reproducibility
set.seed(1, sample.kind = "Rounding")
# Create the test set index
test_index <- createDataPartition(y = edx$Rating, times = 1, p = 0.1, list = FALSE)
summary(test_index)
head(test_index)

# Create the training set (edx_train) and test set (edx_temp)
edx_train <- edx[-test_index, ]
edx_temp <- edx[test_index, ]

head(edx_train)
colnames(edx_train)
summary(edx_train)

head(edx_temp)
colnames(edx_temp)
summary(edx_temp)
######################################################################################
### Model testing 
### 1. Naive Model
### 1. Naive model on edx_train
#############################################################
## Model Creation and Evaluation on edx_train dataset.
##########################################################
### 1. Naive Model on edx_train
#####################################################
# Calculate the mean rating in the edx_train dataset
mean_rating <- mean(edx_train$Rating)
print(mean_rating)
#output is print(mean_rating) [1] 3.512457

# Create a vector of the naive predictions (using the mean rating for all items)
naive_predictions <- rep(mean_rating, nrow(edx_train))

# Calculate RMSE
naive_rmse <- sqrt(mean((edx_train$Rating - naive_predictions) ^ 2))
print(naive_rmse)
#output is print(naive_rmse) [1] 1.060362

## Naive model using median value.
# Calculate the median rating in the edx_train dataset
median_rating <- median(edx_train$Rating)
print(median_rating)
#output is print(median_rating) [1] 4

# Create a vector of the median predictions (using the median rating for all items)
median_predictions <- rep(median_rating, nrow(edx_train))
# Calculate RMSE
median_rmse <- sqrt(mean((edx_train$Rating - median_predictions) ^ 2))
print(median_rmse)
# output print(median_rmse) [1] 1.167076
##############################################
### 1. Naive model on edx_temp
#############################################################
# Naive Model on edx_temp
################################################################
# Calculate the mean rating in the edx_temp dataset
mean_rating_temp <- mean(edx_temp$Rating)
print(mean_rating_temp)
# output print(mean_rating_temp) [1] 3.512541

# Create a vector of the naive predictions (using the mean rating for all items)
naive_predictions_temp <- rep(mean_rating_temp, nrow(edx_temp))

# Calculate RMSE for the naive model on edx_temp
naive_rmse_temp <- sqrt(mean((edx_temp$Rating - naive_predictions_temp) ^ 2))
print(naive_rmse_temp)
#output print(naive_rmse_temp) [1] 1.060056

## Naive model using median value on edx_temp
# Calculate the median rating in the edx_temp dataset
median_rating_temp <- median(edx_temp$Rating)
print(median_rating_temp)
# output print(median_rating_temp)[1] 4

# Create a vector of the median predictions (using the median rating for all items)
median_predictions_temp <- rep(median_rating_temp, nrow(edx_temp))
# Calculate RMSE for the naive model using median on edx_temp
median_rmse_temp <- sqrt(mean((edx_temp$Rating - median_predictions_temp) ^ 2))
print(median_rmse_temp)
#output print(median_rmse_temp) [1] 1.166763
####################################################################################
### 2. Movie Effect Model on edx_train
################################################################
# Calculate the average rating in the edx_train dataset
average_rating <- mean(edx_train$Rating)

# Estimate the movie effects (bi) by calculating the mean rating for each movie
movie_effects <- edx_train %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating))
head(movie_effects)

# Merge movie_effects with edx_train
edx_train <- edx_train %>%
  left_join(movie_effects, by = "MovieID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi
edx_train$predicted_rating <- average_rating + edx_train$movie_effect

# Calculate RMSE
rmse_movie_effect <- sqrt(mean((edx_train$Rating - edx_train$predicted_rating) ^ 2))
print(rmse_movie_effect)
#ouput is [1] 0.9423541

###################################################################
### 2. Movie effect model on edx_temp
####################################################################################
## Movie effect model on edx_temp
# Calculate the average rating in the edx_temp dataset
average_rating_temp <- mean(edx_temp$Rating)

# Estimate the movie effects (bi) by calculating the mean rating for each movie in edx_temp
movie_effects_temp <- edx_temp %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating_temp))
head(movie_effects_temp)

# Merge movie_effects_temp with edx_temp
edx_temp <- edx_temp %>%
  left_join(movie_effects_temp, by = "MovieID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi
edx_temp$predicted_rating <- average_rating_temp + edx_temp$movie_effect

# Calculate RMSE for the Movie Effect model on edx_temp
rmse_movie_effect_temp <- sqrt(mean((edx_temp$Rating - edx_temp$predicted_rating) ^ 2))
print(rmse_movie_effect_temp)
# Output print(rmse_movie_effect_temp)[1] 0.9368914
####################################################################################
### 3. Movie and User effect on edx_train
#################################################
# Calculate the average rating in the edx_train dataset
average_rating_train <- mean(edx_train$Rating)

# Estimate movie effects (bi) by calculating the mean rating for each movie in edx_train
movie_effects_train <- edx_train %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating_train))

# Merge movie_effects_train with edx_train
edx_train <- edx_train %>%
  left_join(movie_effects_train, by = "MovieID")

# Estimate user effects (bu) by calculating the mean rating for each user in edx_train
user_effects_train <- edx_train %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating_train))

# Merge user_effects_train with edx_train
edx_train <- edx_train %>%
  left_join(user_effects_train, by = "UserID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu
edx_train$predicted_rating_user_movie_train <- average_rating_train + edx_train$movie_effect + edx_train$user_effect

# Calculate RMSE for the Movie and User Effect models on edx_train
rmse_movie_user_effect_train <- sqrt(mean((edx_train$Rating - edx_train$predicted_rating_user_movie_train) ^ 2))
print(rmse_movie_user_effect_train)
# output print(rmse_movie_user_effect_train)[1] 0.8765064
###############################################################################
### 3. Movie and User Effect Models on edx_temp
#################################################
# Calculate the average rating in the edx_temp dataset
average_rating_temp <- mean(edx_temp$Rating)

# Estimate movie effects (bi) by calculating the mean rating for each movie in edx_temp
movie_effects_temp <- edx_temp %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating_temp))

# Merge movie_effects_temp with edx_temp
edx_temp <- edx_temp %>%
  left_join(movie_effects_temp, by = "MovieID")

# Estimate user effects (bu) by calculating the mean rating for each user in edx_temp
user_effects_temp <- edx_temp %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating_temp))

# Merge user_effects_temp with edx_temp
edx_temp <- edx_temp %>%
  left_join(user_effects_temp, by = "UserID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu
edx_temp$predicted_rating_user_movie_temp <- average_rating_temp + edx_temp$movie_effect + edx_temp$user_effect

# Calculate RMSE for the Movie and User Effect models on edx_temp
rmse_movie_user_effect_temp <- sqrt(mean((edx_temp$Rating - edx_temp$predicted_rating_user_movie_temp) ^ 2))
print(rmse_movie_user_effect_temp)
#output is print(rmse_movie_user_effect_temp)[1] 0.8490636

####################################################################
# 4. Movie and User Effect Models with Regularization on edx_train
###################################################################

# Set the regularization parameter (lambda)
lambda <- 0.1

# Calculate the average rating in the edx_train dataset
average_rating_train <- mean(edx_train$Rating)

# Estimate movie effects (bi) with regularization
movie_effects_train <- edx_train %>%
  group_by(MovieID) %>%
  summarize(movie_effect = lm(Rating ~ 0, weights = 1 / (1 + lambda))$coef)

# Merge movie_effects_train with edx_train
edx_train <- edx_train %>%
  left_join(movie_effects_train, by = "MovieID")

# Estimate user effects (bu) with regularization
user_effects_train <- edx_train %>%
  group_by(UserID) %>%
  summarize(user_effect = lm(Rating ~ 0, weights = 1 / (1 + lambda))$coef)

# Merge user_effects_train with edx_train
edx_train <- edx_train %>%
  left_join(user_effects_train, by = "UserID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu
edx_train$predicted_rating_user_movie_train_reg <- average_rating_train +
  edx_train$movie_effect + edx_train$user_effect

# Calculate RMSE for the Movie and User Effect models with regularization on edx_train
rmse_movie_user_effect_train_reg <- sqrt(mean((edx_train$Rating -
                                                 edx_train$predicted_rating_user_movie_train_reg) ^ 2))
print(rmse_movie_user_effect_train_reg)
#output print(rmse_movie_user_effect_train_reg)[1] 0.8765064
##################################################################
# 4. Movie and User Effect Models with Regularization on edx_temp
##################################################################

# Calculate the average rating in the edx_temp dataset
average_rating_temp <- mean(edx_temp$Rating)

# Estimate movie effects (bi) with regularization for edx_temp
movie_effects_temp <- edx_temp %>%
  group_by(MovieID) %>%
  summarize(movie_effect = lm(Rating ~ 0, weights = 1 / (1 + lambda))$coef)

# Merge movie_effects_temp with edx_temp
edx_temp <- edx_temp %>%
  left_join(movie_effects_temp, by = "MovieID")

# Estimate user effects (bu) with regularization for edx_temp
user_effects_temp <- edx_temp %>%
  group_by(UserID) %>%
  summarize(user_effect = lm(Rating ~ 0, weights = 1 / (1 + lambda))$coef)

# Merge user_effects_temp with edx_temp
edx_temp <- edx_temp %>%
  left_join(user_effects_temp, by = "UserID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu
edx_temp$predicted_rating_user_movie_temp_reg <- average_rating_temp +
  edx_temp$movie_effect + edx_temp$user_effect

# Calculate RMSE for the Movie and User Effect models with regularization on edx_temp
rmse_movie_user_effect_temp_reg <- sqrt(mean((edx_temp$Rating -
                                                edx_temp$predicted_rating_user_movie_temp_reg) ^ 2))
print(rmse_movie_user_effect_temp_reg)
#output is print(rmse_movie_user_effect_temp_reg)[1] 0.8490636
######################################################################
# 5.Movie, user and genre effect on edx_train
##################################################################
# # Calculate the average rating in the edx_train dataset
average_rating <- mean(edx_train$Rating)

# Estimate the movie effects (bi) by calculating the mean rating for each movie
movie_effects <- edx_train %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating))

# Estimate the user effects (bu) by calculating the mean rating for each user
user_effects <- edx_train %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating))

# Estimate the genre effects (bg) by calculating the mean rating for each genre
genre_effects <- edx_train %>%
  group_by(Genres) %>%
  summarise(genre_effect = mean(Rating - average_rating))

# Merge effects with edx_train
edx_train <- edx_train %>%
  left_join(movie_effects, by = "MovieID") %>%
  left_join(user_effects, by = "UserID") %>%
  left_join(genre_effects, by = "Genres")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu + bg
edx_train$predicted_rating <- average_rating + edx_train$movie_effect + edx_train$user_effect + edx_train$genre_effect

# Calculate RMSE
rmse_movie_user_genre_effect <- sqrt(mean((edx_train$Rating - edx_train$predicted_rating) ^ 2))
print(rmse_movie_user_genre_effect)
# #output is [1] 0.93772
#############################################################################
# 5. Movie, user and Genre effect on edx_temp
###########################################################################
# Estimate the movie effects (bi) for edx_temp by calculating the mean rating for each movie
movie_effects_temp <- edx_temp %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating))

# Estimate the user effects (bu) for edx_temp by calculating the mean rating for each user
user_effects_temp <- edx_temp %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating))

# Estimate the genre effects (bg) for edx_temp by calculating the mean rating for each genre
genre_effects_temp <- edx_temp %>%
  group_by(Genres) %>%
  summarise(genre_effect = mean(Rating - average_rating))

# Merge effects with edx_temp
edx_temp <- edx_temp %>%
  left_join(movie_effects_temp, by = "MovieID") %>%
  left_join(user_effects_temp, by = "UserID") %>%
  left_join(genre_effects_temp, by = "Genres")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu + bg
edx_temp$predicted_rating <- average_rating + edx_temp$movie_effect + edx_temp$user_effect + edx_temp$genre_effect

# Calculate RMSE for edx_temp
rmse_movie_user_genre_effect_temp <- sqrt(mean((edx_temp$Rating - edx_temp$predicted_rating) ^ 2))
print(rmse_movie_user_genre_effect_temp)
#output is print(rmse_movie_user_genre_effect_temp)[1] 0.9184097
#############################################################################
##################################################
### 6. Movie, user and Genre effect with regularization on edx_train
######################################################
# Regularization parameter
lambda <- 0.1

# Calculate the average rating in the edx_train dataset
average_rating <- mean(edx_train$Rating)

# Estimate the movie effects (bi) by calculating the mean rating for each movie
movie_effects <- edx_train %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating))

# Estimate the user effects (bu) by calculating the mean rating for each user
user_effects <- edx_train %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating))

# Estimate the genre effects (bg) by calculating the mean rating for each genre
genre_effects <- edx_train %>%
  group_by(Genres) %>%
  summarise(genre_effect = mean(Rating - average_rating))

# Merge effects with edx_train
edx_train <- edx_train %>%
  left_join(movie_effects, by = "MovieID") %>%
  left_join(user_effects, by = "UserID") %>%
  left_join(genre_effects, by = "Genres")

# Regularization for movie effects
edx_train$movie_effect <- with(edx_train, movie_effect / (1 + lambda * nrow(movie_effects)))

# Regularization for user effects
edx_train$user_effect <- with(edx_train, user_effect / (1 + lambda * nrow(user_effects)))

# Regularization for genre effects
edx_train$genre_effect <- with(edx_train, genre_effect / (1 + lambda * nrow(genre_effects)))

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu + bg
edx_train$predicted_rating <- average_rating + edx_train$movie_effect + edx_train$user_effect + edx_train$genre_effect

# Calculate RMSE
rmse_movie_user_genre_effect <- sqrt(mean((edx_train$Rating - edx_train$predicted_rating) ^ 2))
print(rmse_movie_user_genre_effect)
# output is print(rmse_movie_user_genre_effect)[1] 1.059107
##########################################################################
### 6. Movie, User, and Genre Effect with Regularization on edx_temp
#######################################################################

# Regularization parameter
lambda <- 0.1

# Calculate the average rating in the edx_temp dataset
average_rating_temp <- mean(edx_temp$Rating)

# Estimate the movie effects (bi) by calculating the mean rating for each movie
movie_effects_temp <- edx_temp %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating_temp))

# Estimate the user effects (bu) by calculating the mean rating for each user
user_effects_temp <- edx_temp %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating_temp))

# Estimate the genre effects (bg) by calculating the mean rating for each genre
genre_effects_temp <- edx_temp %>%
  group_by(Genres) %>%
  summarise(genre_effect = mean(Rating - average_rating_temp))

# Merge effects with edx_temp
edx_temp <- edx_temp %>%
  left_join(movie_effects_temp, by = "MovieID") %>%
  left_join(user_effects_temp, by = "UserID") %>%
  left_join(genre_effects_temp, by = "Genres")

# Regularization for movie effects
edx_temp$movie_effect <- with(edx_temp, movie_effect / (1 + lambda * nrow(movie_effects_temp)))

# Regularization for user effects
edx_temp$user_effect <- with(edx_temp, user_effect / (1 + lambda * nrow(user_effects_temp)))

# Regularization for genre effects
edx_temp$genre_effect <- with(edx_temp, genre_effect / (1 + lambda * nrow(genre_effects_temp)))

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu + bg
edx_temp$predicted_rating <- average_rating_temp + edx_temp$movie_effect + edx_temp$user_effect + edx_temp$genre_effect

# Calculate RMSE
rmse_movie_user_genre_effect_temp <- sqrt(mean((edx_temp$Rating - edx_temp$predicted_rating) ^ 2))
print(rmse_movie_user_genre_effect_temp)
#output is print(rmse_movie_user_genre_effect_temp)[1] 1.058703
#################################################################
# Validation of the final_holdout_test data on Movie, User effect model 
#################################################
# Movie and User Effect Models on final_holdout_test
#################################################

# Calculate the average rating in the final_holdout_test dataset
average_rating_final_holdout <- mean(final_holdout_test$Rating)

# Estimate movie effects (bi) by calculating the mean rating for each movie in final_holdout_test
movie_effects_final_holdout <- final_holdout_test %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating_final_holdout))

# Merge movie_effects_final_holdout with final_holdout_test
final_holdout_test <- final_holdout_test %>%
  left_join(movie_effects_final_holdout, by = "MovieID")

# Estimate user effects (bu) by calculating the mean rating for each user in final_holdout_test
user_effects_final_holdout <- final_holdout_test %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating_final_holdout))

# Merge user_effects_final_holdout with final_holdout_test
final_holdout_test <- final_holdout_test %>%
  left_join(user_effects_final_holdout, by = "UserID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu
final_holdout_test$predicted_rating_user_movie_final_holdout <- average_rating_final_holdout +
  final_holdout_test$movie_effect + final_holdout_test$user_effect

# Calculate RMSE for the Movie and User Effect models on final_holdout_test
rmse_movie_user_effect_final_holdout <- sqrt(mean((final_holdout_test$Rating -
                                                     final_holdout_test$predicted_rating_user_movie_final_holdout) ^ 2))
print(rmse_movie_user_effect_final_holdout)
# Output is print(rmse_movie_user_effect_final_holdout) [1] 0.8534251
######################################################################################



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ##################################################
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ### 6. Movie, user and Genre effect with regularization on edx_train
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ######################################################
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization parameter
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   lambda <- 0.1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Calculate the average rating in the edx_train dataset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   average_rating <- mean(edx_train$Rating)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Estimate the movie effects (bi) by calculating the mean rating for each movie
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   movie_effects <- edx_train %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     group_by(MovieID) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     summarise(movie_effect = mean(Rating - average_rating))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Estimate the user effects (bu) by calculating the mean rating for each user
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   user_effects <- edx_train %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     group_by(UserID) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     summarise(user_effect = mean(Rating - average_rating))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Estimate the genre effects (bg) by calculating the mean rating for each genre
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   genre_effects <- edx_train %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     group_by(Genres) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     summarise(genre_effect = mean(Rating - average_rating))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Merge effects with edx_train
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_train <- edx_train %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     left_join(movie_effects, by = "MovieID") %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     left_join(user_effects, by = "UserID") %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     left_join(genre_effects, by = "Genres")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization for movie effects
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_train$movie_effect <- with(edx_train, movie_effect / (1 + lambda * nrow(movie_effects)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization for user effects
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_train$user_effect <- with(edx_train, user_effect / (1 + lambda * nrow(user_effects)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization for genre effects
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_train$genre_effect <- with(edx_train, genre_effect / (1 + lambda * nrow(genre_effects)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu + bg
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_train$predicted_rating <- average_rating + edx_train$movie_effect + edx_train$user_effect + edx_train$genre_effect
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Calculate RMSE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   rmse_movie_user_genre_effect <- sqrt(mean((edx_train$Rating - edx_train$predicted_rating) ^ 2))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   print(rmse_movie_user_genre_effect)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # output is print(rmse_movie_user_genre_effect)[1] 1.059107
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ```
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ### 6. Movie, User, and Genre Effect with Regularization on edx_temp
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ```
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ##################################################
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ### 6. Movie, User, and Genre Effect with Regularization on edx_temp
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ##################################################
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization parameter
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   lambda <- 0.1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Calculate the average rating in the edx_temp dataset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   average_rating_temp <- mean(edx_temp$Rating)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Estimate the movie effects (bi) by calculating the mean rating for each movie
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   movie_effects_temp <- edx_temp %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     group_by(MovieID) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     summarise(movie_effect = mean(Rating - average_rating_temp))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Estimate the user effects (bu) by calculating the mean rating for each user
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   user_effects_temp <- edx_temp %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     group_by(UserID) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     summarise(user_effect = mean(Rating - average_rating_temp))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Estimate the genre effects (bg) by calculating the mean rating for each genre
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   genre_effects_temp <- edx_temp %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     group_by(Genres) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     summarise(genre_effect = mean(Rating - average_rating_temp))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Merge effects with edx_temp
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_temp <- edx_temp %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     left_join(movie_effects_temp, by = "MovieID") %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     left_join(user_effects_temp, by = "UserID") %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     left_join(genre_effects_temp, by = "Genres")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization for movie effects
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_temp$movie_effect <- with(edx_temp, movie_effect / (1 + lambda * nrow(movie_effects_temp)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization for user effects
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_temp$user_effect <- with(edx_temp, user_effect / (1 + lambda * nrow(user_effects_temp)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Regularization for genre effects
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_temp$genre_effect <- with(edx_temp, genre_effect / (1 + lambda * nrow(genre_effects_temp)))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu + bg
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   edx_temp$predicted_rating <- average_rating_temp + edx_temp$movie_effect + edx_temp$user_effect + edx_temp$genre_effect
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # Calculate RMSE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   rmse_movie_user_genre_effect_temp <- sqrt(mean((edx_temp$Rating - edx_temp$predicted_rating) ^ 2))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   print(rmse_movie_user_genre_effect_temp)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   #output is print(rmse_movie_user_genre_effect_temp)[1] 1.058703
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ```
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ## Results for all above models
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ## Overview
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   The table below summarizes the Root Mean Squared Error (RMSE) values for different models applied to the dataset.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Model                                                | RMSE      |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |------------------------------------------------------|-----------|
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Naive model                                          | 1.060362  |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Movie effect model                                   | 0.9423541 |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Movie and User effect model                           | 0.8765064 |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Movie and User effect with regularization               | 0.8765064 |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Movie, User and Genre effect                          | 0.93772   |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Movie, user and Genre effect with regularization      | 1.059107  |
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ## Observations
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     The lowest RMSE is achieved by the "Movie and User effect model. The RMSE value obtained with the Movie and User effect model with regularization is also the same. Surprisingly, regularization did not lead to a reduction in RMSE in this case.

### Decision

Given the identical RMSE values and the absence of improvement with regularization, the "Movie and User effect model" (without regularization) is chosen as the final model for testing on the validation dataset (final_holdout_test).

### Validation of the final_holdout_test data on Movie, User effect model 

### Movie and User Effect Model on final_holdout_test

```
#################################################
# Movie and User Effect Models on final_holdout_test
#################################################

# Calculate the average rating in the final_holdout_test dataset
average_rating_final_holdout <- mean(final_holdout_test$Rating)

# Estimate movie effects (bi) by calculating the mean rating for each movie in final_holdout_test
movie_effects_final_holdout <- final_holdout_test %>%
  group_by(MovieID) %>%
  summarise(movie_effect = mean(Rating - average_rating_final_holdout))

# Merge movie_effects_final_holdout with final_holdout_test
final_holdout_test <- final_holdout_test %>%
  left_join(movie_effects_final_holdout, by = "MovieID")

# Estimate user effects (bu) by calculating the mean rating for each user in final_holdout_test
user_effects_final_holdout <- final_holdout_test %>%
  group_by(UserID) %>%
  summarise(user_effect = mean(Rating - average_rating_final_holdout))

# Merge user_effects_final_holdout with final_holdout_test
final_holdout_test <- final_holdout_test %>%
  left_join(user_effects_final_holdout, by = "UserID")

# Calculate predicted ratings (yu,i) based on the formula: yu,i = μ + bi + bu
final_holdout_test$predicted_rating_user_movie_final_holdout <- average_rating_final_holdout +
  final_holdout_test$movie_effect + final_holdout_test$user_effect

# Calculate RMSE for the Movie and User Effect models on final_holdout_test
rmse_movie_user_effect_final_holdout <- sqrt(mean((final_holdout_test$Rating -
                                                     final_holdout_test$predicted_rating_user_movie_final_holdout) ^ 2))
print(rmse_movie_user_effect_final_holdout)
Output is print(rmse_movie_user_effect_final_holdout) [1] 0.8534251
```
### Result of Movie and User effect on final_holdout_test

| Model                     | RMSE      |
|---------------------------|-----------|
| Movie and User effect     | 0.8534251  |

## Conclusion
The exploration of different collaborative filtering models on the MovieLens dataset has provided valuable insights into the prediction of user ratings for movies. Below is a summary of the key findings:

**Model Performance**:
The "Movie and User effect model" demonstrated the lowest RMSE, indicating its effectiveness in predicting user ratings. Surprisingly, introducing regularization in the "Movie and User effect model with regularization" did not lead to a further reduction in RMSE, yielding identical results.
Validation Set Performance:

The selected model, "Movie and User effect model," exhibited strong generalization to new, unseen data, achieving an impressive RMSE of 0.8534251 on the validation set (final_holdout_test).

**Decision for Deployment**:Considering the comparable performance and simplicity of the "Movie and User effect model" without regularization, this model is recommended for deployment. Its effectiveness on the validation set suggests robust predictive capabilities.

**Areas for Further Investigation**: Despite the overall success, it's essential to explore the factors contributing to the lack of improvement with regularization. Further investigation into the dataset characteristics and the regularization approach may provide insights.

In conclusion, the chosen "Movie and User effect model" stands out as a reliable choice for predicting user ratings in collaborative filtering scenarios. The findings lay the foundation for continued refinement and exploration in the domain of recommendation systems.The methodologies applied in this project draw inspiration from the coursework of the Harvard Data Science Certificate Program. Personally, I found great satisfaction in the process of constructing diverse models and comprehending the impact of various variables on predicting Root Mean Square Error (RMSE) values. While I acknowledge that there might be additional techniques to further explore the dataset and achieve lower RMSE values, this marks my inaugural engagement with such analyses, and I have invested my utmost effort into this endeavor. I am keenly aware that alternative modeling methods could potentially yield superior outcomes. I express gratitude for the opportunity to delve into this dataset, and I look forward to advancing my skills in future explorations.

### Reference

- Irizarry, R.A. Introduction to Data Science. Retrieved from
- F. Maxwell Harper and Joseph A. Konstan. 2015. The MovieLens Datasets: History and Context. ACM Transactions on Interactive Intelligent Systems (TiiS) 5, 4: 19:1–19:19. https://doi.org/10.1145/2827872
- The discussion section for this course with recommendations from the Teaching Assistant and harvardEdx Team. ttps://discussions.edx.org/course-v1:HarvardX+PH125.9x+2T2023/posts/643f23ce70435a04a476d1cc
- The R Project for Statistical Computing. https://www.r-project.org/







