#########################################################
#title: "Blackwell_HarvardX_PH125-9_MovieLens_Project"
#author: "Todd Blackwell"
#date: March 29, 2024
#########################################################

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes
#Load necessary packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(magrittr)
library(recosystem)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

column_names <- colnames(edx)
column_names

##########################################################
# Getting to know the data 
##########################################################

#//Questions adapted from https://learning.edx.org/course/course-v1:HarvardX+PH125.9x+3T2023
#//Code is my own (author)

#How many rows and columns are there in the edx dataset?
num_rows <- nrow(edx)
num_rows

num_columns <- ncol(edx)
num_columns

#How many 0 through 5's were given as ratings in the edx dataset?
num_zeros <- sum(edx$rating == 0)
num_zeros

num_ones <- sum(edx$rating == 1)
num_ones

num_twos <- sum(edx$rating == 2)
num_twos

num_threes <- sum(edx$rating == 3)
num_threes

num_fours <- sum(edx$rating == 4)
num_fours

num_fives <- sum(edx$rating == 5)
num_fives

#How many different movies are in the edx dataset?
num_unique_movies <- length(unique(edx$movieId))
num_unique_movies

#How many different users are in the edx dataset?
num_unique_users <- length(unique(edx$userId))
num_unique_users

#How many movie ratings are in each of the following genres in the edx dataset?
num_drama_ratings <- sum(grepl("Drama", edx$genres))
num_drama_ratings
num_comedy_ratings <- sum(grepl("Comedy", edx$genres))
num_comedy_ratings
num_thriller_ratings <- sum(grepl("Thriller", edx$genres))
num_thriller_ratings
num_romance_ratings <- sum(grepl("Romance", edx$genres))
num_romance_ratings

#Which movie has the greatest number of ratings?
ratings_per_movie <- table(edx$title)
movie_with_most_ratings <- names(ratings_per_movie)[which.max(ratings_per_movie)]
movie_with_most_ratings

#What are the five most given ratings in order from most to least?
ratings_count <- table(edx$rating)
sorted_ratings <- sort(ratings_count, decreasing = TRUE)
five_most_given_ratings <- names(sorted_ratings)[1:5]
five_most_given_ratings

#Which is more common - half star or whole star ratings?

# Convert ratings to numeric
ratings_numeric <- as.numeric(edx$rating)

# Extract the decimal parts of the ratings
decimal_parts <- ratings_numeric %% 1

# Count the number of ratings with non-zero decimal parts (indicating half-star ratings)
num_half_star_ratings <- sum(decimal_parts != 0)

# Print the count of half-star ratings
print(num_half_star_ratings)

# Count the total number of ratings
total_ratings <- length(ratings_numeric)

# Calculate the number of whole-star ratings
num_whole_star_ratings <- total_ratings - num_half_star_ratings

# Print the count of whole-star ratings
print(num_whole_star_ratings)

##########################################################
# Create training and testing sets 
##########################################################

#set seed for reproducing
set.seed(1)

#split row indices based on userID.  
indexes <- split(1:nrow(edx), edx$userId)

#sample 20% of the indices, then unlist the indices, sort them, and store them in test
test_ind <- sapply(indexes, function(ind) sample(ind, ceiling(length(ind)*.2))) |>
  unlist(use.names = TRUE) |> sort()

#extract rows in test_ind to create the test set
test_set <- edx[test_ind,]

#create the training set by excluding test rows
train_set <- edx[-test_ind,]

#Remove movies that are not in both test and train sets
test_set <- test_set |> 
  semi_join(train_set, by = "movieId")
train_set <- train_set |> 
  semi_join(test_set, by = "movieId")

#Use pivot_wider to make a matrix with users in rows and movies in columns
y <- select(train_set, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames

##########################################################
#Just the Average Model
##########################################################

#//Code for this model adapted from https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+3T2023/home
#//Good faith efforts were made to make the code my own (the authors)

#Compute RMSE for vectors of ratings and their corresponding predictors
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Predict the same rating for all movies regardless of user
mu <- mean(y, na.rm = TRUE)
mu

#Predict all unknown ratings
naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse

#Create a results table, with our first result called "Just the average"
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

##########################################################
#Movie Effect Model
##########################################################

#Compute the least squares estimate
b_i <- colMeans(y - mu, na.rm = TRUE)

#View the estimates in a plot
qplot(b_i, bins = 10, color = I("black"))

#create a data frame with three columns, movieID, mu and b_i
fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b_i = b_i)

#perform left join between test_set and fit_movies, matching on movieID.
movie_effect_result <- left_join(test_set, fit_movies, by = "movieId") |> 

#add a new column prediction to the data frame
mutate(pred = mu + b_i) |> 

#summarize the RMSE
summarize(rmse = RMSE(rating, pred))

#place the RMSE into a Vector
Movie_Effect_RMSE <- movie_effect_result$rmse

#Confirm vector result = summarize result
Movie_Effect_RMSE

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = Movie_Effect_RMSE ))
rmse_results %>% knitr::kable()

##########################################################
#User Effect Model
##########################################################

#Compute and plot average rating for those that have rated 100 or more movies
b_u <- rowMeans(y, na.rm = TRUE)
qplot(b_u, bins = 30, color = I("black"))

#Compute b_u as a user-specific effect on ratings
b_u <- rowMeans(sweep(y - mu, 2, b_i), na.rm = TRUE)

#Construct a predictor and see if RMSE improves
fit_users <- data.frame(userId = as.integer(rownames(y)), b_u = b_u)

user_effect_result <- left_join(test_set, fit_movies, by = "movieId") |> 
  left_join(fit_users, by = "userId") |> 
  mutate(pred = mu + b_i + b_u) |> 
  summarize(rmse = RMSE(rating, pred))

#place the RMSE into a Vector
User_Effect_RMSE <- user_effect_result$rmse

#Confirm vector result = summarize result
User_Effect_RMSE

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          tibble(method="User Effect Model",
                                     RMSE = User_Effect_RMSE ))

rmse_results %>% knitr::kable()

##########################################################
#Genre Effect Model
##########################################################

# Split the genres into separate columns
genres_list <- strsplit(as.character(edx$genres), "\\|")

# Convert the list of lists into a single list of genres
all_genres <- unlist(genres_list)

# Count the occurrences of each genre
genre_counts <- table(all_genres)

# Convert table to a data frame
genre_counts_df <- as.data.frame(genre_counts)

# Print the data frame in a table format
kable(genre_counts_df, caption = "Genre Counts")

# Compute b_g as a genre-specific effect on ratings
fit_genres <- train_set %>% left_join(fit_movies, by = "movieId") %>%
  left_join(fit_users, by='userId') %>%
  group_by(genres) %>% summarize(b_g = mean(rating - mu - b_i - b_u))

# Construct a predictor for genre testing
predict_test_with_genre <- test_set %>% left_join(fit_movies, by='movieId') %>% 
  left_join(fit_users, by='userId') %>%
  left_join(fit_genres, by='genres') %>% 
  mutate(pred = mu + b_i + b_u + b_g) %>% 
  pull(pred)

# Generate Genre Effect RMSE
Genre_Effect_RMSE <- RMSE(predict_test_with_genre, test_set$rating)

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre Effect Model",
                                     RMSE = Genre_Effect_RMSE ))

rmse_results %>% knitr::kable()

##########################################################
#Regularized Movie Effect Model
##########################################################

#Calculate regularization terms for each movie in the training set

#set regularization parameter to 3
lambda <- 3

#calculate mean rating of all movies in the training set
mu <- mean(train_set$rating)

#group rows of training set by movieID column & calculate summary statistics
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#Calculate predicted ratings based on regularization
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

regularized_movie_result <- RMSE(predicted_ratings, test_set$rating)

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = regularized_movie_result ))
rmse_results %>% knitr::kable()

#######################################
#REGULARIZED MOVIE + USER EFFECT MODEL#
#######################################

#Create regularization parameters for collaborative filtering
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

#Select the optimal value of lambda by computing RMSE for different lambdas
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  

#Identify minimum RMSE
lambdas[which.min(rmses)]

#Select optimal Lambda value for collaborative filtering model with plot
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

#Re-Identify minimum RMSE
lambdas[which.min(rmses)]

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

###############################################
#REGULARIZED MOVIE + USER + GENRE EFFECT MODEL#
###############################################

#Create regularization parameters for collaborative filtering
lambdas <- seq(0, 20, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - mu - b_u)/(n() + l))
  
  reg_genre_model_predict <- test_set %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(reg_genre_model_predict, test_set$rating))
})

# Plot Lambdas & RMSES
qplot(lambdas, rmses)

# Find minimum lambda
min_genre_lambda <- lambdas[which.min(rmses)]
min_genre_lambda

# Calculate the minimum RMSE 
rmse_reg_genre_model <- min(rmses)
rmse_reg_genre_model

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Genre Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

###############################################
#Matrix Factorization - Recosystem
###############################################
#//code adapted from github.com/papacosmas/MovieLens and github/yixuan/recosystem
#//Final code is my own (author)

#Define RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Arrange training data is sparse matrix form
edx_factorization <- edx %>% select(movieId,userId,rating)
validation_factorization <- final_holdout_test %>% select(movieId,userId,rating)

#Save factorizations as tables on HD
write.table(edx_factorization , file = "trainingset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_factorization, file = "validationset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

set.seed(1)
training_dataset <- data_file( "trainingset.txt")
validation_dataset <- data_file( "validationset.txt")

#create a model object called r
r = Reco()

#set tuning parameters and candidate values
opts = r$tune(training_dataset, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                            costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 10))

#train the model using tuning parameters
r$train(training_dataset, opts = c(opts$min, nthread = 1, niter = 20))

## Store the iterative RMSE values in a vector
rmse_values <- c(0.9725, 0.8718, 0.8390, 0.8174, 0.8016, 0.7898, 0.7803, 0.7724, 0.7657, 0.7598,
                 0.7546, 0.7500, 0.7459, 0.7422, 0.7388, 0.7357, 0.7328, 0.7302, 0.7277, 0.7254)

# Extract the RMSE values
start_mf_rmse <- max(rmse_values)
end_mf_rmse <- min(rmse_values)
avg_mf_rmse <- mean(rmse_values)

# Create a data frame with all RMSE values
mf_rmse_df <- data.frame(
  method = c("Matrix Factorization - Start", 
             "Matrix Factorization - End", 
             "Matrix Factorization - Mean"),
  RMSE = c(start_mf_rmse, end_mf_rmse, avg_mf_rmse)
)

# Update and display rmse_results table
rmse_results <- bind_rows(rmse_results, mf_rmse_df)

rmse_results %>% knitr::kable()

###############################################
#Matrix Factorization - Validation Testing
###############################################

# Write predictions to a tempfile on HD
stored_prediction = tempfile()   

#make predictions on validation set
r$predict(validation_dataset, out_file(stored_prediction)) 

#display first 20 predictions
print(scan(stored_prediction, n = 20))

#create dataframe from tables
real_ratings <- read.table("validationset.txt", header = FALSE, sep = " ")$V3
pred_ratings <- scan(stored_prediction)

#calculate RMSE
final_holdout_test_rmse <- RMSE(real_ratings,pred_ratings)
final_holdout_test_rmse

#Update and display rmse_results table.
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Final Holdout Test",  
                                     RMSE = final_holdout_test_rmse))

rmse_results %>% knitr::kable()

############################################
# Appendix A - Matrix Factorization Attempts
############################################

#Multiple attempts at Matrix Factorization were made where code errored out.  
#We have included those attempts in this appendix.
#Code will result in an error, but are shown to display that multiple other attempts at machine learning methods were made.

##################################
# Matrix Factorization - SVD
##################################
# Matrix factorization using Singular Value Decomposition (SVD) is a technique that breaks down a matrix into three smaller matrices to capture underlying patterns and relationships within the data
# Error details: Error in mult(A, VJ) : requires numeric/complex matrix/vector arguments

if(!require(bigmemory)) install.packages("bigmemory", repos = "http://cran.us.r-project.org")
if(!require(irlba)) install.packages("irlba", repos = "http://cran.us.r-project.org")
library(bigmemory)
library(irlba)

# Convert train_set to a big.matrix object
train_bigmatrix <- as.big.matrix(as.matrix(train_set))
nrow(train_bigmatrix)
ncol(train_bigmatrix)

# Adjust the value of nv to be less than the minimum dimension of the input matrix
nv <- min(nrow(train_bigmatrix), ncol(train_bigmatrix)) - 4

# Perform Singular Value Decomposition (SVD) using irlba
svd_result <- irlba(train_bigmatrix, nv = nv)

# Extract singular vectors and values
u <- svd_result$u
d <- svd_result$d
v <- svd_result$v

# Reconstruction using SVD
reconstructed <- u %*% diag(d) %*% t(v)

# Extract predicted ratings for test set
predicted_ratings <- reconstructed[test_set$userId, test_set$movieId]

# Calculate RMSE
svd_result <- RMSE(predicted_ratings, test_set$rating)
svd_result

##########################################
# Non-Negative Matrix Factorization (NMF)
##########################################
#Similar to SVD but constrains the factors to be non-negative, which can lead to more interpretable results.
# Error details: error in evaluating the argument 'object' in selecting a method for function 'basis': object 'nmf_result' not found

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")
n
install.packages("NMF")
library(NMF)

# Convert train_set to a matrix if it's not already in matrix format
train_matrix <- as.matrix(train_set)

# Remove rows with NA values
train_matrix_numeric <- na.omit(train_matrix)

class(train_matrix_numeric)

# Replace negative values in train_matrix with zeros
train_matrix_numeric[train_matrix < 0] <- 0

# Filter out non-numeric columns from train_matrix
train_matrix_numeric <- as.matrix(as.numeric(train_matrix_numeric))

# Specify the number of latent factors (rank) for NMF
rank <- 10  # Adjust as needed

# Perform Non-Negative Matrix Factorization (NMF)
nmf_result <- nmf(train_matrix_numeric, rank = rank)

# Extract factor matrices (basis and coefficient matrices)
basis <- basis(nmf_result)  # Basis matrix (W)
coef <- coef(nmf_result)    # Coefficient matrix (H)

# Reconstruct the ratings matrix using factor matrices
reconstructed_matrix <- basis %*% coef

###############################################
#Matrix Factorization - Recommenderlab
###############################################
# The recommenderlab package provides matrix factorization methods such as SVD and NMF
# Since the previous SVD and NMF methods errored out, we are trying recommenderlab
# Error details: Error: attempt to apply non-function

# Install and load required packages
install.packages("recommenderlab")
library(recommenderlab)

# Convert train_set to a realRatingMatrix object (if not already in that format)
train_matrix <- as(train_set, "realRatingMatrix")

# Define the SVD recommender function
SVD_recommender <- function(train_matrix, ...) {
  svd_model <- svd(as(train_matrix, "matrix"))
  list(U = svd_model$u, D = diag(svd_model$d), V = svd_model$v)
}

# Register the new SVD recommender method
recommenderRegistry$register(method = "SVD", recommender = SVD_recommender, dataType = "realRatingMatrix", parameter = list())

# Define a new SVD recommender method
set.seed(123)  # Set seed for reproducibility
SVD_recommender <- function(data, ...) {
  svd_model <- svd(as(data, "matrix"))
  list(U = svd_model$u, D = diag(svd_model$d), V = svd_model$v)
}

# Register the new SVD recommender method
recommenderRegistry$register(method = "SVD", recommender = SVD_recommender, dataType = "realRatingMatrix", parameter = list())

# Convert the training set to a realRatingMatrix
train_matrix <- as(train_set, "realRatingMatrix")

# Train the SVD model on the training set
svd_model <- Recommender(train_matrix, method = "SVD")

# Generate recommendations for users in the test set
recommendations <- predict(svd_model, newdata = test_set, n = 10)

# Show the top recommended items for each user in the test set
top_recommendations <- as(recommendations, "list")
top_recommendations

###############################################
#Matrix Factorization - Minibatch
###############################################
# Because the previous matrix factorization methods have failed due to size contraints, we make one last attempt
# We attempt to test our matrix in batches of 1,000 each for ~70,000 rows
# Error details: Error: cannot allocate vector of size 716.0 Gb

# Create user-item rating matrix from the training set
rating_matrix <- sparseMatrix(i = train_set[,1], j = train_set[,2], x = train_set[,3])

# Minibatch control parameters (adjust as needed)
batch_size <- 1000  # Adjust batch size based on memory constraints
init <- "svdpp"    # Choose appropriate initialization method

# Define the matrix factorization model using caret
model_control <- trainControl(method = "list",
                              summaryFunction = oneSE,
                              returnData = TRUE)

# Define the matrix factorization function with minibatch support
mf_function <- function(train_data, ...) {
  # Extract user and item IDs for minibatch processing
  user_ids <- train_data[, 1]
  item_ids <- train_data[, 2]
  ratings <- train_data[, 3]

############################################
# Appendix B - Random Forest Method
############################################  
# We also tested the edx dataset using the Random Forest Method.
# The RMSEs for these methods were above 1, much higher than other methods we used.
# Possible reasons for this are:
# a. The datasets patterns and relationships are challenging for the Random Forest model to capture accurately.
# b. The features in the random forest model may not represent the underlying patterns in the dataset.
# c. The random forest model may be overfitting the training data.
# Ultimately, the random forest method was not the best choice for this dataset.
# There are better machine learning algorithms better suited for this dataset.
  
#################################################
#Distributed Random Forest Method - Primary Genre
#################################################

# Extract the first genre before the "|" character
train_set$primary_genre <- sub("\\|.*", "", train_set$genres)
  
# Print the first few rows to verify
head(train_set)
unique_primary_genres <- sort(unique(train_set$primary_genre))
unique_primary_genres
  
str(train_set)
  
# Create dummy variables for "primary_genre" using model.matrix()
genre_dummies <- model.matrix(~ primary_genre - 1, data = train_set)
  
# Print the first few rows to see the dummy variables
head(genre_dummies)
  
# Train Random Forest model using dummy variables
rf_model <- randomForest(x = genre_dummies,
                           y = train_set$rating,
                           ntree = 50,  # Adjust as needed
                           importance = TRUE)  # Compute variable importance
  
rfmodel