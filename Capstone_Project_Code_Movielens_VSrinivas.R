# Movie Ratings Prediction Code    
# Capstone Project
# Data Science Professional Certificate Program from HarvardX

# Author: "Valmeti Srinivas"
# date: "10/10/2019"

  
# Data Set-up:

# Load required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# Download data and create edx & validation datsets
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# If using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1, sample.kind="Rounding")

# Validation set will be approx 10% of MovieLens data
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from temp set back into edx set
removed <- anti_join(temp, validation)

edx <- rbind(edx, removed)

# Create Year column for later analysis
library(lubridate)
edx <- edx %>% mutate(year=year(as_datetime(timestamp)))

# Remove objects that are no longer necessary
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# 3. Methods & Analysis - Data Exploration & Results:

# edx dataset structure, variables, data glimpse 
dim(edx)
glimpse(edx)

# number of ratings with 0 rating 
edx %>% filter(rating==0) %>% count()

# number of ratings with 3 rating 
edx %>% filter(rating==3) %>% count()

# no.of unique movies, users & genres
n_distinct(edx$movieId)
n_distinct(edx$userId)
n_distinct (edx$genres)

# no.of ratings under Drama, Comedy, Thriller & Romance genres
edx %>% filter(grepl("Drama", genres)) %>% count()
edx %>% filter(grepl("Comedy", genres)) %>% count()
edx %>% filter(grepl("Thriller", genres)) %>% count()
edx %>% filter(grepl("Romance", genres)) %>% count()

# Top 10 movies with maximum ratings
edx %>% group_by(movieId,title) %>% summarize(count=n()) %>% 
  arrange(desc(count)) %>% top_n(10)

# Top 10 users who gave maximum number of ratings
edx %>% group_by(userId) %>% summarize(count=n()) %>% 
  arrange(desc(count)) %>% top_n(10)

# Top 10 genres with maximum number of ratings
edx %>% group_by(genres) %>% summarize(count=n()) %>% 
  arrange(desc(count)) %>% top_n(10)

# Ratings in the order of maximum rate
edx %>% group_by(rating) %>% summarize(count=n()) %>% 
  arrange(desc(count))

# Half star ratings Vs full star ratings
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

# Store top 10 Movies by the number of ratings
store_top_10ms <- edx %>%
  dplyr::count(movieId) %>%
  top_n(10) %>%
  pull(movieId)

# Plot ratings for top 10 movies
edx %>%
  filter(movieId %in% store_top_10ms) %>%   mutate (movieId=as.character(movieId)) %>%
  select(movieId, rating) %>% 
  ggplot(aes(movieId,rating)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))

# Store top 25 users by the number of ratings
store_top_25us <- edx %>%
  dplyr::count(userId) %>%
  top_n(25) %>%
  pull(userId)

# Plot Top 25 users ratings
edx %>%
  filter(userId %in% store_top_25us) %>% 
  mutate (userId=as.character(userId)) %>%
  select(userId, rating) %>% 
  ggplot(aes(userId,rating)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))

# Store top 10 genres by the number of ratings
store_top_10gs <- edx %>%
  dplyr::count(genres) %>%
  top_n(10) %>%
  pull(genres)

# Plot Top 10 genres ratings
edx %>%
  filter(genres %in% store_top_10gs) %>% 
  select(genres,movieId, rating) %>% 
  ggplot(aes(genres,rating)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))

# All users might not rate all movies
uni_200users <- sample(unique(edx$userId), 200)
rafalib::mypar()
edx %>% filter(userId %in% uni_200users) %>% 
  select(userId, movieId, rating) %>% 
  spread(movieId, rating) %>% select(sample(ncol(.), 200)) %>% 
  as.matrix() %>% t(.)%>%
  image(1:200, 1:200,. , xlab="Movies", ylab="Unique Users")

# Distribution of movies based no.of ratings received
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# Distribution of users based on no.of ratings ratings given
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# Distribution of genres based on no.of ratings ratings under
edx %>% 
  group_by(genres) %>%
  filter(n()>=100000) %>%
  summarize(n=n(),avg=mean(rating),se=sd(rating)/sqrt(n())) %>% arrange(desc(avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Start year and end year in the dataset (of the ratings)
min(edx$year)
max(edx$year)

# Distribution of ratings over the years
edx %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year)))%>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Distribution of no.of ratings week wise
edx <- mutate(edx, date = as_datetime(timestamp))

edx %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# 4. Methods & Analysis - Model Building & Results:

# Create test dataset and train dataset
set.seed(1, sample.kind="Rounding")
test_ind <- createDataPartition(y=edx$rating,times=1, p=.1, list=FALSE)
train_ds <- edx[-test_ind,]
test_ds_temp <- edx[test_ind,]

# Make sure userId and movieId in test dataset are also in train dataset
test_ds <- test_ds_temp %>% 
  semi_join(train_ds, by = "movieId") %>%
  semi_join(train_ds, by = "userId")

# Add rows removed from test_ds_temp dataset back into train dataset
rmvd <- anti_join(test_ds_temp, test_ds)
train_ds <- rbind(train_ds, rmvd)

# Formula for RMSE
RMSE <- function(testval_ratings, pred_ratings){
  sqrt(mean((testval_ratings - pred_ratings)^2))
}

# Model.1: Computing predicted ratings based on the average of population
mu_simple <- mean(train_ds$rating)
mu_simple

mod1_rmse <- RMSE(test_ds$rating, mu_simple)
mod1_rmse

# RMSE table for different models
rmse_table <- tibble(Model = "Model.1", Method="Simple Population Mean Model",
                     RMSE = mod1_rmse)
rmse_table %>% knitr::kable()

# Model.2: Computing predicted ratings based on movie effects
mu <- mean(train_ds$rating)

# Add movie effect
movie_effects <- train_ds %>% 
  group_by(movieId) %>% 
  summarize(m_effect = mean(rating - mu))

movie_effects %>% qplot(m_effect, geom ="histogram", bins = 30, data = ., color = I("black"))

# Predict ratings
pred_ratings <- mu + test_ds %>% 
  left_join(movie_effects, by='movieId') %>%
  .$m_effect

mod2_rmse <- RMSE(test_ds$rating, pred_ratings)

rmse_table <- bind_rows(rmse_table,
                        tibble(Model = "Model.2", Method="Movie Effect Model",
                               RMSE = mod2_rmse ))

mod2_rmse
rmse_table %>% knitr::kable()

# Model.3: Computing predicted ratings based on movie effects and user effects
# Add user effect
user_effects <- train_ds %>% 
  left_join(movie_effects, by='movieId') %>%
  group_by(userId) %>%
  summarize(u_effect = mean(rating - mu - m_effect))

user_effects %>% qplot(u_effect, geom ="histogram", bins = 30, data = ., color = I("black"))

# Predict ratings
pred_ratings <- test_ds %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(pred = mu + m_effect + u_effect) %>%
  .$pred

mod3_rmse <- RMSE(test_ds$rating, pred_ratings)
rmse_table <- bind_rows(rmse_table,
                        tibble(Model = "Model.3", Method="Movie + User Effect Model",
                               RMSE = mod3_rmse ))

mod3_rmse
rmse_table %>% knitr::kable()

# Model.4: Computing predicted ratings based on movie, user & genre effects
# Add genre effect
genre_effects <- train_ds %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  group_by(genres) %>%
  summarize(g_effect = mean(rating - mu - m_effect - u_effect))

genre_effects %>% qplot(g_effect, geom ="histogram", bins = 30, data = ., color = I("black"))

# Predict ratings
pred_ratings <- test_ds %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(genre_effects, by='genres') %>%
  mutate(pred = mu + m_effect + u_effect + g_effect) %>%
  .$pred

mod4_rmse <- RMSE(test_ds$rating, pred_ratings)
rmse_table <- bind_rows(rmse_table,
                        tibble(Model = "Model.4", Method="Movie + User + Genre Effect Model",
                               RMSE = mod4_rmse ))

mod4_rmse
rmse_table %>% knitr::kable()

# Predict ratings on Validation Dataset for testing purpose
val_test_ratings <- validation %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(genre_effects, by='genres') %>%
  mutate(pred = mu + m_effect + u_effect + g_effect) %>%
  .$pred

val_test_rmse <- RMSE(validation$rating, val_test_ratings)
val_test_rmse

# Retrospective analysis - 20 largest prediction gaps - Model.2:
test_ds %>% 
  left_join(movie_effects, by='movieId') %>%
  mutate(error = rating - (mu + m_effect)) %>%
  arrange(desc(abs(error))) %>% 
  select(title,  error) %>% slice(1:20) 

# create a database that connects movieId to movie title
mtitles <- edx %>% 
  select(movieId, title) %>%
  distinct()

# Best 20 movies as predicted by our Model.2
movie_effects %>% left_join(mtitles, by="movieId") %>%
  arrange(desc(m_effect)) %>% 
  select(title, m_effect) %>% 
  slice(1:20) 

train_ds %>% dplyr::count(movieId) %>% 
  left_join(movie_effects) %>%
  left_join(mtitles, by="movieId") %>%
  arrange(desc(m_effect)) %>% 
  select(title, m_effect, n) %>% 
  slice(1:20)

# Worst 20 movies as predicted by our model.2
train_ds %>% dplyr::count(movieId) %>% 
  left_join(movie_effects) %>%
  left_join(mtitles, by="movieId") %>%
  arrange(m_effect) %>% 
  select(title, m_effect, n) %>% 
  slice(1:20)

# Finding optimal alpha - penalty term to reduce the effect of few ratings
alphas <- seq(0, 20, 0.25)
mu <- mean(train_ds$rating)
totals <- train_ds %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(alphas, function(a){
  pred_ratings <- test_ds %>% 
    left_join(totals, by='movieId') %>% 
    mutate(m_effect = s/(n_i+a)) %>%
    mutate(pred = mu + m_effect) %>%
    .$pred
  return(RMSE(test_ds$rating, pred_ratings))
})

qplot(alphas, rmses) 
opt_alpha <- alphas[which.min(rmses)]
opt_alpha

# recalculate the regularized movie effects and plot against initial movie effects.
alpha <- opt_alpha
mu <- mean(train_ds$rating)
movie_reg_effects <- train_ds %>% 
  group_by(movieId) %>% 
  summarize(m_effect = sum(rating - mu)/(n()+alpha), n_i = n()) 

tibble(original = movie_effects$m_effect, 
       regularlized = movie_reg_effects$m_effect, 
       n = movie_reg_effects$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# Best 20 movies predicted after regularisation
train_ds %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_effects) %>%
  left_join(mtitles, by="movieId") %>%
  arrange(desc(m_effect)) %>% 
  select(title, m_effect, n) %>% 
  slice(1:20)

# Worst 20 movies predicted after regularisation
train_ds %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_effects) %>%
  left_join(mtitles, by="movieId") %>%
  arrange(m_effect) %>% 
  select(title, m_effect, n) %>% 
  slice(1:20)

# Model.5: Computing predicted ratings based on regularised movie, user & genre effects
alphas <- seq(0, 20, 0.25)

rmses <- sapply(alphas, function(a){
  mu <- mean(train_ds$rating)
  
  # Add movie effect with regularisation
  m_effect <- train_ds %>%
    group_by(movieId) %>%
    summarize(m_effect = sum(rating - mu)/(n()+a))
  
  # Add user effect with regularisation
  u_effect <- train_ds %>% 
    left_join(m_effect, by="movieId") %>%
    group_by(userId) %>%
    summarize(u_effect = sum(rating - m_effect - mu)/(n()+a))
  
  # Add genre effect with regularisation
  g_effect <- train_ds %>% 
    left_join(m_effect, by="movieId") %>%
    left_join(u_effect, by="userId") %>%
    group_by(genres) %>%
    summarize(g_effect = sum(rating - m_effect - u_effect - mu)/(n()+a))
  
  # Predict ratings
  pred_ratings <- 
    test_ds %>% 
    left_join(m_effect, by = "movieId") %>%
    left_join(u_effect, by = "userId") %>%
    left_join(g_effect, by = "genres") %>%
    mutate(pred = mu + m_effect + u_effect + g_effect) %>%
    .$pred
  return(RMSE(test_ds$rating, pred_ratings))
})

qplot(alphas, rmses)
alphas[which.min(rmses)]
mod5_rmse <- min(rmses)
rmse_table <- bind_rows(rmse_table,
                        tibble(Model = "Model.5", 
                               Method="Regularized (Movie + User + Genre) Effect Model",  
                               RMSE = mod5_rmse ))

mod5_rmse
rmse_table %>% knitr::kable()

# Methods & Analysis - Final Model Testing on Validation Dataset:

# Testing the final model on the Validation dataset & Computing final RMSE
#(Regularised Movie + User + Genre Effect model)
alphas <- seq(0, 20, 0.25)

rmses_v <- sapply(alphas, function(a){
  mu <- mean(train_ds$rating)
  
  # Add regularised movie effect
  m_effect <- train_ds %>%
    group_by(movieId) %>%
    summarize(m_effect = sum(rating - mu)/(n()+a))
  
  # Add regularised user effect
  u_effect <- train_ds %>% 
    left_join(m_effect, by="movieId") %>%
    group_by(userId) %>%
    summarize(u_effect = sum(rating - m_effect - mu)/(n()+a))
  
  # Add regularised genre effect
  g_effect <- train_ds %>%
    left_join(m_effect, by="movieId") %>%
    left_join(u_effect, by="userId") %>%
    group_by(genres) %>%
    summarize(g_effect = sum(rating - m_effect - u_effect - mu)/(n()+a))
  
  # Predict ratings for validation dataset
  pred_ratings <- 
    validation %>% 
    left_join(m_effect, by = "movieId") %>%
    left_join(u_effect, by = "userId") %>%
    left_join(g_effect, by = "genres") %>%
    mutate(pred = mu + m_effect + u_effect + g_effect) %>%
    .$pred
  return(RMSE(validation$rating, pred_ratings))
})

qplot(alphas, rmses_v)  
alphas[which.min(rmses_v)]
val_rmse <- min(rmses_v)
val_rmse

# The RMSE value that our final model achieved on "validation" dataset is 0.8648532 


###############################################