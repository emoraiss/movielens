################################

#Harvardx - Data Science Capstone - Movielens Project

#For this project, we will be creating a movie recommendation system using the MovieLens dataset.

#Student: Eline Morais

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#########################

#Understanding the dataset

#######################
# The following packages are required. 

library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(stringr)
library(lubridate)

# First of all let's check if we have any missing data.

any(is.na(edx))
any(is.na(validation))


#Dimension - edx dataset shows 9.000.055 observations (rows) and 6 variables (columns)  
  dim(edx)
  head(edx)

#Each row represents a rating given by one user to one movie. 
#Edx presents 10.677 different movies and 69.878 different users, 797 mixed genres and 10 different ratings.
  edx %>% summarise( Movies = n_distinct(movieId), Users= n_distinct(userId), Genres = n_distinct(genres), Ratings= n_distinct(rating))
  
#Quick summary of edx
  summary(edx)

# Edx shows 10 different rating scores, between  0.5 and 5, following the distribution:
#Most ratings

  edx %>% group_by(rating) %>% summarise(count= n()) %>% arrange(desc(count))

  edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth=0.3, color="black", fill="lightblue") + 
  ggtitle("Rating Distribution (Edx)")

# Some movies are rated more often than others
#~1150 movies were rated less than 10 times and few movies (143) were rated more than 10.000 times
  edx %>% group_by(movieId) %>% summarize(rated = n()) %>% arrange(desc(rated)) %>%
  ggplot(aes(rated)) + 
  geom_histogram(bins = 30, binwidth=0.2, color="black", show.legend = FALSE, fill ="lightblue" ) + 
  scale_x_log10() + 
  ggtitle("Movies Rated Distribution")

  edx %>% group_by(title) %>% summarize(count= n()) %>% filter(count<=10) %>% count()
  
  edx %>% group_by(title) %>% summarize(count= n()) %>% filter(count>=10000) %>% count()

#Is there any relationship between released year and ratings?

#To see this it will be necessary to extract release year from "title" into a separate column.
#We'll create "date" variable adjusting timestamp as well.
  edx <- edx %>% mutate(release_year = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),
                        title = str_remove(title, "[/(]\\d{4}[/)]$"),
                       date = as_datetime(timestamp))
  
  head(edx)
 
#We see the oldest movie was released in 1915 and the newer in 2008. 
#Also, dataset presents  that ratings had began in 1995 and had ended in 2009.
 edx %>% summarise( first_release = min(edx$release_year),
                    last_release = max(edx$release_year),
                    first_rating = min(year(edx$date)),
                    last_rating = max(year(edx$date))) %>% knitr:: kable()
 

#We can see an exponential growth of releasing movies after 1990 
#Number of movies per year/decade
  edx %>%
   select(movieId, release_year) %>% 
   group_by(release_year) %>% 
   summarise(count = n())  %>% 
   arrange(release_year) %>%
   ggplot(aes(x = release_year, y = count)) +
   geom_line(color="blue")+
  ggtitle("Movies released per year")
 
#At the same way, we see a peak in the number of users rating movies released in 1995, 
#(year that reviews started), decreasing in both directions rating of old films and newer movies 
 
 #users by release year
  edx %>% group_by(release_year) %>% summarize(users= n_distinct(userId), year = as.character(first(release_year))) %>%
   qplot(year, users, data = .) +
   coord_trans(y = "sqrt") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   ggtitle("Release Year vs. users")
 
#Movies started to be ratings in 1995. It has taken 3 years to stabilize the number of users rating movies. 
#We can see a linear trend since 1999 until 2009 which data ratings seems to be incomplete.
 #users by rating year
  edx %>% mutate(rating_year= year(date)) %>% group_by(rating_year) %>% summarize(users= n_distinct(userId), year = as.character(first(rating_year))) %>%
   arrange(desc(users)) %>% top_n(., 5)
  edx %>% mutate(rating_year= year(date)) %>% group_by(rating_year) %>% summarize(users= n_distinct(userId), year = as.character(first(rating_year))) %>%
   qplot(rating_year, users, data=.) +
   coord_trans(y = "sqrt") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   ggtitle("Rating Year vs. users")
 
 
 #We also see that, on average, movies that came out after 1993 get more ratings. 
   edx %>% group_by(movieId) %>%
   summarize(n = n(), year = as.character(first(release_year))) %>%
   qplot(year, n, data = ., geom = "boxplot") +
   coord_trans(y = "sqrt") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   ggtitle("Release Year vs. Ratings")
   
# Older "classics" get higher ratings. This could allow us to penalize a movie based on release year
# by a calculated weight.
  #View release year vs rating
        edx %>% group_by(movieId) %>% filter(n()>=100) %>% ungroup() %>% group_by(release_year) %>%
     summarize(rating = mean(rating)) %>%
     ggplot(aes(release_year, rating)) +
     geom_point() +
     theme_hc() + 
     geom_smooth() +
     ggtitle("Release Year vs. mean Rating")
        
#As shown, two facts are to be expected:
#1- more recent films have less ratings because they have less time to be evaluated,
#2- increase in the average rating of older films, since it is necessary to have a genuine interest from
#users to search for old films, it is likely that the most rated old movie are the classics.

# Follow the table with the 25 movies most ratings per year since 2009 (last year of rating) 
#descending from numbers of ratings per year  
        
      edx %>% 
      group_by(movieId) %>%
      summarize(n = n(), years = 2009 - first(release_year),
                    title = title[1],
                    rating = mean(rating)) %>%
      mutate(rate = n/years) %>%
      top_n(25, rate) %>%
      arrange(desc(rate)) 

#From the previous table we see that the most rated movies tend to have above average ratings. 
#This is not surprising: more people watch popular movies. To confirm this, follow the plot of average
#rating versus number of ratings showing an estimate of the trend.
      
  #Number of ratings x mean 
  edx %>% group_by(movieId) %>% summarize(count= n(), mean= mean(rating)) %>% arrange(desc(mean)) %>%
    ggplot(aes(count, mean)) + 
    geom_point(binwidth=0.2, color="black", show.legend = FALSE) + 
    geom_smooth()+
    coord_trans(x = "sqrt") +
    ggtitle("Number of Ratings x mean")
        
# Yet, we can look for correlation between number of times each user has reviewed movies and its average.
#It's seems there is no correlation between number ratings and its average.
  
  users<- edx %>% group_by(userId) %>% summarise(avg= mean(rating))
  count_users<- edx %>% count(userId) %>% left_join(., users, by = "userId") 

  count_users %>% 
    ggplot(aes(n, avg)) + geom_point()+
    coord_trans(x= "sqrt")+
    ggtitle("User Reviews vs. mean Rating")
  

  
# Dataset show 797 different mixed genres 
n_distinct(edx$genres)


#Some movies fall under several genres.Defining a category as whatever combination appears in this "genres" 
#column.We can plot a bar plot to see if there is also a genre effect.
#To facilitate visualization we are going to Keep only categories with more than 50,000 ratings. Then compute the average and standard error for each
#category. Plot these as error bar plots.
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 50000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Genres effect in  average")

    
#The plot shows strong evidence of a genre effect. 

################################################################################

# Modelling aproach

################################################################################
#creating training and test set from edx - Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_temp <- edx[test_index,]

edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(edx_temp, removed)

#Training dataset defined as edx_train with 8.100.065 rows and 8 columns. 
#(Columns "release_year" and "date" added)
dim(edx_train)
#Test data set defined as edx_test with 899.990 rowns and 8 columns
dim(edx_test)

#define a function to calculate the residual mean squared error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#The simplest model - Mean
#the simplest possible recommendation system: we predict the same rating for all movies regardless of user.
mu_hat<- mean(edx_train$rating)
mu_hat

# mean - RMSE (1.060054)
mean_rmse <- RMSE(edx_test$rating, mu_hat)
mean_rmse


# Modeling movie effects - bias due to movie (0.9429615)
#We understand that some movies are generally rated higher
mu<- mean(edx_train$rating)
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

#Predicting rating including movie effects 
movie_hat <- mu + edx_test %>% 
left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

movie_rmse<- RMSE(edx_test$rating, movie_hat)
movie_rmse

#Modeling user effects - bias due to users (0.8646843)
#we can see by the plot there is variability across users as weel. Adding the bias of user to 
#the model imply in computing the difference between the average and "bi".
edx_train %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
head(user_avgs)

#Predicting rating including movie effects
user_hat <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
user_rmse<- RMSE(edx_test$rating, user_hat)
user_rmse


#Genres effect

#As we see on exploratory analysis, there is a variability across genres as weel. Adding the bias of genres
#to the model imply in computing the difference between the average and "bi" e "bu".
genres_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i- b_u))

head(genres_avgs)

#Predicting rating including movie and user effects
genres_hat <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by= "genres") %>%
  mutate(pred = mu + b_i + b_u +b_g) %>%
  pull(pred)
genres_rmse<- RMSE(edx_test$rating, genres_hat)
genres_rmse

#See if there is some improvement using release_year
year_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by="genres") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - mu - b_i- b_u - b_g))


#Predicting rating including movie and user effects
year_hat <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by= "genres") %>%
  left_join(year_avgs, by= "release_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)
year_rmse<- RMSE(edx_test$rating, year_hat)
year_rmse


#We see improvements in RMSE by adding movie, user and genres bias about 10%. Now Let’s explore where we 
#made mistakes in our first model, using only movie effects b_i. Here are the 10 largest mistakes:
edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (movie_hat)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

#Discovering the best 10 best and worst movies and the number or ratings they had.
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()
#the best
edx_train %>%
  count(movieId) %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>%
  slice(1:10) %>% 
  knitr::kable()

#the worst
edx_train %>%
  count(movieId) %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>%
  slice(1:10) %>% 
  knitr::kable()
#Regularization
#The supposed “best” and “worst” movies were rated by very few users, in most cases just 1. These movies 
#were mostly obscure ones. This is because with just a few users, we have more uncertainty. 
#Therefore, larger estimates of  b_i, negative or positive, are more likely.
#These are noisy estimates that we should not trust, especially when it comes to prediction. 
#Large errors can increase our RMSE, so we would rather be conservative when unsure.

#Regularization permits us to penalize large estimates that are formed using small sample sizes.
#when our sample size n_i is very large, a case which will give us a stable estimate, then the penalty λ
#is effectively ignored since n_i+λ ≈ n_i. However, when the n_i is small, then the estimate b_i(λ) 
#is shrunken towards 0. The larger λ, the more we shrink.

# Let's compute these regularized estimates of b_i using lambda=5. Then, look at the top 10 best 
#and worst movies now.
lambda <- 5
movie_reg_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#To see how the estimates shrink, we can  make a plot of the regularized estimates versus the least squares estimates.
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#Now, let’s look at the top 10 best and worst movies based on the penalized estimates ^b_i (λ):
#the best
edx_train %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n)
  slice(1:10) %>% 
  knitr::kable()
#the worst
edx_train %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>%
  knitr::kable()
#Do we improve our results?
#Results regularization modeling RMSE  (0.8642307)
reg_ratings_hat <- edx_test %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by= "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
reg_ratings_rmse<- RMSE(edx_test$rating, reg_ratings_hat)
reg_ratings_rmse


#Now note that lambda is a tuning parameter.
#We can use cross-validation to choose it.
#Now let's try another values for lambda, and penalize small sample sizes of users and genres as well.
#Regularized Movie + user + genres effect (0.8638148)

lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g<- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  
  b_y <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(release_year) %>%
    summarize(b_y = sum(rating - b_g - b_u - b_i - mu)/(n()+l))
  
    reg_movie_user_hat <- 
    edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by= "genres") %>%
    left_join(b_y, by= "release_year") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    pull(pred)
    
  return(RMSE(edx_test$rating,reg_movie_user_hat))
})  
qplot(lambdas, rmses)

lambda<- lambdas[which.min(rmses)]
lambda

reg_movie_user_rmses<- min(rmses)
reg_movie_user_rmses

#We can see a resume about our improvements on the following dataframe.
rmse_results <- tibble(method = c("Just the average","Movie Effect","User Effect", "Genres effect","Year effect","Regularized Movie Effect Model", "Regularized Movie + User + Genres + Year effect"),
                       RMSE= c(mean_rmse, movie_rmse, user_rmse, genres_rmse,year_rmse,reg_ratings_rmse, reg_movie_user_rmses))
print.data.frame(rmse_results)

####################################################

#Final prediction

#####################################################
#Now it's time to use the best model to predict ratings in the validation dataset.
#First we add "release_year" and "date" to obtain a dataset in the same structure than training set,
#Later we use the bias obtained from the training set for "movie, user, genre and release year.

validation <- validation %>% mutate(release_year = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),
                      title = str_remove(title, "[/(]\\d{4}[/)]$"),
                      date = as_datetime(timestamp))
l<-4.5
mu <- mean(edx_train$rating)

b_i <- edx_train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx_train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_g<- edx_train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
b_y <- edx_train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - b_g - b_u - b_i - mu)/(n()+l))


validation_hat <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by= "genres") %>%
  left_join(b_y, by= "release_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

#Evaluating the model in the validation dataset - Final RMSE (0,86469)
RMSE(validation$rating, validation_hat)

