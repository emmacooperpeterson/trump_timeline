library(tidyverse)
library(lubridate)
library(rtweet)
library(glue)



#' Get a dataframe of users followed by Donald Trump
#'
#' @return dataframe
get_following <- function() {
  # get a dataframe of users followed by Donald Trump
  following <- 
    get_friends("realDonaldTrump", n = 100) %>%
     mutate(index = row_number())
  return(following)
}


#' Randomly choose a user who is followed by Trump
#' 
#' @param following dataframe returned by `get_following()`
#' 
#' @return user_id, string
choose_user <- function(following) {
  random_number <- round(runif(1, 1, nrow(following)))
  user_id <- following %>% filter(index == random_number) %>% pull(user_id)
  return(user_id)
}


#' Get the most recent tweet from a given user.
#' If the most recent tweet is not from today, return NULL.
#' Since the bot will retweet once daily, this will prevent
#' multiple retweets of the same tweet
#' 
#' @param user_id character, returned by `choose_user()`
#' 
#' @return status_id -- NULL if no tweet from today
get_latest_tweet <- function(user_id) {
  
  tweet_id <- NULL
  
  tweet <-
    get_timeline(user_id, n = 1) %>%
    select(created_at, text, status_id)
  
  tweet_time <- 
    tweet %>%
    pull(created_at)
  
  print(tweet)
  
  if (tweet_time >= today()) tweet_id <- tweet %>% pull(status_id)

  return(tweet_id)
}




following <- get_following()
user_id <- choose_user(following)
username <- get_username(user_id)
tweet_id <- get_latest_tweet(user_id)

#post_tweet(retweet_id = tweet_id)

