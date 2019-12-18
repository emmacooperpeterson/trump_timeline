library(tidyverse)
library(lubridate)
library(twitteR)
library(rtweet)
library(gmailr)
library(glue)

# set up tokens
source("tokens.R")
setup_twitter_oauth(api_key, api_secret_key, access_token, access_secret_token)



#' Get a dataframe of users followed by Donald Trump
#'
#' @return dataframe
get_following <- function() {
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
#' @return tweet, dataframe (NULL if no tweet from today)
get_latest_tweet <- function(user_id) {
  
  tweet <- NULL
  
  recent_tweet <-
    get_timeline(user_id, n = 1) %>%
    select(created_at, status_id, screen_name)
  
  tweet_time <- 
    recent_tweet %>%
    pull(created_at)
  
  if (tweet_time >= today()) tweet <- recent_tweet

  return(tweet)
}



#' Retweet a tweet with a comment ("quote retweet")
#' H/T https://rayheberer.ai/archive/tweetprocessing/
#' 
#' @param username character, from `get_latest_tweet()`
#' @param tweet_id character, from `get_latest_tweet()`
#' @param comment character
retweet_with_comment <- function(username, tweet_id, comment) {
  url <- glue("https://twitter.com/{username}/status/{tweet_id}")
  updateStatus(glue("{comment} {url}"), bypassCharLimit = TRUE)
}



#' send myself a notification email when `main()` fails
send_failure_email <- function() {
  
  email <-
    gm_mime() %>%
    gm_to("emmacooperpeterson@gmail.com") %>%
    gm_from("trumptimeline1@gmail.com") %>%
    gm_subject("trumptimeline failed :(") %>%
    gm_text_body("nooooooo")
  
  gm_send_message(email)

}



#' Main function to run the whole process
#' from pulling Trump's current following list
#' to retweeting a recent tweet from an account on that list 
#'
#' @return 
main <- function() {
  
  # pull a list of accounts currently followed by trump
  following <- get_following()
  
  retweeted <- FALSE
  attempts <- 0
  
  # until we retweet a tweet from today, keep trying
  while (!retweeted) {
    attempts <- attempts + 1
    user_id <- choose_user(following)
    tweet <- get_latest_tweet(user_id)
    
    # if we found a tweet from today, retweet it
    if (!is.null(tweet)) {
      tweet_username <- tweet %>% pull(screen_name)
      tweet_id <- tweet %>% pull(status_id)
      retweet_with_comment(tweet_username, tweet_id, "This tweet was on Trump's timeline today.")
      retweeted <- TRUE
      
    } else if (attempts > 10) break # give up if we try more than 10 times
  }
  
  if (!retweeted) send_failure_email()
}







# run it ! ----------------------------------------------------------------
main()

