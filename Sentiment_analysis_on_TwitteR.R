setwd("D:/R Docs/ML code/Kaggle")

#install required packages
library(twitteR)
library(purrr)
library(plyr)
library(dplyr)
library(stringr)
require('ROAuth')
require('RCurl')
require('twitteR')

#defining sentiment score function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  #putting score along with text in a dataframe
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#scanning 'positive-words' and 'negative-words' text files
pos.words = scan('Sentiment on Twitter/positive-words.txt', what='character', comment.char=';')
neg.words = scan('Sentiment on Twitter/negative-words.txt', what='character', comment.char=';')

#first create Twitter app from "https://apps.twitter.com/app/new" and generate following details

consumer_key <-"paste consumer key here"
consumer_secret <- "paste consumer secret here"
access_token<-"paste access token here"
access_secret <- "paste access token secret here"

reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'
  
cred <- OAuthFactory$new( consumerKey= consumer_key, 
                          consumerSecret= consumer_secret,
                          requestURL= reqURL,
                          accessURL= accessURL,
                          authURL= authURL)

cred$handshake()

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret )

#extracting 1000 tweets about '@HillaryClinton' and  '@realDonaldTrump' 
tweets1 = searchTwitter('@HillaryClinton', n=1000)
tweets1.df <- tbl_df(map_df(tweets1, as.data.frame))

tweets2 = searchTwitter('@realDonaldTrump', n=1000)
tweets2.df <- tbl_df(map_df(tweets2, as.data.frame))

tweets1.scores = score.sentiment(tweets1.df$text, pos.words, neg.words, .progress='text')
tweets2.scores = score.sentiment(tweets2.df$text, pos.words, neg.words, .progress='text')

#total score for both
hillary_score <- sum(tweets1.scores$score)
trump_score <- sum(tweets2.scores$score)

#plotting histogram of scores
hist(tweets1.scores$score, xlab = "Sentiment Score of tweets1")
hist(tweets2.scores$score, xlab = "Sentiment Score of tweets2")

  