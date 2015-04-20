rm(list=ls()) # Clear the previously used libraries

# Load the required R libraries
library(twitteR)
library(ROAuth)
library(RCurl)

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"

# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"

# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "KCpbE3kI4dd87tEvZwsY7kUAl"
consumerSecret <- "5X9Gr3BDFM5lqCsFlUoUmuPD3K6kbmRHDL0VLLG14y89DfnTqx"
accessToken <- "67876855-5Lkg2s2PNcpimqDh2yMGxj3W2FgpyZ5Db2LG4W1Je"
accessSecret <- "UXfwPzBGda6WeDG0y738AO2Jw1ZlpPdc2AjKsHZx6bEf7"

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)
# Asking for access
twitCred$handshake(cainfo="cacert.pem")

#registerTwitterOAuth(twitCred)

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

delta.tweets = searchTwitter('@delta', n=1500)

	