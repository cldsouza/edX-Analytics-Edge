
#Designed to access Twitter’s JSON API and supporting OAuth authentication 
#through its companion ROAuth package, twitteR makes searching Twitter as 
#simple as can be.

install.packages('twitteR', dependencies=T)
library(twitteR)
library(ROAuth)
require(RCurl)
library(plyr)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "KCpbE3kI4dd87tEvZwsY7kUAl"
consumerSecret <- "5X9Gr3BDFM5lqCsFlUoUmuPD3K6kbmRHDL0VLLG14y89DfnTqx"

twitCred <- getTwitterOAuth(consumer_key= consumerKey, consumer_secret=consumerSecret)
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()


spicejet.tweets = searchTwitter('@flyspicejet', n=1500)

length(delta.tweets)