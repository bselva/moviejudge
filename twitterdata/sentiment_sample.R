#init
library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(RColorBrewer) 

#provide access info to connect
setwd("./")
key=""
secret=""
access_token =	""
access_key = ""

authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret)
save(authenticate, file="twitter authentication.Rdata")

sentiment_scores = function(sentences, pos.words, neg.words, .progress='none')
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
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos = readLines("dictionary/positive-words.txt")
neg = readLines("dictionary/negative-words.txt")

N=10000  # tweets to request from each query

tweets=searchTwitter('#Zootopia', lang="en", N, since='2016-04-01', until = '2016-04-20', resultType="recent")

Tweets.text = laply(tweets,function(t)t$getText())
Tweets.text <- iconv(Tweets.text, 'UTF-8', 'ASCII')

analysis = sentiment_scores(Tweets.text, pos, neg)

table(analysis$score[analysis$score!=0])

hist(analysis$score[analysis$score!=0],xlab=" ",main="Sentiment of sample tweets",col="skyblue", border="black")

mean(analysis$score[analysis$score!=0])

m <- cbind(Tweets.text, analysis$score)
write.table(m,"Zootopia_text.txt")

