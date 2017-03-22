# required packages
library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tm)
library(RColorBrewer)
library(wordcloud)

api_key <- "xxxxxxxxxxxxxxxxxxx"
api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# harvest some tweets
some_tweets = searchTwitter("RedIphone", n=1500, lang="en")

# get the text
feed_rediphone <- laply(some_tweets, function(t) t$getText())

# 1. Start the Sentiment Analysis
# Read in dictionary of positive and negative workds
good_text <- scan('positive-words.txt',
            what='character', comment.char=';')
bad_text <- scan('negative-words.txt',
            what='character', comment.char=';')

score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

rediphone <- score.sentiment(feed_rediphone, good_text, bad_text, .progress='text')
rediphone$name <- 'rediphone'

plotdat <- rediphone[c("name", "score")]

# Remove anything less than -3 or greater than 3
plotdat <- plotdat[!plotdat$score > 3, ]
plotdat <- plotdat[!plotdat$score < (-3), ]

qplot(factor(score), data=plotdat, geom="bar", 
      fill=factor(name),
      xlab = "Sentiment Score")

# 2. Now, the word cloud
tweets.text = sapply(some_tweets, function(x) x$getText())



# Replace blank space ("rt")
tweets.text <- gsub("rt", "", tweets.text)

# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)

# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)

# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)

# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)

# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)

# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)

#convert all text to lower case
tweets.text <- tolower(tweets.text)

#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))

#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generate wordcloud
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
