# Solution Exercise 2
# Author: Lionel Fillatre

#=======================================================================
# Call the function to connect to Twitter
#=======================================================================
connectTwitter <- function() {
  
  library(twitteR)
  library(tm)
  library(wordcloud)
  library(RColorBrewer)
  
  #-----------------------------------------------------------------------
  # Connexion to Twitter
  #-----------------------------------------------------------------------
  
  api_key <- ""
  api_secret <- ""
  access_token <- ""
  access_token_secret <- ""
  
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) # authentification
  
}

#=======================================================================
# Call the function to create a cloud of words based on Tweets
#=======================================================================
cloud_twitter <- function(processedWord, nbTweets = 100, maxWords = 20) {

  #-----------------------------------------------------------------------
  # Processing the tweets and clean up
  #-----------------------------------------------------------------------
  tweets <-searchTwitter(processedWord,n=nbTweets) # retrieve tweets
  
  #Extract text content of all the tweets
  tweetTxt = sapply(tweets, function(x) x$getText())
  
  # Convert text in latin1 into ASCII. If non-convertible bytes in the input, replace by sub.
  #If not NA it is used to replace any non-convertible bytes in the input.
  tweetTxt <- sapply(tweetTxt,function(row) iconv(row, from="latin1", to="ASCII", sub=""))
  
  #In tm package, the documents are managed by a structure called Corpus
  # A vector source interprets each element of the vector tweetTxt as a document. 
  myCorpus <- Corpus(VectorSource(tweetTxt))
  
  #Create a term-document matrix from a corpus
  # remove terms in stopwords
  tdm <- TermDocumentMatrix(myCorpus,control = list(removePunctuation = TRUE,
                                                    stopwords = c(stopwords("english")), 
                                                    removeNumbers = TRUE, tolower = TRUE))
  
  #Convert as matrix
  m = as.matrix(tdm)
  
  #-----------------------------------------------------------------------
  # Plot the cloud of words
  #-----------------------------------------------------------------------
  
  #Get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing=TRUE) 
  
  #Create data frame with words and their frequencies
  dm = data.frame(word=names(word_freqs), freq=word_freqs)

  #Plot wordcloud
  #rot.per: percentage of vertical words
  wordcloud(dm$word, dm$freq, c(4,.6), random.order=FALSE, max.words=maxWords, rot.per=.25, colors=brewer.pal(8, "Dark2"))

}