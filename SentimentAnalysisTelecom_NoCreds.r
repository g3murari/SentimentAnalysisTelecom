## This is an R program to perform sentiment analysis for telecom project 
##	as part of PGPBABI program at Great Lakes Institute of Management.
## Author: Gayatri Sukumar Kanthimathi. Last Modified: Nov 5, 2014

## Step 1: Load all the required R packages. Ensure that all of these
## 	packages are installed on RStudio before running this script.
library(twitteR) # R based twitter client
library(Rfacebook) # R facebook client
library(Rook) # For facebook queries
library(httpuv) # For facebook queries
library(sentiment) # sentiment analysis package by Timothy Jurka avalilable at
					# http://cran.r-project.org/src/contrib/Archive/sentiment/
library(plyr) # Tools for splitting, applying and combining data
library(ggplot2) # Graphics
library(wordcloud) # Word clouds
library(RColorBrewer) # Colour palettes
library(RCurl) # Curl
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", 
			package = "RCurl")))

## Step 2: 
## a) Go to http://dev.twitter.com/apps and create a new application. Use 
##	the data from the new application below.
reqURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "*****FillKeyHere*****"
consumerSecret = "*****FillKeyHere*****"
twitterCred = OAuthFactory$new(consumerKey = consumerKey,
					consumerSecret = consumerSecret, requestURL = reqURL,
					accessURL = accessURL, authURL = authURL)
twitterCred$handshake() # All URLs are https. Need to enter PIN here...
##twitterCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", 
##		package = "RCurl"))
registerTwitterOAuth(twitterCred)

## b) Go to https://developers.facebook.com/apps and create a new application. 
##	Use the data from the new application below.
##fbOAuth = fbOAuth(app_id="*****FillKeyHere*****", 
##				app_secret="*****FillKeyHere*****")
## Ensure to perform the copy paste site URL from R to the facebook App page 
##	after the above step.
##save(fbOAuth, file="fboauth")
##load("fboauth")
## Let us go the temp token route as the facebook APIs have changed and it is 
## 	not advisable to use the facebook app anymore. The temp tokens are valid
## 	2 hours. Get it from https://developers.facebook.com/tools/explorer and
##	set API version to Unversioned. Copy the generated token here.
fbToken = "*****FillKeyHere*****"

## Step 2: 
## a) Now, let us collect the data from twitter for the month of October
##	2014. Let us use the same 5000 feeds as allocated by the social media
## 	analytics tool trial version.
OctTweets = searchTwitter("uninor", n = 5000, lang = "en", since = '2014-10-01', 
				until = '2014-10-31') ## 13.49 secs for 100 tweets
## let us do the geocode (lat, long, radius) later. 
##	eg: geocode='42.375,-71.1061111,10mi'
## Let us now extract the text out of the tweets. 'text' is the field name.
OctTweetsText = sapply(OctTweets, function(x) x$getText())

## b) Now, let us collect the data from facebook for the month of October
##	2014. Let us use the same 5000 feeds as allocated by the social media
## 	analytics tool trial version.
OctFB = searchFacebook(string = "uninor", token = fbToken, n = 5000, 
			since = URLencode("01 october 2014 00:00"), 
			until = URLencode("31 october 2014 23:59")) ## 0.7 s for 11 posts
## let us do the geocode (lat, long, radius) later. Since Uninor is only in 
##	India not used for this project
##	eg: geocode='42.375,-71.1061111,10mi'
## Let us now extract the text out of the tweets. 'text' is the field name.
OctFBText = OctFB$message


## Step 3: Let us process tweets and extract pure text for sentiment analysis
OctConditionedText = OctTweetsText ## Copy Oct Tweet Text and condition 
OctConditionedText = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", OctConditionedText)
	## Rmv reduntant retweets
OctConditionedText = gsub("@\\w+", "", OctConditionedText) ## Rmv @ people
OctConditionedText = gsub("[[:punct:]]", "", OctConditionedText) ## Rmv puncts
OctConditionedText = gsub("[[:digit:]]", "", OctConditionedText) ## Rmv any nums
OctConditionedText = gsub("http\\w+", "", OctConditionedText) ## Rmv any links
OctConditionedText = gsub("[ \t]{2,}", "", OctConditionedText) ## Rmv spaces
OctConditionedText = gsub("^\\s+|\\s+$", "", OctConditionedText) ## Rmv spaces
OctFBConditionedText = OctFBText
OctFBConditionedText = gsub("[[:punct:]]", "", OctFBConditionedText) ## Rmv puncts
OctFBConditionedText = gsub("[[:digit:]]", "", OctFBConditionedText) ## Rmv any nums
OctFBConditionedText = gsub("http\\w+", "", OctFBConditionedText) ## Rmv any links
OctFBConditionedText = gsub("[ \t]{2,}", "", OctFBConditionedText) ## Rmv spaces
OctFBConditionedText = gsub("^\\s+|\\s+$", "", OctFBConditionedText) ## Rmv spaces


## Step 4: Let us convert all text to lower case, but we shall be very careful 
##	about it so as to handle error...
## Define the function to convert to lower case with error handling...
try.error = function(x)
{
   oriText = NA ## initialize to NA
   ## check if error exists when converting to lower case
   try_error = tryCatch(tolower(x), error=function(e) e) 
   ## if no error exists, then convert to lower case and assign to oriText
   if (!inherits(try_error, "error")) oriText = tolower(x)
   return(oriText)
}
OctConditionedText = sapply(OctConditionedText, try.error)
OctFBConditionedText = sapply(OctFBConditionedText, try.error)

## Step 5: Remove NA tweets/FBcomments...
OctConditionedText = OctConditionedText[!is.na(OctConditionedText)]
names(OctConditionedText) = NULL
OctFBConditionedText = OctFBConditionedText[!is.na(OctFBConditionedText)]
names(OctFBConditionedText) = NULL

## Step 6: 
## a) Store the number of mentions in tweets and facebook
nTweets = length (OctConditionedText)
nFBMentions = length (OctFBConditionedText)
totalMentions = nTweets + nFBMentions
## b) Combine the texts
allMentions = c (OctConditionedText, OctFBConditionedText)

## Step 7: Let us now use the sentiment package and perform sentiment analysis
##	classify emotion
classEmo = classify_emotion(allMentions, algorithm="bayes", prior=1.0)
## The best fit emotion is available in the 7th column. Get that and replace
##	all NAs by unknown
emotion = classEmo[,7]
emotion[is.na(emotion)] = "unknown"
# classify polarity
classPolarity = classify_polarity(allMentions, algorithm="bayes")
# get polarity best fit
## The best fit emotion is available in the 4th column. Get that and replace
polarity = classPolarity[,4]

## Step 8: Create DF with results and write to a csv file.
mentionsDF = data.frame(text = allMentions, emotion = emotion, 
			polarity = polarity, stringsAsFactors = FALSE)
write.csv(mentionsDF, file="./TelecomMentions.csv")

## Step 9: Last but not least, let us generate some graphics
## 	Pie chart for tweet / FB percentage
pieDF = data.frame(varNames = c("tweets", "facebook mentions"),
			vals = c(nTweets, nFBMentions))
ggplot(pieDF, aes(x = "", y = vals, fill = varNames)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Dark2") +
  coord_polar("y", start = pi / 3) +
  labs(title = "mention spread")
  
##	Emotions Graph...
ggplot(mentionsDF, aes(x=emotion)) + 
	geom_bar(aes(y=..count.., fill=emotion)) +
	scale_fill_brewer(palette="Dark2") + 
	labs(x="emotion categories", y="number of mentions") +
	ggtitle("Sentiment Analysis of Mentions about Telecom (by emotion)")
	
# Polarity Graph...
ggplot(mentionsDF, aes(x=polarity)) +
	geom_bar(aes(y=..count.., fill=polarity)) +
	scale_fill_brewer(palette="RdGy") +
	labs(x="polarity categories", y="number of mentions") +
	ggtitle("Sentiment Analysis of Mentions about Telecom (by polarity)")

# separating text by emotion
poles = levels(factor(mentionsDF$polarity))
npoles = length(poles)
pol.docs = rep("", npoles)
for (i in 1:npoles)
{
   tmp = mentionsDF[polarity == poles[i], ]
   pol.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
pol.docs = removeWords(pol.docs, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(pol.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = poles

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(npoles, "Dark2"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)
