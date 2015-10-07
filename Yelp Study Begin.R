##Capstone Project Start
##suppressMessages(require(rjson))
##suppressMessages(require(tm))
require(jsonlite)


##RJSON STYLE
##json_file <- "C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"
##json_data <- fromJSON(json_file)

##jsonlite style

business_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")
checkin_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json")
review_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json")
tip_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tip.json")
user_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json")
##According to StackOverflow
##http://stackoverflow.com/questions/26519455/error-parsing-json-file-with-the-jsonlite-package?_sm_au_=iVVWtcSDTcntsVDP

checkins <- fromJSON(sprintf("[%s]", paste(readLines(checkin_file), collapse=",")))
businesses <- fromJSON(sprintf("[%s]", paste(readLines(business_file), collapse=",")))
reviews <- fromJSON(sprintf("[%s]", paste(readLines(review_file), collapse=",")))
tips <- fromJSON(sprintf("[%s]", paste(readLines(tip_file), collapse=",")))
users <- fromJSON(sprintf("[%s]", paste(readLines(user_file), collapse=",")))

head(checkins)

##Unlist Reviews

##df <- data.frame(matrix(unlist(reviews),nrow=150, byrow=T),stringsAsFactors = F)
##names(df) <- names(reviews[[1]])

##Ideas

##When do people play basketball somewhere???
##Which doctors get best reviews?

##Get doctors only
##Categories is list, so we are using GREP, not which

doctors <- grep("Doctor",businesses$categories)
docsData <- businesses[doctors,]

docReviews <- reviews[reviews$business_id %in% docsData$business_id,]

stryker <- grep("chairs", docReviews$text)
strykerInfo <- docReviews[stryker,]

fivers <- subset(reviews, stars ==5)

class(reviews$stars)

hist(docsData$stars)

##Doc Reviews Word Cloud
##https://deltadna.com/blog/text-mining-in-r-for-term-frequency/

library('wordcloud')
review_text <- paste(docReviews$text, collapse=" ")
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

##Last User Question

##More than 1 fan

users$someFans <- sapply(users$fans,function(a) ifelse(a>1,1,0))
users$someFunnies <- sapply(users$votes$funny, function(a) ifelse(a>1,1,0))

mytable <- table(users$someFans,users$someFunnies)
fisher.test(mytable)

##Does flu shot affect pharmacy reviews?

pharms <- grep("Pharm|Drugs",businesses$categories)
pharmData <- businesses[pharms,]

pharmReviews <- reviews[reviews$business_id %in% pharmData$business_id, ]

pharmReviews$fluShot <- grepl("[Ff]lu [Ss]hot",pharmReviews$text)
pharmReviews$fluShot <- sapply(pharmReviews$fluShot, function(a) ifelse(a,"Flu Shot","No Flu Shot"))
boxplot(stars~fluShot,data=pharmReviews)
t.test(stars~fluShot,data=pharmReviews,alternative="greater")

##Basketball Search

parks <- grep("Park|Playgrou",businesses$categories)
parkData <- businesses[parks,]

basketballTip <- grep("[Bb]asketball|[Bb][Bb]all|[Hh]oop|[Bb]allin",tips$text)
bballTips <- tips[basketballTip,]

parkBallTips <- bballTips[bballTips$business_id %in% parkData$business_id,]

parkBallTips$text
##What words are predictors of good/bad ratings for docotors (or anything) - chairs/germy/drugs/prescription?
##